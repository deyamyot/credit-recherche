#set working directory
setwd("D:/Audrey_Anne/Godbout_analyse/credit de recherche/gitcredit_recherche")

#######################################
#importation data

donnees_base<-read.table("data/donnees_deploiment.csv", header=T,sep=";", dec=".")[-c(33:39),-c(14:15)]
mobile<-read.table("data/MaxN_mobile.csv", header=T,sep=";",dec=".")[-33,]
peumobile<-read.table("data/decompte_peumobile.csv", header=T, sep=";", dec=".")[,-16]
recouvrement<-read.table("data/pourcentage_recouvrement.csv", header=T, sep=";",dec=".")[-147,]

colnames(donnees_base)[5]<-"coordonnee_y"


#physico
#instalation package
#install.packages("sdmpredictors")
#install.packages("leaflet")
#install.packages("raster")

#lecture library
library(sdmpredictors)
library(leaflet)
library(raster)


#list affichant les donnees disponible de bio-oracle
#identifier le nom des couche que lon souhaite utiliser
layers_physico<-list_layers(dataset="Bio-ORACLE")

#====================================================
#BO2_salinitymean_bdmin
#Salinity indicates the dissolved salt content in the ocean.
#https://bio-oracle.org/data/1.0/BO_salinity.zip

#BO2_tempmean_bdmin
#Sea surface temperature is the temperature of the water at the ocean 
#surface. This parameter indicates the temperature of the topmost meter 
#of the ocean water column.
#https://bio-oracle.org/data/1.0/BO_sstmean.zip

#source :
#Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F, De Clerck O (2012) Bio-ORACLE: A global environmental dataset for marine species distribution modelling. Global Ecology and Biogeography, 21, 272–281.
#[access publication]   [supporting information]

#Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H., Serrão, E. A., & De Clerck, O. (2017). Bio-ORACLE v2.0: Extending marine data layers for bioclimatic modelling. Global Ecology and Biogeography.
#[access publication]   [supporting information]
#===========================================================

#download raster des physico desirer de biooracle
#DOIT ETRE CONNECTER A INTERNET POUR ETRE EN MESURE DE FAIRE LEXTRACTION DES COUCHES
#equalarea = projection lonlat
#rasterstack= les different raster afficher comme un ensemble de couche et non comme une liste
temp<-load_layers(layercodes="BO2_tempmean_bdmin", equalarea=FALSE, rasterstack=TRUE) # moy de 2000 a 2014, en celsius
sal<-load_layers(layercodes="BO2_salinitymean_bdmin",equalarea=FALSE, rasterstack=TRUE)# moy de de 2000 a 2014, en PSS (practical salinity scale)
#data.frame selon les coordonnees des sites desirer
my.sites <- data.frame(Id=donnees_base$ID, Lat=donnees_base$coordonnee_y, Lon=donnees_base$coordonnee_x)

#extraction donnees pour chacun des points
my.sites.physico<- data.frame(temperature=extract(temp,my.sites[,2:3]), salinite=extract(sal,my.sites[,2:3]))
#prendre en consideration que se sont des temperature moyenne annuelle et donc il est normale-je pense-quelles soient negative 
 
#ajout des donnees dans le document de donnees_bas
donnees_complet<-cbind(donnees_base, my.sites.physico)
colnames(donnees_complet)[14] <- "temp_moy" 
colnames(donnees_complet)[15] <- "sal_moy" 

#division en trois grande zone dechantillonnage selon leur coordonnees qui correspondent aussi au date 
for (i in 1:nrow(donnees_complet)){
  if (donnees_complet$date[i]=="27/07/2021"){
    donnees_complet$zone[i]<-"1"
   }
 else if (donnees_complet$date[i]=="28/07/2021"){
  donnees_complet$zone[i]<-"2"
 }
  else if (donnees_complet$date[i]=="29/07/2021"){
    donnees_complet$zone[i]<-"3"
  }
  else {
    donnees_complet$zone[i]<-"4"
  }
}

#mapping physico----------
# Crop raster vis-a-vis le st-laurent
#st_laurent_ex <- extent(-100, 45, 30.75, 72.5)
#temp_moy_min_crop <- crop(temp, st_laurent_ext)

# Generate a nice color ramp and plot the map
#my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
#plot(temp_moy_min_crop,col=my.colors(1000),axes=FALSE, box=FALSE)
#title(cex.sub = 1.25, sub = "Température moyenne en milieux côtier (ºC)")

#####################################
####Manipulation des donnees 
####################################

#calcul diversite---------
#espece mobile
mobile$nb_espece<-rowSums(mobile[,2:18]>0)
#espece peumobile
peumobile$nb_espece<-rowSums(peumobile[,7:15]>0)
#espece nonmobile
recouvrement$nb_espece<-rowSums(recouvrement[,8:12]>0)


#totale par espece pour une la ligne de cage
#tot<-(apply(mobile[,c(2:18)], 2, sum))


#pourcentage recouvrement-----------
#correction nb croissement tot=54
#cadre trop grand = enlever une ranger supplementaire pour avoir le mm nb de croissement que les autres
recouvrement[90,11]<-52
recouvrement[111,9]<-43
recouvrement[111,11]<-11

#nb de point evaluer en fonction des point non-visible
recouvrement$nb_point_ev<-recouvrement$nb_points_tot - recouvrement$nb_points_nonvisible
#nb de point pour lequel le recouvrement negale pas a une des categorie biotique
recouvrement$non_biotique<-recouvrement$nb_point_ev - rowSums(recouvrement[,8:12])


#le probleme est que pour certaine colone la valeur est plus elever que celle du nb de point ev 
#il faudrait donc le prendre en consideration au prealable
#aussi puisque l'on fait apelle a la colonne complet dans le calcul, on a pas besoin de la boucle
#recouvrement biotique
for( i in 1:nrow(recouvrement)){
  recouvrement$p_algue_r<-(recouvrement[,8]/recouvrement[,19])*100
  recouvrement$p_algue_b<-(recouvrement[,9]/recouvrement[,19])*100
  recouvrement$p_algue_v<-(recouvrement[,10]/recouvrement[,19])*100
  recouvrement$p_coralline<-(recouvrement[,11]/recouvrement[,19])*100
  recouvrement$p_coquillage<-(recouvrement[,12]/recouvrement[,19])*100
  recouvrement$p_non_biotique<-(recouvrement[,20]/recouvrement[,19])*100
  i=i+1
}

#recouvrement granulo
for ( i in 1:nrow(recouvrement)){
  recouvrement$p_sable<-((recouvrement[,13]/recouvrement[,19])*100)
  recouvrement$p_gravier<-((recouvrement[,14]/recouvrement[,19])*100)
  recouvrement$p_caillou<-((recouvrement[,15]/recouvrement[,19])*100)
  recouvrement$p_bloc<-((recouvrement[,16]/recouvrement[,19])*100)
  recouvrement$p_rochemere<-((recouvrement[,17]/recouvrement[,19])*100)
  i=i+1
}

####Table SQL
#install.packages('RSQLite')
library(RSQLite)

connexion <- dbConnect(SQLite(), dbname="D:/Audrey_Anne/Godbout_analyse/credit de recherche/gitcredit_recherche/lien.db")

#utiliser pour vider la table si problematique
dbSendQuery(connexion,"DROP TABLE mobile;") 

mobile_sql <- '
CREATE TABLE mobile (
  ID INTEGER(4),
  crabe_com INTEGER(4),
  crabe_a INTEGER(4),
  crabe_n INTEGER(4),
  bernard INTEGER(4),
  homard INTEGER(4), 
  buccin INTEGER(4),
  etoile_com INTEGER(4),
  etoile_p INTEGER(4),
  etoile_t INTEGER(4),
  amphipode INTEGER(4),
  ctenophore INTEGER(4),
  cyclopteridae INTEGER(4),
  capelan INTEGER(4),
  maquereau INTEGER(4),
  chaboisseau INTEGER(4),
  morue INTEGER(4),
  lacon INTEGER(4),
  nb_espece INTEGER(4),
    PRIMARY KEY (ID)
);'

dbSendQuery(connexion, mobile_sql)
dbListTables(connexion)

#utiliser pour vider la table si problematique
dbSendQuery(connexion,"DROP TABLE peumobile;") 

peumobile_sql <- '
CREATE TABLE peumobile (
  ID INTEGER(4),
  temps INTEGER(4),
  nb_image INTEGER(4),
  num_image CHAR(50),
  nb_points_tot INTEGER(4),
  nb_points_nonvisible INTEGER(4),
  oursin INTEGER(4), 
  dollars_sable INTEGER(4),
  anemone_p INTEGER(4),
  anemone_r INTEGER(4),
  anemone_pb INTEGER (4),
  concombre_v INTEGER(4),
  concombre_r INTEGER(4),
  ver_noire INTEGER(4),
  coraux_mou INTEGER(4),
  nb_espece INTEGER(4),
    PRIMARY KEY (ID,temps,nb_image),
    FOREIGN KEY (temps) REFERENCES recouvrement(temps),
    FOREIGN KEY (nb_image) REFERENCES recouvrement(num_image)
    
);'

dbSendQuery(connexion, peumobile_sql)
dbListTables(connexion)


#utiliser pour vider la table si problematique
dbSendQuery(connexion,"DROP TABLE recouvrement;") 

recouvrement_sql <- '
CREATE TABLE recouvrement (
  ID INTEGER(4),
  temps INTEGER(4),
  num_image INTERGER(4),
  type_cam VACHAR (1),
  nom_image CHAR (50),
  nb_points_tot INTEGER(4),
  nb_points_nonvisible INTEGER(4), 
  algue_r INTEGER(4),
  algue_v INTEGER(4),
  algue_b INTEGER(4),
  coralline INTEGER(4),
  coquillage INTEGER(4),
  sable INTEGER(4),
  gravier INTEGER(4),
  caillou INTEGER(4),
  bloc INTEGER(4),
  rochemere INTEGER(4),
  nb_point_ev INTEGER(4),
  p_sable REAL(8),
  p_gravier REAL(8),
  p_caillou REAL(8),
  p_bloc REAL(8),
  p_rochemere REAL(8),
  non_biotique INTEGER(4),
  p_algue_r REAL(8),
  p_algue_v REAL(8),
  p_algue_b REAL(8),
  p_coralline REAL(8),
  p_coquillage REAL(8),
  p_non_biotique REAL(8),
  nb_espece INTERGER(4),
    PRIMARY KEY (ID, temps,num_image),
    FOREIGN KEY (temps) REFERENCES peumonile(temps),
    FOREIGN KEY (num_image) REFERENCES peumobile(nb_image)
);'

dbSendQuery(connexion, recouvrement_sql)
dbListTables(connexion)

#utiliser pour vider la table si problematique
dbSendQuery(connexion,"DROP TABLE donnees_complet;") 

donnees_complet_sql <- '
CREATE TABLE donnees_complet (
  ID INTEGER(4),
  num_cam REAL(8),
  date DATE(4),
  coordonnee_x REAL(8),
  coordonnee_y REAL(8),
  WPT INTEGER(4), 
  temps_depart TIME(6),
  nb_images INTEGER(4),
  delais_sec INTEGER(4),
  temps_analyser_min INTEGER(4),
  nb_images_analyser INTEGER(4),
  baited VACHAR(50),
  profondeur_m REAL(8),
  temp_moy REAL(8),
  sal_moy REAL(8),
  zone INTEGER (4)
    PRIMARY KEY (ID)
);'

dbSendQuery(connexion, donnees_complet_sql)
dbListTables(connexion)

#injection des donnees
dbWriteTable(connexion, append = TRUE, name= "mobile", value= mobile, row.names = FALSE)
dbWriteTable(connexion, append = TRUE, name = "peumobile", value = peumobile, row.names = FALSE)
dbWriteTable(connexion, append = TRUE, name = "recouvrement", value = recouvrement, row.names = FALSE)
dbWriteTable(connexion, append = TRUE, name = "donnees_complet", value = donnees_complet, row.names = FALSE)


##############################################
##### Manipulation donnees et analyse gradient
#############################################

#on souhaite mettre la variable reponse = nb_esp en fonction des variables explicatives
#(coordonnees,temp,sal,%recouvrment type de susbtrat)
#et voir par la suite si on observe une difference dans les especes presentes

#---------------------------
#1-garder la valeur la plus elever pour le recouvrement du nb espece nonmobile
#extraction de la donnee avec SQL
#les donnes sont alors dans des tables reelles
max_valeur_rec_sql<-"
SELECT ID, max(nb_espece) AS nb_espece
FROM recouvrement 
GROUP BY ID
;"
nb_esp_rec<-dbGetQuery(connexion, max_valeur_rec_sql)
head(nb_esp_rec)

#mm chose pour peumobile
max_valeur_pm_sql<-"
SELECT ID, max(nb_espece) AS nb_espece
FROM peumobile
GROUP BY ID
;"
nb_esp_pm<-dbGetQuery(connexion, max_valeur_pm_sql)
head(nb_esp_pm)

#------------------------------------
#2-rassembler les donnees pour voir le facteur du gradient
#faire la somme des valeurs pour avoir un nb despece total par points
gradient<-cbind(mobile$ID,mobile$nb_esp,nb_esp_rec$nb_esp,nb_esp_pm$nb_esp,donnees_complet$coordonnee_x,donnees_complet$coordonnee_y,donnees_complet$temp_moy,donnees_complet$sal_moy)
colnames(gradient)<-c("ID","esp_mobile","esp_rec","esp_peumobile","coordonne_x","coordonnne_y","temp_moy","sal_moy")
esp_tot<-rowSums(gradient[,c(2:4)]) # matrice implique quon ne peut pas utiliser le $ pour referer a une colone et quon doit dabord creer un vecteur pour ensuite lajouter a la matrice
gradient<-cbind(gradient,esp_tot)
summary(gradient)

#------------------------------------
#3-assomption manova
#install.packages("micompr")
#install.packages("MVN")
#install.packages("biotools")
#install.packages("rstatix")


library(micompr)
library(MVN)
library(biotools)
library(rstatix)


#taille adequate de lechantillon = oui jai plus e lignes que de colonnes (9 X32)
#independance des info = oui
#absences de donnes aberrantes univarie et multivarie
d_gradient<-as.data.frame(gradient)
#PK NE RECONNAIT PAS COMME UN DATA FRAME POUR LOPPERATION ALORS QUE IS.DATA.FRAME IS TRUE !!!!!!!!!!!!!!!!!!!!!
identify_outliers(d_gradient[,7]) # univarie
identify_outliers(gradient[,8])
mahalanobis_distance()# multivarie
#normalite a plusieurs variable 
mshapiro.test()
#absence de multicolinairite
#linearite
#homogeneite des variances = test de Levenne
#homogeneite des matrice de variance-covariance= test boxM
am<-assumptions_manova(gradient[,7:8],gradient[,1])

#------------------------------------
#4-analyse multivariees impact du gradient
#ANOVA multivarie-------------

mod1<-manova(cbind(temp_moy, sal_moy) ~ ID, data = d_gradient)
summary.aov(mod1)

#PCA-------------------
pca_mod1<-prcomp((d_gradient[,c(1,7,8)]), center=TRUE,scale=TRUE)
summary(pca_mod1)
#proportion variance = correspond *100 au pourcentage que PC1 explique de la variance totale

str(pca_mod1)#permet de voir les objets de la PCA
#The center point ($center), scaling ($scale), standard deviation(sdev) of each principal component
#The relationship (correlation or anticorrelation, etc) between the initial variables and the principal components ($rotation)
#The values of each sample in terms of the principal components ($x)

#imager PCA-------------
##installation packages necessaire------
#install.packages('ggplot2')
library(ggplot2)

#install.packages("devtools", repo="http://cran.us.r-project.org")
library(devtools)
#code pour tenter une erreur de lecture de la library
#remove.packages("cli")
#install.packages("cli")
#mise a jour des packages (tools)
#ouverture du Rstudio en tant qu'administrateur (clic droit sur l'icone)
#remove.packages("rlang")
#install.packages("rlang")
#remove.packages("devtools")
#install.packages("devtools", dependencies = TRUE)
library(devtools)

#install_github("vqv/ggbiplot", force=TRUE)
library(ggbiplot)

##plot PCA-------------
#install.packages("yarrr")
library("yarrr")#pour avoir acces a plus de couleurs entre autre

dev.off()#effacer figure deja ouverte
ggbiplot(pca_mod1,labels=rownames(d_gradient[,c(1,7,8)]), obs.scale=1, var.scale=1, ellipse=TRUE, groups=donnees_complet$zone)+
  scale_colour_manual(name= "pony",values=c("blue","purple2","pink","orange"))
#le label nous permet de situer chacune des stations


#-----------------------------------
#4-Ajouter la diversite comme variable Y supplementaire
mod2<-manova(cbind(temp_moy, sal_moy) ~ cbind(ID,esp_tot), data = d_gradient)
summary.aov(mod2)

pca_mod2<-prcomp((d_gradient[,c(1,7:9)]), center=TRUE,scale=TRUE)
summary(pca_mod2)

str(pca_mod2)

dev.off()#effacer figure deja ouverte
ggbiplot(pca_mod2,labels=rownames(d_gradient[,c(1,7:9)]), obs.scale=1, var.scale=1, ellipse=TRUE, groups=donnees_complet$zone)+
  scale_colour_manual(name= "pony",values=c("blue","purple2","pink","orange"))
#-----------------------------------
#5-Possibilite quil y est une interaction entre la temperature et la salinite
mod3<-manova(cbind(temp_moy, sal_moy,temp_moy*sal_moy) ~ cbind(ID,esp_tot), data = d_gradient)
summary.aov(mod3)
#devrait potentiellement faire une lm pour interpreter la relation qui est positive
#COMMENT FAIRE UNE PCA POUR AVEC UNE RELATION INTERVARIABLE X ??????????

#----------------------------------------
#6-test avec seulement l autre variable Y
mod4<-manova(cbind(temp_moy, sal_moy) ~ esp_tot, data = d_gradient)
summary.aov(mod2)

pca_mod4<-prcomp((d_gradient[,c(7:9)]), center=TRUE,scale=TRUE)
summary(pca_mod2)

str(pca_mod2)

dev.off()#effacer figure deja ouverte
ggbiplot(pca_mod4,labels=rownames(d_gradient[,c(7:9)]), obs.scale=1, var.scale=1, ellipse=TRUE, groups=donnees_complet$zone)+
  scale_colour_manual(name= "pony",values=c("blue","purple2","pink","orange"))
###############################################
#### Imager abondance espece par station
##############################################
#est-ce que je devrais faire une carte qui represente pour chacune des especes elles sont presente a quel point dechantillonnage genre un enorme graphique de barplot
#ou des graohique individuel de presence/absence et abondance en fonction des different point ce qui me donnerais une idee de la distribution => scatterplot

#scatterplot pour chacune des especes----------------------------
#certaine especes ont ete prise en compte en terme de recouvrement donc comment faire la mm chose ?

#1-extraire les bonne donnes de la table peumobile=> valeurs max
max_pm_sql<-"
SELECT ID, max(oursin) AS oursin, max(dollars_sable) AS dollars_sable, max(anemone_p) AS anemone_p, max(anemone_r) AS anemone_r, max(anemone_pb) AS anemone_pb, max(concombre_v) AS concombre_v, max(concombre_r) AS concombre_r, max(ver_noire) AS ver_noire,max(coraux_mou) AS coraux_mou
FROM peumobile
GROUP BY ID
;"
max_pm<-dbGetQuery(connexion, max_pm_sql)
head(max_pm)

#2-faire une table qui rassemble toutes les donnnes
ab_esp<-cbind(max_pm,mobile[,2:18],donnees_complet$zone)
colnames(ab_esp)[28]<-"zone"


#3-boucle pour faire tous les scatterplot en mm temps
#marge entre les graphique sur la figure
dev.off()#pour effecer la figure preceddament ouverte
par(mar=c(4,4,5,2))#x,y,haut,bas
par(mfrow=c(5,6))#combien de graphique par combien

#VERIFIER SI IL UTILISE LES BONNES COLONNES ET COMMENT DONNER LE TITRE DES COLONNES SANS TOUTES LES ECRIRES A LA MAIN
for (i in 2:ncol(ab_esp)){
  plot(x=ab_esp$ID,y=ab_esp[,i], 
       xlab="ID", ylab="MaxN",main=(colnames(donnees_complet)[,i]))
}

###############################################
#### Manipulation donnees et analyse type sol
###############################################

#on veut mettre en relation la diversite des especes et mm labondance avec le type de sol
#potentiellement faire un graphique de reseau entre les types de sol et les differentes especes 
#pour etre en mesure de mettre en relation tous ca => utiliser les pourcentages 


#Analyse multivaree--------------------
#1-moyenne valeur de pourcentage de recouvrement---------------
##VERIFIER POURCENTAGE SABLE !!!!!!!!!!!!!!
moy_p_rec_sql<-"
SELECT ID, avg(p_sable) AS pmoy_sable, avg(p_gravier) AS pmoy_gravier, avg(p_caillou) AS pmoy_caillou, avg(p_bloc) AS pmoy_bloc, avg(p_rochemere) AS pmoy_rochemere
FROM recouvrement 
GROUP BY ID
;"
pmoy_rec<-dbGetQuery(connexion, moy_p_rec_sql)
head(pmoy_rec)

#2-table sol---------------------
sol<-cbind(donnees_complet$zone,pmoy_rec,esp_tot)
colnames(sol)[1]<-"zone"
summary(sol)
#3- faire un classement selon le pourcentage de substrat le plus eleve ---------------------
#je veux que pour chacune des rangees il compare les 4 colones et identifie
#celle avec le plus au pourcentage
for (i in 1:nrow(sol)){
 if (sol$pmoy_sable[i]> (sol$pmoy_gravier & sol$pmoy_gravier & sol$pmoy_caillou & sol$pmoy_bloc & sol$pmoy_rochemere)){
    sol$g_sub[i] <- "s"
    }
  else{
    if (sol$pmoy_gravier[i]> (sol$pmoy_sable & sol$pmoy_caillou & sol$pmoy_bloc & sol$pmoy_rochemere)){
      sol$g_sub[i] <- "g"
    }
    else{
      if (sol$pmoy_caillou[i]> (sol$pmoy_sable & sol$pmoy_gravier & sol$pmoy_bloc & sol$pmoy_rochemere)){
        sol$g_sub[i] <- "c"
      }
      else{
        if (sol$pmoy_bloc[i]> (sol$pmoy_sable & sol$pmoy_gravier & sol$pmoy_caillou & sol$pmoy_rochemere)){
          sol$g_sub[i] <- "b"
        }
        else{
          if (sol$pmoy_rochemere[i]> (sol$pmoy_sable & sol$pmoy_gravier & sol$pmoy_caillou & sol$pmoy_bloc)){
            sol$g_sub[i] <- "r"
          }
        }
      }
    }
  }
}


#4-ANOVA et pca------------------------
d_sol<-as.data.frame(sol)
mod5<-manova(cbind(pmoy_sable,pmoy_gravier,pmoy_caillou,pmoy_bloc,pmoy_rochemere) ~ ID, data = d_sol)
summary.aov(mod5)

pca_mod5<-prcomp((d_sol[,c(2:7)]), center=TRUE,scale=TRUE)
summary(pca_mod5)

str(pca_mod5)

#delimitation des groupe devrait fonctionner lorsque boucle sera corriger
dev.off()#effacer figure deja ouverte
ggbiplot(pca_mod5,labels=rownames(d_sol[,c(2:7)]), obs.scale=1, var.scale=1, ellipse=TRUE, groups=sol$g_sub)+
  scale_colour_manual(name= "pony",values=c("blue","purple2","pink","orange"))

#5- en fonction de l autre variable y--------------------------------
d_sol<-as.data.frame(sol)
mod6<-manova(cbind(pmoy_sable,pmoy_gravier,pmoy_caillou,pmoy_bloc,pmoy_rochemere) ~ esp_tot, data = d_sol)
summary.aov(mod6)

pca_mod6<-prcomp((d_sol[,c(3:8)]), center=TRUE,scale=TRUE)
summary(pca_mod6)

str(pca_mod5)

#delimitation des groupe devrait fonctionner lorsque boucle sera corriger
dev.off()#effacer figure deja ouverte
ggbiplot(pca_mod6,labels=rownames(d_sol[,c(3:7)]), obs.scale=1, var.scale=1, ellipse=TRUE, groups=sol$g_sub)+
  scale_colour_manual(name= "pony",values=c("blue","purple2","pink","orange"))

#6- avec les deux varaibles y ---------------------------------
d_sol<-as.data.frame(sol)
mod7<-manova(cbind(pmoy_sable,pmoy_gravier,pmoy_caillou,pmoy_bloc,pmoy_rochemere) ~ cbind(ID,esp_tot), data = d_sol)
summary.aov(mod7)

pca_mod7<-prcomp((d_sol[,c(2:8)]), center=TRUE,scale=TRUE)
summary(pca_mod7)

str(pca_mod7)

#delimitation des groupe devrait fonctionner lorsque boucle sera corriger
dev.off()#effacer figure deja ouverte
ggbiplot(pca_mod7,labels=rownames(d_sol[,c(2:8)]), obs.scale=1, var.scale=1, ellipse=TRUE, groups=sol$sub_zone)+
  scale_colour_manual(name= "pony",values=c("blue","purple2","pink","orange"))
#===========================================
#note de cours biometrie assite
#===========================================
#analyses
#maximum likelihood
#nous permet de trouver le modele ajuste qui decrit le mieux nos donnees
#SSE
#correspond a la variance dans les donnees qui nest pas expliquer par notre modele, on veut donc la reduire au maximum
#SSY=SSr+SSE
#SSY = observation i - observation moyenne
#SSR (somme des carres du a la regression) = prediction i - observation moyenne
#SSE = observation i - prediction  i
#si les variables explicatives explique pas/peu y alors SSY=SSE
#R^2 quantifie la proportion de la variance de y qui est prise en compte par les variables explicatives
#R^2 = SSR/SSY = SSY-SSE /SSY
#R^2 adj (ajuste) = 1- (1-R^2)((n-1)/n-k-1)
#permet de tenir compte que la tendance a augmenter avec le nb de variables explicative **particulierement imoprtant si n est faible**

#determiner sil y a presence dune correlation ou simplement dune relation dans la regression
#si correlation=> r=1 ou -1

#ANOVA
#compare la variance inter-traitement à celle intra-traitement
#si inter > intra alors la variable explicative a un effet plus important que l'erreur aleatoire et est donc susceptible d'influencer significativement la variable reponse
#anova permet de comparer des modeles nicher entre eux
#=================================================

#=====================
#test de correlation
#====================
#on veut savoir sil y a une correlation entre le nb_esp et les types dhabitats
#cor.test()
#paarson, kendall, sperman


#====================================
#test danalyse possible
#================================
#####Régression lineraire ???
#critere de base --------------
#prealable-existance ---
#y est une variable que nous avons echantillonne de facon aleatoire
#moy_mobile=mean(mobile$nb_espece)
#moy_peumobile=mean(peumobile$nb_espece)
#var_mobile=var(mobile$nb_espece)
#var_peumobile=var(peumobile$nb_espece)
#mod_ind_mobile<-lm(mobile$nb_espece~recouvrement$p_non_biotique+recouvrement$p_sable+recouvrement$p_gravier+recouvrement$p_caillou+recouvrement$p_bloc+recouvrement$p_rochemere)
#summary(mod_ind_mobile)
#anova(mod_ind_mobile)
#mod_ind_peumobile<-lm(peumobile$nb_espece~recouvrement$p_non_biotique+recouvrement$p_sable+recouvrement$p_gravier+recouvrement$p_caillou+recouvrement$p_bloc+recouvrement$p_rochemere)
#summary(mod_ind_peumobile)
#anova(mod_ind_peumobile)
#prealable-independance--
#les donnees ne sont pas necessairement independantes puisque les pourcentages de granulometrie sont complementaire
#le pourcentage de recouvrement non_biotique pourrait egalement etre interrelier au pourcentage de granulo puisquil correspond lors de lanalyse des images pour le recouvrement biotique a tout ce qui ne letait pas et qui etait donc inevitablement granulo
#FAIRE TEST CORRELATION !!!!!!!!!pour determiner les variables explicatives a utiliser
#est-ce que si non respect independance = glm ou impossible => ANOVA ?
#quelles sont les sortent dANOVA ?
#mod_ind_mobile<-lm(mobile$nb_espece~recouvrement$p_non_biotique+recouvrement$p_sable+recouvrement$p_gravier+recouvrement$p_caillou+recouvrement$p_bloc+recouvrement$p_rochemere)
