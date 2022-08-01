#set working directory
setwd("E:/Audrey_Anne/Godbout_analyse/credit de recherche/gitcredit_recherche")

#######################################
#importation data
donnees_base<-read.table("data/donnees_deploiment.csv", header=T,sep=";", dec=".")[-c(33:39),-c(14:15)]
mobile<-read.table("data/MaxN_mobile.csv", header=T,sep=";",dec=".")[-33,]
peumobile<-read.table("data/decompte_peumobile.csv", header=T, sep=";", dec=".")[,-16]
recouvrement<-read.table("data/pourcentage_recouvrement.csv", header=T, sep=";",dec=".")[-147,]

colnames(donnees_base)[5]<-"coordonnee_y"
########################################
#physico
#instalation package
install.packages("sdmpredictors")
install.packages("leaflet")
install.packages("raster")

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
temp<-load_layers(layercodes="BO2_tempmean_bdmin", equalarea=FALSE, rasterstack=TRUE)
sal<-load_layers(layercodes="BO2_salinitymean_bdmin",equalarea=FALSE, rasterstack=TRUE)
#data.frame selon les coordonnees des sites desirer
my.sites <- data.frame(Id=donnees_base$ID, Lat=donnees_base$coordonnee_y, Lon=donnees_base$coordonnee_x)

#extraction donnees pour chacun des points
my.sites.physico<- data.frame(temperature=extract(temp,my.sites[,2:3]), salinite=extract(sal,my.sites[,2:3]))
#prendre en consideration que se sont des temperature moyenne annuelle et donc il est normale-je pense-quelles soient negative 
 
#ajout des donnees dans le document de donnees_bas
donnees_complet<-cbind(donnees_base, my.sites.physico)
colnames(donnees_complet)[14] <- "temp_moy" 
colnames(donnees_complet)[15] <- "sal_moy" 

#####################################
####Manipulation des donnees

#calcul diversite---------
#espece mobile
mobile$nb_espece<-rowSums(mobile[,2:18]>0)
#espece peumobile
peumobile$nb_espece<-rowSums(peumobile[,7:15]>0)


#totale par espece pour une la ligne de cage
#tot<-(apply(mobile[,c(2:18)], 2, sum))


#pourcentage recouvrement-----------

#nb de point evaluer en fonction des point non-visible
recouvrement$nb_point_ev<-recouvrement$nb_points_tot - recouvrement$nb_points_nonvisible
#nb de point pour lequel le recouvrement negale pas a une des categorie biotique
recouvrement$non_biotique<-recouvrement$nb_point_ev - rowSums(recouvrement[,8:12])
#correction nob croissement tot=54
#cadre trop grand = enlever une ranger supplementaire pour avoir le mm nb de croissement que les autres
recouvrement[90,11]<-52
recouvrement[111,9]<-43
recouvrement[111,11]<-11


#recouvrement biotique
for( i in 1:nrow(recouvrement)){
  recouvrement$p_algue_r<-(recouvrement[,8]/recouvrement[,18])*100
  recouvrement$p_algue_b<-(recouvrement[,9]/recouvrement[,18])*100
  recouvrement$p_algue_v<-(recouvrement[,10]/recouvrement[,18])*100
  recouvrement$p_coralline<-(recouvrement[,11]/recouvrement[,18])*100
  recouvrement$p_coquillage<-(recouvrement[,12]/recouvrement[,18])*100
  recouvrement$p_non_biotique<-(recouvrement[,19]/recouvrement[,18])*100
  i=i+1
}

#recouvrement granulo
for ( i in 1:nrow(recouvrement)){
  recouvrement$p_sable<-(recouvrement[,13]/recouvrement[,18])*100
  recouvrement$p_gravier<-(recouvrement[,14]/recouvrement[,18])*100
  recouvrement$p_caillou<-(recouvrement[,15]/recouvrement[,18])*100
  recouvrement$p_bloc<-(recouvrement[,16]/recouvrement[,18])*100
  recouvrement$p_rochemere<-(recouvrement[,17]/recouvrement[,18])*100
  i=i+1
}

####Table SQL
install.packages('RSQLite')
library(RSQLite)

connexion <- dbConnect(SQLite(), dbname="E:/Audrey_Anne/Godbout_analyse/credit de recherche/gitcredit_recherche/lien.db")

#utiliser pour vider la table si problematique
#dbSendQuery(connexion,"DROP TABLE mobile;") 

mobile_sql <- '
CREATE TABLE mobile (
  ID INTEGER(4),
  crab_com INTEGER(4),
  crab_a INTEGER(4),
  crab_n INTEGER(4),
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
#dbSendQuery(connexion,"DROP TABLE peumobile;") 

peumobile_sql <- '
CREATE TABLE peumobile (
  ID INTEGER(4),
  temps INTEGER(4),
  nb_image INTEGER(4),
  num_image CHAR(50),
  nb_points_tot INTEGER(4),
  oursin INTEGER(4), 
  dollars_sable INTEGER(4),
  anemone_p INTEGER(4),
  anemon_r INTEGER(4),
  concombre_v INTEGER(4),
  concombre_r INTEGER(4),
  ver_noire INTEGER(4),
  coraux_mou INTEGER(4),
  nb_espece INTEGER(4),
    PRIMARY KEY (ID)
);'

dbSendQuery(connexion, peumobile_sql)
dbListTables(connexion)


#utiliser pour vider la table si problematique
#dbSendQuery(connexion,"DROP TABLE recouvrement;") 

recouvrement_sql <- '
CREATE TABLE recouvrement (
  ID INTEGER(4),
  temps INTEGER(4),
  nb_image INTEGER(4),
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
  p_nonbiotique REAL(8),
    PRIMARY KEY (ID)
);'

dbSendQuery(connexion, recouvrement_sql)
dbListTables(connexion)

#utiliser pour vider la table si problematique
#dbSendQuery(connexion,"DROP TABLE donnees_complet;") 

donnees_complet_sql <- '
CREATE TABLE donnees_complet (
  ID INTEGER(4),
  num_image REAL(8),
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
    PRIMARY KEY (ID)
);'

dbSendQuery(connexion, donnees_complet_sql)
dbListTables(connexion)


#####Régression lineraire ???
#critere de base --------------
#prealable-existance ---
#y est une variable que nous avons echantillonne de facon aleatoire
moy_mobile=mean(mobile$nb_espece)
moy_peumobile=mean(peumobile$nb_espece)
var_mobile=var(mobile$nb_espece)
var_peumobile=var(peumobile$nb_espece)
mod_ind_mobile<-lm(mobile$nb_espece~recouvrement$p_non_biotique+recouvrement$p_sable+recouvrement$p_gravier+recouvrement$p_caillou+recouvrement$p_bloc+recouvrement$p_rochemere)
summary(mod_ind_mobile)
anova(mod_ind_mobile)
mod_ind_peumobile<-lm(peumobile$nb_espece~recouvrement$p_non_biotique+recouvrement$p_sable+recouvrement$p_gravier+recouvrement$p_caillou+recouvrement$p_bloc+recouvrement$p_rochemere)
summary(mod_ind_peumobile)
anova(mod_ind_peumobile)
#prealable-independance--
#les donnees ne sont pas necessairement independantes puisque les pourcentages de granulometrie sont complementaire
#le pourcentage de recouvrement non_biotique pourrait egalement etre interrelier au pourcentage de granulo puisquil correspond lors de lanalyse des images pour le recouvrement biotique a tout ce qui ne letait pas et qui etait donc inevitablement granulo
#FAIRE TEST CORRELATION !!!!!!!!!pour determiner les variables explicatives a utiliser
#est-ce que si non respect independance = glm ou impossible => ANOVA ?
#quelles sont les sortent dANOVA ?
mod_ind_mobile<-lm(mobile$nb_espece~recouvrement$p_non_biotique+recouvrement$p_sable+recouvrement$p_gravier+recouvrement$p_caillou+recouvrement$p_bloc+recouvrement$p_rochemere)
