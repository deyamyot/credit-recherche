#set working directory
setwd("C:/Users/deyam/Desktop/School/universite/S6/credit de recherche/credit2/credit-recherche")

#######################################
#importation data

donnees_base<-read.table("data/donnees_deploiment.csv", header=T,sep=";", dec=".")[-c(33:39),-c(14:15)]
mobile<-read.table("data/MaxN_mobile.csv", header=T,sep=";",dec=".")[-33,]
peumobile<-read.table("data/decompte_peumobile.csv", header=T, sep=";", dec=".")[,-16]
recouvrement<-read.table("data/pourcentage_recouvrement.csv", header=T, sep=";",dec=".")[-147,]

colnames(donnees_base)[5]<-"coordonnee_y"

#install.packages("sdmpredictors")
#install.packages("leaflet")
#install.packages("raster")
library(sdmpredictors)
library(leaflet)
library(raster)

#importation des donnees environnementale a partir de bio-oracle
layers_physico<-list_layers(dataset="Bio-ORACLE")
temp<-load_layers(layercodes="BO2_tempmean_bdmin", equalarea=FALSE, rasterstack=TRUE) # moy de 2000 a 2014, en celsius
sal<-load_layers(layercodes="BO2_salinitymean_bdmin",equalarea=FALSE, rasterstack=TRUE)# moy de de 2000 a 2014, en PSS (practical salinity scale)
my.sites <- data.frame(Id=donnees_base$ID, Lat=donnees_base$coordonnee_y, Lon=donnees_base$coordonnee_x)
my.sites.physico<- data.frame(temperature=extract(temp,my.sites[,2:3]), salinite=extract(sal,my.sites[,2:3]))
 
donnees_complet<-cbind(donnees_base, my.sites.physico)
colnames(donnees_complet)[14] <- "temp_moy" 
colnames(donnees_complet)[15] <- "sal_moy" 

#categorie selon les dates/zones dechantillonnage 
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

#corrections
recouvrement[90,11]<-52
recouvrement[111,9]<-43
recouvrement[111,11]<-11
recouvrement[32,13]<-32
recouvrement[31,13]<-38
recouvrement[26,13]<-42
recouvrement[25,13]<-33

recouvrement$nb_point_ev<-recouvrement$nb_points_tot - recouvrement$nb_points_nonvisible
recouvrement$non_biotique<-recouvrement$nb_point_ev - rowSums(recouvrement[,8:12])

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

#recouvrement substrat
for ( i in 1:nrow(recouvrement)){
  recouvrement$p_sable<-(recouvrement[,13]/recouvrement[,19])*100
  recouvrement$p_gravier<-(recouvrement[,14]/recouvrement[,19])*100
  recouvrement$p_caillou<-(recouvrement[,15]/recouvrement[,19])*100
  recouvrement$p_bloc<-(recouvrement[,16]/recouvrement[,19])*100
  recouvrement$p_rochemere<-(recouvrement[,17]/recouvrement[,19])*100
  i=i+1
}

#######################################
####Table SQL
#######################################

#install.packages('RSQLite')
library(RSQLite)
connexion <- dbConnect(SQLite(), dbname="C:/Users/deyam/Desktop/School/universite/S6/credit de recherche/credit2/credit-recherche/lien.db")


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
  zone INTEGER (4),
    PRIMARY KEY (ID)
);'

dbSendQuery(connexion, donnees_complet_sql)
dbListTables(connexion)

#injection des donnees
dbWriteTable(connexion, append = TRUE, name= "mobile", value= mobile, row.names = FALSE)
dbWriteTable(connexion, append = TRUE, name = "peumobile", value = peumobile, row.names = FALSE)
dbWriteTable(connexion, append = TRUE, name = "recouvrement", value = recouvrement, row.names = FALSE)
dbWriteTable(connexion, append = TRUE, name = "donnees_complet", value = donnees_complet, row.names = FALSE)


#################################################################
##### manipulation des donnees abondance
################################################################

#---------------------------
#1-valeur max pour mobile, peumobile et nonmobile
max_valeur_rec_sql<-"
SELECT ID, max(nb_espece) AS nb_espece
FROM recouvrement 
GROUP BY ID
;"
nb_esp_rec<-dbGetQuery(connexion, max_valeur_rec_sql)
head(nb_esp_rec)


max_valeur_pm_sql<-"
SELECT ID, max(nb_espece) AS nb_espece
FROM peumobile
GROUP BY ID
;"
nb_esp_pm<-dbGetQuery(connexion, max_valeur_pm_sql)
head(nb_esp_pm)

max_pm_sql<-"
SELECT ID, max(oursin) AS oursin, max(dollars_sable) AS dollars_sable, max(anemone_p) AS anemone_p, max(anemone_r) AS anemone_r, max(anemone_pb) AS anemone_pb, max(concombre_v) AS concombre_v, max(concombre_r) AS concombre_r, max(ver_noire) AS ver_noire,max(coraux_mou) AS coraux_mou
FROM peumobile
GROUP BY ID
;"
max_pm<-dbGetQuery(connexion, max_pm_sql)
head(max_pm)

#------------------------------------
#2-table donnees gradient
gradient<-cbind(mobile$ID,mobile$nb_esp,nb_esp_rec$nb_esp,nb_esp_pm$nb_esp,donnees_complet$coordonnee_x,donnees_complet$coordonnee_y,donnees_complet$temp_moy,donnees_complet$sal_moy)
colnames(gradient)<-c("ID","esp_mobile","esp_rec","esp_peumobile","coordonne_x","coordonnne_y","temp_moy","sal_moy")
esp_tot<-rowSums(gradient[,c(2:4)]) 
gradient<-as.data.frame(cbind(gradient,esp_tot))
summary(gradient)


#---------------------------------------------------------------
#3-faire une table qui rassemble toutes les donnnes dabondance-------------
ab_esp<-cbind(max_pm,mobile[,2:18],donnees_complet$zone)
colnames(ab_esp)[28]<-"zone"

#--------------------------------------------------------------
#4-tableau abondance relative
tot<-colSums(ab_esp[,c(2:27)])
tot<-as.numeric(c("NA",tot, "NA"))
ab_esp<-rbind(ab_esp,tot)
rownames(ab_esp)[33]<-"tot"

#install.packages(dplyr)
library(dplyr)
abr<- ab_esp[,-c(1,28)]
abr<-abr%>%mutate_if(is.character,as.numeric)# pour regler l erreur de chiffre non binaire

for (i in 1:32){
  for (j in 1:ncol(abr)){
    if( abr[33,j]==0 ){
      abr[i,j]<-0
    }
      else { abr[i,j] <- (abr[i,j]/abr[33,j])
      }
    }
  }




###############################################
#### Manipulation donnees recouvrement
###############################################

#1-moyenne valeur de pourcentage de recouvrement---------------
moy_p_rec_sql<-"
SELECT ID, avg(p_sable) AS pmoy_sable, avg(p_gravier) AS pmoy_gravier, avg(p_caillou) AS pmoy_caillou, avg(p_bloc) AS pmoy_bloc, avg(p_rochemere) AS pmoy_rochemere
FROM recouvrement 
GROUP BY ID
;"
pmoy_rec<-dbGetQuery(connexion, moy_p_rec_sql)
head(pmoy_rec)

#2-table sol---------------------
abr<-abr[-33,]
sol<-cbind(donnees_complet$zone,pmoy_rec,esp_tot,abr)
colnames(sol)[1]<-"zone"
summary(sol)

#3- categorie selon le pourcentage de recouvrement du substrat---------------------
for (i in 1:nrow(sol)){
if (((sol[i,3])> (sol[i,4]))& ((sol[i,3])> (sol[i,5]))& ((sol[i,3])> (sol[i,6]))&((sol[i,3])> (sol[i,7])))  {
  sol$g_sub[i] <- "s" #sable
}
  else if (((sol[i,4])> (sol[i,3]))& ((sol[i,4])> (sol[i,5]))& ((sol[i,4])> (sol[i,6]))&((sol[i,4])> (sol[i,7]))){
    sol$g_sub[i] <- "g" #gravier
  }
  else if (((sol[i,5])> (sol[i,3]))& ((sol[i,5])> (sol[i,6]))& ((sol[i,5])> (sol[i,7]))&((sol[i,5])> (sol[i,4]))){
    sol$g_sub[i] <- "c" #cailloux
  }
  else if (((sol[i,6])> (sol[i,3]))& ((sol[i,6])> (sol[i,5]))& ((sol[i,6])> (sol[i,5]))&((sol[i,6])> (sol[i,7]))){
    sol$g_sub[i] <- "b" #bloc
  }
  else if (((sol[i,7])> (sol[i,3]))& ((sol[i,7])> (sol[i,5]))& ((sol[i,7])> (sol[i,6]))&((sol[i,7])> (sol[i,4]))){
    sol$g_sub[i] <- "r" #rochemere
    
  }
}

#4- boucle categorie pour les pourcentage de recouvrement biotique----------------
moy_p_bio_sql<-"
SELECT ID, avg(p_algue_r) AS pmoy_algue_r, avg(p_algue_b) AS pmoy_algue_b, avg(p_algue_v) AS pmoy_algue_v, avg(p_coralline) AS pmoy_coralline, avg(p_coquillage) AS pmoy_coquillage
FROM recouvrement 
GROUP BY ID
;"
pmoy_bio<-dbGetQuery(connexion, moy_p_bio_sql)
head(pmoy_bio)

#table biotique---------------------
bio<-cbind(donnees_complet$zone,pmoy_bio,esp_tot,abr)
colnames(bio)[1]<-"zone"
summary(bio)

#diviser en categorie de pourcentage de recouvrement biotique-----------------
for (i in 1:nrow(bio)){
  if (((bio[i,3])> (bio[i,4]))& ((bio[i,3])> (bio[i,5]))& ((bio[i,3])> (bio[i,6]))&((bio[i,3])> (bio[i,7])))  {
    bio$b_sub[i] <- "r" #algue rouge
  }
  else if (((bio[i,4])> (bio[i,3]))& ((bio[i,4])> (bio[i,5]))& ((bio[i,4])> (bio[i,6]))&((bio[i,4])> (bio[i,7]))){
    bio$b_sub[i] <- "b" #algue brune
  }
  else if (((bio[i,5])> (bio[i,3]))& ((bio[i,5])> (bio[i,6]))& ((bio[i,5])> (bio[i,7]))&((bio[i,5])> (bio[i,4]))){
    bio$b_sub[i] <- "v" #algue brune
  }
  else if (((bio[i,6])> (bio[i,3]))& ((bio[i,6])> (bio[i,5]))& ((bio[i,6])> (bio[i,5]))&((bio[i,6])> (bio[i,7]))){
    bio$b_sub[i] <- "c" #coralline
  }
  else if (((bio[i,7])> (bio[i,3]))& ((bio[i,7])> (bio[i,5]))& ((bio[i,7])> (bio[i,6]))&((bio[i,7])> (bio[i,4]))){
    bio$b_sub[i] <- "s" #coquillage
    
  }
}

#################
#ordination
#################
#https://ourcodingclub.github.io/tutorials/ordination/
#https://www.datanovia.com/en/blog/cluster-analysis-in-r-simplified-and-enhanced/
#https://archetypalecology.wordpress.com/2018/02/18/non-metric-multidimensional-scaling-nmds-what-how/
#abondance pour chaque site en fonction des especes 
#utilisation de la table ab_esp
ab_esp_bo<-ab_esp[-33,-c(1,5,19,21,28)]#garder seulement les rangees et colones dabondance
ab_esp_r<-ab_esp[-33,-c(1,5,19,21,28)]#enlever etoile_t et anemone_r pcq aucun ind compter
ab_esp_b<-ab_esp[-33,-c(1,5,19,21,28)]#enlever cnetophore pcq pas mobile

#install.packages("vegan")
library(vegan)
#install.packages("factoextra")
library(factoextra)
#install.packages("ggplot2")
library(ggplot2)
############################# avec donnees boolean
#transformation de la table en valeur booleenne
for (i in 1:nrow(ab_esp_bo)){
  for (j in 1:ncol(ab_esp_bo)){
    if( ab_esp_bo[i,j]>0 ){
      ab_esp_bo[i,j]<-1
    }
  }
}


# dis_esp_bo<-vegdist(ab_esp_bo, method="bray")
# ordiplot(dis_esp_bo)# x et y ?
# fviz_dist(dis_esp_bo)
NMDS_bo<-metaMDS(ab_esp_bo,distance="bray",k=2, noshare=TRUE, trymax=1000,trace=FALSE)
sppscores(NMDS_bo)<-ab_esp_bo
NMDS_bo
summary(NMDS_bo)
g_NMDS_bo<-ordiplot(NMDS_bo, type="t") 
stressplot(NMDS_bo, main="stress de l'ordination pr?sence-absence")


# s_bo<-scale(ab_esp_bo)
# res_hc_bo<-eclust(s_bo,FUNcluster="hclust", method="bray")
# fviz_dend(res_hc_bo, resct=TRUE)

############################# avec donnees racine 4e
for (i in 1:nrow(ab_esp_r)){
  for (j in 1:ncol(ab_esp_r)){
    if( ab_esp_r[i,j]>0 ){
      ab_esp_r[i,j]<- (ab_esp_r[i,j]^(1/4))
    }
  }
}

# dis_esp_r<-vegdist(ab_esp_r, method="bray")
# ordiplot(dis_esp_r)# x et y ?
# fviz_dist(dis_esp_r)
NMDS_r<-metaMDS(ab_esp_r, distance="bray",k=2, noshare=TRUE, trymax=1000, trace=FALSE)#bray par defaut
sppscores(NMDS_r)<-ab_esp_r 
NMDS_r
ordiplot(NMDS_r, type="t")
stressplot(NMDS_r)

# s_r<-scale(ab_esp_r)
# res_hc_r<-eclust(s_r,FUNcluster="hclust",method="bray")
# fviz_dend(res_hc_r)#pas le mm classement

############################# avec donnees brutes
# dis_esp_b<-vegdist(ab_esp_b, method="bray")
# ordiplot(dis_esp_b)# x et y ?
# fviz_dist(dis_esp_b)
NMDS_b<-metaMDS(ab_esp_b, noshare=TRUE, distance="bray",k=2, trymax=1000, trace=FALSE)
sppscores(NMDS_b)<-ab_esp_b
NMDS_b
ordiplot(NMDS_b, type="t")
stressplot(NMDS_b)

s_b<-scale(ab_esp_b)
res_hc_b<-eclust(s_b,FUNcluster="hclust", method="bray")
fviz_dend(res_hc_b,cex=0.5,k_colors="black")#pas le mm classement 

############################
#DISTLM
#############################
#install.packages("dbstats")
library(dbstats)

#table regroupement variables env
#temp+sal+recouvrement sol+recouvrment bio
table_env<-as.data.frame(cbind(gradient$ID,gradient$temp_moy,gradient$sal_moy,sol$pmoy_sable,sol$pmoy_gravier,sol$pmoy_caillou,sol$pmoy_bloc,sol$pmoy_rochemere, bio$pmoy_algue_r,bio$pmoy_algue_b,bio$pmoy_algue_v, bio$pmoy_coralline, bio$pmoy_coquillage))
colnames(table_env)<-c("ID","temp_moy", "sal_moy","pmoy_sable","pmoy_gravier","pmoy_caillou","pmoy_bloc","pmoy_rochemere","pmoy_algue_r","pmoy_algue_b","pmoy_algue_v","pmoy_coralline","pmoy_coquillage")
#ajoute des divisions par groupe selon les cluster
#division selon booleen
# for (i in 1:nrow(table_env)){
#   if (table_env$ID[i]==10|table_env$ID[i]==1|table_env$ID[i]==6|table_env$ID[i]==22|table_env$ID[i]==5|table_env$ID[i]==11|table_env$ID[i]==7|table_env$ID[i]==8|table_env$ID[i]==9){
#     table_env$groupe_bo[i]<-"1"
#   }
#   else if (table_env$ID[i]==2|table_env$ID[i]==3){
#     table_env$groupe_bo[i]<-"2"
#   }
#   else if (table_env$ID[i]==28|table_env$ID[i]==30|table_env$ID[i]==19|table_env$ID[i]==24|table_env$ID[i]==32|table_env$ID[i]==31|table_env$ID[i]==18|table_env$ID[i]==12|table_env$ID[i]==14){
#     table_env$groupe_bo[i]<-"3"
#   }
#   else if (table_env$ID[i]==29|table_env$ID[i]==20|table_env$ID[i]==21|table_env$ID[i]==27|table_env$ID[i]==5|table_env$ID[i]==4|table_env$ID[i]==26){
#     table_env$groupe_bo[i]<-"4"
#   }
#   else {
#     table_env$groupe_bo[i]<-"5"
#   }
# }

# #division selon racine 4e
# for (i in 1:nrow(table_env)){
#   if (table_env$ID[i]==22|table_env$ID[i]==3){
#     table_env$groupe_r[i]<-"1"
#   }
#   else if (table_env$ID[i]==13|table_env$ID[i]==17|table_env$ID[i]==23|table_env$ID[i]==15|table_env$ID[i]==16){
#     table_env$groupe_r[i]<-"2"
#   }
#   else if (table_env$ID[i]==10|table_env$ID[i]==22|table_env$ID[i]==4|table_env$ID[i]==1|table_env$ID[i]==6|table_env$ID[i]==5|table_env$ID[i]==8|table_env$ID[i]==9|table_env$ID[i]==7|table_env$ID[i]==11){
#     table_env$groupe_r[i]<-"3"
#   }
# 
#   else {
#     table_env$groupe_r[i]<-"4"
#   }
# }



# #division selon brute
# for (i in 1:nrow(table_env)){
#   if (table_env$ID[i]==16|table_env$ID[i]==13|table_env$ID[i]==17|table_env$ID[i]==23|table_env$ID[i]==28|table_env$ID[i]==30|table_env$ID[i]==10|table_env$ID[i]==4|table_env$ID[i]==7|table_env$ID[i]==11|table_env$ID[i]==25|table_env$ID[i]==5|table_env$ID[i]==8|table_env$ID[i]==26|table_env$ID[i]==6|table_env$ID[i]==9){
#     table_env$groupe_b[i]<-"1"
#   }
#   else if (table_env$ID[i]==31|table_env$ID[i]==20|table_env$ID[i]==22|table_env$ID[i]==29|table_env$ID[i]==32|table_env$ID[i]==29|table_env$ID[i]==15|table_env$ID[i]==21|table_env$ID[i]==19|table_env$ID[i]==27|table_env$ID[i]==1|table_env$ID[i]==24|table_env$ID[i]==12|table_env$ID[i]==14|table_env$ID[i]==18){
#     table_env$groupe_b[i]<-"2"
#   }
#   else {
#     table_env$groupe_bo[i]<-"3"
#   }
# }
#division selon brute
for (i in 1:nrow(table_env)){
  if (table_env$ID[i]==16|table_env$ID[i]==13|table_env$ID[i]==17|table_env$ID[i]==23){
    table_env$groupe_b[i]<-"1"
  }
  else if (table_env$ID[i]==28|table_env$ID[i]==30){
    table_env$groupe_b[i]<-"2"
  }
  else if (table_env$ID[i]==10|table_env$ID[i]==4|table_env$ID[i]==7|table_env$ID[i]==11|table_env$ID[i]==25|table_env$ID[i]==5|table_env$ID[i]==8|table_env$ID[i]==26|table_env$ID[i]==6|table_env$ID[i]==9){
    table_env$groupe_b[i]<-"3"
  }
  else if (table_env$ID[i]==31|table_env$ID[i]==20|table_env$ID[i]==22|table_env$ID[i]==29|table_env$ID[i]==32|table_env$ID[i]==15|table_env$ID[i]==21|table_env$ID[i]==19|table_env$ID[i]==27|table_env$ID[i]==1|table_env$ID[i]==24|table_env$ID[i]==12|table_env$ID[i]==14|table_env$ID[i]==18){
    table_env$groupe_b[i]<-"4"
  }
  else {
    table_env$groupe_b[i]<-"5"
  }
}
##dbrda booleen
# dbrda_bo<-dbrda(ab_esp_bo~temp_moy+sal_moy+pmoy_sable+pmoy_gravier+pmoy_caillou+pmoy_bloc+pmoy_rochemere+pmoy_algue_r+pmoy_algue_b+pmoy_algue_v+pmoy_coralline+pmoy_coquillage,data=table_env, distance="bray", scale=TRUE, sqrt.dsit=TRUE)
# sppscores(dbrda_bo)<-ab_esp_bo
# plot(dbrda_bo) 
# summary(dbrda_bo)
##dbrda racine 4e
# dbrda_r<-dbrda(ab_esp_r~temp_moy+sal_moy+pmoy_sable+pmoy_gravier+pmoy_caillou+pmoy_bloc+pmoy_rochemere+pmoy_algue_r+pmoy_algue_b+pmoy_algue_v+pmoy_coralline+pmoy_coquillage,data=table_env, distance="bray", scale=TRUE, sqrt.dist=TRUE)
# sppscores(dbrda_r)<-ab_esp_r
# plot(dbrda_r)
# anova.cca(dbrda_r)
# summary(dbrda_r)
#dbrda brute
dbrda_b<-dbrda(ab_esp_b~temp_moy+sal_moy+pmoy_sable+pmoy_gravier+pmoy_caillou+pmoy_bloc+pmoy_rochemere+pmoy_algue_r+pmoy_algue_b+pmoy_algue_v+pmoy_coralline+pmoy_coquillage,data=table_env, distance="bray", scale=TRUE, sqrt.dsit=TRUE)
#sppscores(dbrda_b)<-ab_esp_b
plot(dbrda_b,col=factor(table_env$groupe_b))
#legend("topleft",legend=levels(factor(table_env$groupe_b)),pch=19,col=factor(levels(factor(table_env$groupe_b))))
#fviz_plot(dbrda_b,label="var", habillage=sol$g_sub ,addEllipses = TRUE, ellipse.level=0.95, col.ind=sol$g_sub)
#anova.cca(dbrda_b)
summary(dbrda_b)
############################
#simper
###########################


#nous donne pour la comparaison des groupes
#le pourcentage de contribution des esp a la distance entre les esp (bray) de la communaute
#donc le pourcentage de contribution d'une esp a la moyenne de dissimilarite bray entre les groupes
#les esp qui sont resorties correspondent a 70% de la difference entre les groupes
#sim_bo<-simper(ab_esp_bo,table_env$groupe_bo)
#sim_r<-simper(ab_esp_r,table_env$groupe_r)
sim_b<-simper(ab_esp_b,table_env$groupe_b)
summary(sim_b,ordered=TRUE)
sim_b
##################
#Graphique presentation
#####################
#selon les esp les plus importantes ressortie par simper 
#un par comparaison de groupe
#abondance par site
ab_esp_b<-as.matrix(ab_esp_b)
n_site<-as.character(1:32)
barplot_amphipode<-barplot(ab_esp_b[,17],beside=TRUE,names.arg=n_site, xlab="Sites", ylab="Abondance maximale (MAxN)",main="Répartition de l'abondance des amphipodes",ylim=c(0,400))
barplot_oursin<-barplot(ab_esp_b[,1],beside=TRUE,names.arg=n_site, xlab="Sites", ylab="Abondance maximale (MaxN)",main="Répartition de l'abondance des oursins",ylim=c(0,400))
barplot_dollars<-barplot(ab_esp_b[,2],beside=TRUE,names.arg=n_site, xlab="Sites", ylab="Abondance maximale (MAxN)",main="Répartition de l'abondance des dollars de sable",ylim=c(0,400))

#abondance totale
ab_tot<-as.matrix(ab_esp[33,-c(1,5,19,21,28)])
barplot_ab_tot<-barplot(ab_tot, las=2, ylim=c(0,3000),cex.axis=1,cex=0.8,ylab="Abondance totale", main="Répartition de l'abondance des espèces pour toutes les sites")

#abondance par site
# r_g<-as.matrix(sol[,c(3:7)])
# colnames(r_g)<-c("p_sable","p_caillou","p_gravier","p_bloc","p_rochemère")
barplot_r_g<-barplot(sol[,3], ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en sable")
barplot_r_caillou<-barplot(sol[,4],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en caillou", beside=TRUE)
barplot_r_gravier<-barplot(sol[,5],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en gravier", beside=TRUE)
barplot_r_bloc<-barplot(sol[,6],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en bloc", beside=TRUE)
barplot_r_rochemere<-barplot(sol[,7],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en roche mère", beside=TRUE)

#abondance par site
barplot_r_algue_r<-barplot(bio[,3],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en algues rouge", beside=TRUE)
barplot_r_algue_b<-barplot(bio[,4],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en algues brune", beside=TRUE)
barplot_r_algue_v<-barplot(bio[,5],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en algues verte", beside=TRUE)
barplot_r_coralline<-barplot(bio[,6],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en corallines", beside=TRUE)
barplot_r_coquillage<-barplot(bio[,7],names.arg=n_site, ylim=c(0,100),xlab="Sites",ylab="Pourcentage de recouvrement(%)", main="Pourcentage de recouvrement en coquillages", beside=TRUE)

#abondance moyenne
moy_ab<-colMeans(ab_esp_b)
sd_ab<-apply(ab_esp_b,2,sd)
barplot_abmoy<-barplot(moy_ab, las=2, ylim=c(0,200),cex.axis=1,cex=0.8,ylab="Abondance maximale moyenne (ind./site)", main="Abondance moyenne maximale par espèce")
#moy_barplot<-barplot(moy_ab, xlab="espèces", ylab="SLA moyen (mm2/mg)",ylim=c(0,400))
arrows(barplot_abmoy, moy_ab+sd_ab,barplot_abmoy, moy_ab - sd_ab, angle = 90,code=3, length=0.1) 
#legend("topright",legend = rownames(moy_ab),pch = 15,col = 1:nrow(moy_ab), col=c("#eb8060","#a1e9f0"))

#regression pour imager conclusion
#oursin
dev.off()
rec_rm<-plot(ab_esp_b[,1]~table_env[,8], xlab="Pourcentage de recouvrement en roche mère (%)",ylab="Abondance maximale d'oursins (MaxN/site)", main="Relation entre le type de substrat et l'abondace des oursins")
abline(lm(ab_esp_b[,1]~table_env[,8]))
rec_bloc<-plot(ab_esp_b[,1]~table_env[,7], xlab="Pourcentage de recouvrement en bloc (%)", ylab="Abondance maximale d'oursins (MaxN/site)",main="Relation entre le type de substrat et l'abondace des oursins")
abline(lm(ab_esp_b[,1]~table_env[,7]))
rec_rm<-plot(ab_esp_b[,1]~table_env[,6], xlab="Pourcentage de recouvrement en caillou (%)",ylab="Abondance maximale d'oursins (MaxN/site)" ,main="Relation entre le type de substrat et l'abondace des oursins")
abline(lm(ab_esp_b[,1]~table_env[,6]))
rec_rm<-plot(ab_esp_b[,1]~table_env[,5], xlab="Pourcentage de recouvrement en gravier (%)",ylab="Abondance maximale d'oursins (MaxN/site)",main="Relation entre le type de substrat et l'abondace des oursins" )
abline(lm(ab_esp_b[,1]~table_env[,5]))
rec_rm<-plot(ab_esp_b[,1]~table_env[,4], xlab="Pourcentage de recouvrement en sable (%)",ylab="Abondance maximale d'oursins (MaxN/site)",main="Relation entre le type de substrat et l'abondace des oursins" )
abline(lm(ab_esp_b[,1]~table_env[,4]))
#amphipode
rec_rm<-plot(table_env[,4]~ab_esp_b[,17], xlab="Abondance maximale d'amphipodes (MaxN/site)",ylab="Pourcentage de recouvrement en sable (%)",main="Relation entre le type de substrat et l'abondace des amphiopdes" )
abline(lm(table_env[,4]~ab_esp_b[,17]))
#coralline
rec_bloc<-plot(table_env[,12]~ab_esp_b[,1], xlab="Abondance maximale d'oursins (MaxN/site)", ylab="Pourcentage de recouvrement de coralline (%)",main="Relation entre le recouvrement en coralline et l'abondace des oursins")
abline(lm(table_env[,12]~ab_esp_b[,1]))
rec_bloc<-plot(table_env[,7]~table_env[,12], xlab="Abondance maximale de coralline (MaxN/site)", ylab="Pourcentage de recouvrement en bloc (%)",main="Relation entre le type de substrat et l'abondace de coralline")
abline(lm(table_env[,7]~table_env[,12]))

#pie recouvrement sol
par(bg="transparent")
rec<-table_env[,c(4:8)]
rec<-as.matrix(rec)
pie(rec[1,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[2,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[3,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[4,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[5,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[6,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[7,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[8,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[9,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[10,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[11,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[12,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[13,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[14,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[15,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[16,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[17,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[18,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[19,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[20,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[21,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[22,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[23,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[24,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[25,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[26,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[27,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[28,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[29,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[30,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[31,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))
pie(rec[32,], labels=c("%sable","%gravier","%caillou","%bloc","%roche mère"),col=c("#e3d4af","#be9b7b","#726255","#4b3832","#000000"))

#pie recouvrement bio
par(bg="transparent")
rec_b<-table_env[,c(9:13)]
rec_b<-as.matrix(rec_b)
pie(rec_b[1,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[2,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[3,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[4,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[5,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[6,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
#pie(rec_b[7,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[8,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[9,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[10,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
#pie(rec_b[11,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[12,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[13,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[14,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[15,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[16,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[17,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[18,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[19,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[20,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[21,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[22,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[23,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[24,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[25,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[26,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[27,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[28,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[29,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[30,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[31,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))
pie(rec_b[32,], labels=c("%algues rouge","%algues brune","%algues verte","%coralline","%coquillage"),col=c("#990000","#4b3832","#47764b","#cc78a1","#fff4e6"))

