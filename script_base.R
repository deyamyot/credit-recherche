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
ab_esp_bo<-ab_esp[-33,-c(1,28)]#garder seulement les rangees et colones dabondance
ab_esp_r<-ab_esp[-33,-c(1,28)]
ab_esp_b<-ab_esp[-33,-c(1,28)]

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
NMDS_bo<-metaMDS(ab_esp_bo, noshare=TRUE, autotransform=FALSE, trymax=1000,trace=FALSE)
sppscores(NMDS_bo)<-ab_esp_bo
ordiplot(NMDS_bo, type="t") 
stressplot(NMDS_bo)
NMDS_bo$confdist

res_hc_bo<-eclust(ab_esp_bo,FUNcluster="hclust")
fviz_dend(res_hc_bo, rect=TRUE)

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
NMDS_r<-metaMDS(ab_esp_r, noshare=TRUE, autotransform=FALSE, trymax=1000, trace=FALSE)#bray par defaut
sppscores(NMDS_r)<-ab_esp_r 
ordiplot(NMDS_r, type="t")
stressplot(NMDS_r)

res_hc_r<-eclust(ab_esp_r,FUNcluster="hclust")
fviz_dend(res_hc_r, rect=TRUE)#pas le mm classement

############################# avec donnees brutes
# dis_esp_b<-vegdist(ab_esp_b, method="bray")
# ordiplot(dis_esp_b)# x et y ?
# fviz_dist(dis_esp_b)
NMDS_b<-metaMDS(ab_esp_b, noshare=TRUE, autotransform=FALSE, trymax=1000, trace=FALSE)
sppscores(NMDS_b)<-ab_esp_b
ordiplot(NMDS_b, type="t")
stressplot(NMDS_b)

res_hc_b<-eclust(ab_esp_b,FUNcluster="hclust")
fviz_dend(res_hc_b)#pas le mm classement 


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
for (i in 1:nrow(table_env)){
  if (table_env$ID[i]==10|table_env$ID[i]==1|table_env$ID[i]==6|table_env$ID[i]==22|table_env$ID[i]==5|table_env$ID[i]==11|table_env$ID[i]==7|table_env$ID[i]==8|table_env$ID[i]==9){
    table_env$groupe_bo[i]<-"1"
  }
  else if (table_env$ID[i]==2|table_env$ID[i]==3){
    table_env$groupe_bo[i]<-"2"
  }
  else if (table_env$ID[i]==28|table_env$ID[i]==30|table_env$ID[i]==19|table_env$ID[i]==24|table_env$ID[i]==32|table_env$ID[i]==31|table_env$ID[i]==18|table_env$ID[i]==12|table_env$ID[i]==14){
    table_env$groupe_bo[i]<-"3"
  }
  else if (table_env$ID[i]==29|table_env$ID[i]==20|table_env$ID[i]==21|table_env$ID[i]==27|table_env$ID[i]==5|table_env$ID[i]==4|table_env$ID[i]==26){
    table_env$groupe_bo[i]<-"4"
  }
  else {
    table_env$groupe_bo[i]<-"5"
  }
}

#division selon racine 4e
for (i in 1:nrow(table_env)){
  if (table_env$ID[i]==22|table_env$ID[i]==7|table_env$ID[i]==11|table_env$ID[i]==8|table_env$ID[i]==9|table_env$ID[i]==1|table_env$ID[i]==4|table_env$ID[i]==6|table_env$ID[i]==10){
    table_env$groupe_r[i]<-"1"
  }
  else if (table_env$ID[i]==13|table_env$ID[i]==17|table_env$ID[i]==23|table_env$ID[i]==3|table_env$ID[i]==5|table_env$ID[i]==16){
    table_env$groupe_r[i]<-"2"
  }

  else {
    table_env$groupe_r[i]<-"3"
  }
}

#division selon brute
for (i in 1:nrow(table_env)){
  if (table_env$ID[i]==1|table_env$ID[i]==24){
    table_env$groupe_b[i]<-"1"
  }
  else if (table_env$ID[i]==14|table_env$ID[i]==28|table_env$ID[i]==12|table_env$ID[i]==21|table_env$ID[i]==2|table_env$ID[i]==29|table_env$ID[i]==17|table_env$ID[i]==18|table_env$ID[i]==19|table_env$ID[i]==27){
    table_env$groupe_b[i]<-"2"
  }
  else if (table_env$ID[i]==4|table_env$ID[i]==7|table_env$ID[i]==11){
    table_env$groupe_b[i]<-"3"
  }
  else if (table_env$ID[i]==10|table_env$ID[i]==8|table_env$ID[i]==22|table_env$ID[i]==6|table_env$ID[i]==9){
    table_env$groupe_b[i]<-"4"
  }
  else {
    table_env$groupe_bo[i]<-"5"
  }
}

#dbrda booleen
dbrda_bo<-capscale(ab_esp_bo[,-19]~temp_moy+sal_moy+pmoy_sable+pmoy_gravier+pmoy_caillou+pmoy_bloc+pmoy_rochemere+pmoy_algue_r+pmoy_algue_b+pmoy_algue_v+pmoy_coralline+pmoy_coquillage,data=table_env, distance="bray", scale=TRUE, sqrt.dsit=TRUE)
plot(dbrda_bo) # enlever les amphipodes

#dbrda racine 4e
dbrda_r<-capscale(ab_esp_r[,-19]~temp_moy+sal_moy+pmoy_sable+pmoy_gravier+pmoy_caillou+pmoy_bloc+pmoy_rochemere+pmoy_algue_r+pmoy_algue_b+pmoy_algue_v+pmoy_coralline+pmoy_coquillage,data=table_env, distance="bray", scale=TRUE, sqrt.dsit=TRUE)
plot(dbrda_r) # enlever les amphipodes

#dbrda brute
dbrda_b<-capscale(ab_esp_b[,-19]~temp_moy+sal_moy+pmoy_sable+pmoy_gravier+pmoy_caillou+pmoy_bloc+pmoy_rochemere+pmoy_algue_r+pmoy_algue_b+pmoy_algue_v+pmoy_coralline+pmoy_coquillage,data=table_env, distance="bray", scale=TRUE, sqrt.dsit=TRUE)
plot(dbrda_b) # enlever les amphipodes


#simper


###########################
#PERMANOVA
##########################
#A-SOL------------------------
#install.packages("vegan")
# library(vegan)
# #set.seed permutation
# x_sol<-paste0(sol[,c(9:34)])
# y_sol<-paste0(sol[,c(3:7)])
# set.seed(1)
# perm_sol<-adonis2(sol[,c(3:7)]~sol[,9]+sol[,10]+sol[,11]+sol[,12]+sol[,13]+sol[,14]+sol[,15]+sol[,16]+sol[,17]+sol[,18]+sol[,19]+sol[,20]+sol[,21]+sol[,22]+sol[,23]+sol[,24]+sol[,25]+sol[,26]+sol[,27]+sol[,28]+sol[,29]+sol[,30]+sol[,31]+sol[,32]+sol[,33]+sol[,34], distance="bray")
# summary(perm_sol)
# plot(perm_sol)
# 
# ###########################################
# #B-BIOTIQUE-----------------
# perm_bio<-adonis2(bio[,c(3:7)]~bio[,9]+bio[,10]+bio[,11]+bio[,12]+bio[,13]+bio[,14]+bio[,15]+bio[,16]+bio[,17]+bio[,18]+bio[,19]+bio[,20]+bio[,21]+bio[,22]+bio[,23]+bio[,24]+bio[,25]+bio[,26]+bio[,27]+bio[,28]+bio[,29]+bio[,30]+bio[,31]+bio[,32]+bio[,33]+bio[,34], distance="bray")
# summary(perm_bio)
# plot(perm_bio)
# #ligne 11 toutes des zeros
# 
# ###########################
# #DISTANCE 
# ##########################
# #jai note distance lm mais est ce quil voulait dire hellinger ?????? pca lm existe pas !!
# #A-SOL---------------------------
# # dis_sol<-vegdist(sol[,-c(1:7,35)], method="bray")
# # db_dis_sol<-dbrda(dis_sol~g_sub+esp_tot, data=sol, dist="bray", squale=TRUE,sqrt.dsit=TRUE)
# # plot(db_dis_sol)
# #ordiplot(db_dis_sol)
# #https://www.davidzeleny.net/anadat-r/doku.php/en:rda_cca_examples
# 
# #meilleur disposition des point avec Hellinger
# dis_sol<-vegdist(sol[,-c(1:7,35)], method="hellinger")
# db_dis_sol<-dbrda(dis_sol~g_sub+esp_tot, data=sol, dist="hellinger", squale=TRUE,sqrt.dsit=TRUE)
# summary(db_dis_sol)
# plot(db_dis_sol)
# 
# #################################
# #B-BIOTIQUE---------------
# dis_bio<-vegdist(bio[,-c(1:7,35)], method="hellinger")
# db_dis_bio<-dbrda(dis_bio~b_sub+esp_tot, data=sol, dist="hellinger", squale=TRUE,sqrt.dsit=TRUE)
# summary(db_dis_bio)
# plot(db_dis_bio)
# 
# 
# 
# #########################
# #PCA 
# ########################
# #A-SOL------------------------------
# #enlever amphipode pcq tire le graphique et calcul estimation plutot que MaxN
# pca_sol<-prcomp((sol[,c(9:26,28:34)]+gradient[,c(7,8)]), center=TRUE, scale=TRUE)
# summary(pca_sol)
# str(pca_sol)
# #install.packages("ggfortify")
# #install.packages("factoextra")
# library(ggfortify)
# library(factoextra)
# fviz_pca_ind(pca_sol,habillage=sol$g_sub,col.ind="cos2",repel=TRUE)+scale_color_manual(values=c("slateblue2","orchid","chocolate1","gold","yellowgreen"))
# fviz_pca_var(pca_sol,col.var="contrib",repel=TRUE,gradient.cols=c("slateblue2","orchid","chocolate1","gold","yellowgreen"))
# #warnings pcq surperposage des etiquettes
# fviz_pca_biplot(pca_sol,label="var", habillage=sol$g_sub, col.ind=sol$g_sub,repel=TRUE)
# 
# ##########################
# #B-BIOTIQUE-----------------------
# pca_bio<-prcomp((bio[,c(9:26,28:34)]+gradient[,c(7,8)]), center=TRUE, scale=TRUE)
# summary(pca_bio)
# str(pca_bio)
# fviz_pca_ind(pca_bio,habillage=bio$b_sub,col.ind="cos2")+scale_color_manual(values=c("red4","cyan3","gold"))
# fviz_pca_var(pca_bio,col.var="contrib",gradient.cols=c("red4","cyan3","gold"))
# fviz_pca_biplot(pca_bio,label="var", habillage=bio$b_sub,addEllispses=TRUE,ellipse.level=0.95, col.ind=bio$b_sub)#too few points to calculate a ellipse
# 
