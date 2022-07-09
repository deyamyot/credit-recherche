#set working directory
setwd("E:/Audrey_Anne/Godbout_analyse/credit de recherche/gitcredit_recherche")

#######################################
#importation data

donnees_base<-read.table("data/donnees_deploiment.csv", header=T,sep=";", dec=".")[-c(33:39),-c(14:15)]
mobile<-read.table("data/MaxN_mobile.csv", header=T,sep=";",dec=".")[-33,]
peumobile<-read.table("data/decompte_peumobile.csv", header=T, sep=";", dec=".")[,-16]
recouvrement<-read.table("data/pourcentage_recouvrement.csv", header=T, sep=";",dec=".")[-147,]

colnames(donnees_base)[5] <- "coordonnee_y" 
########################################
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

temp<-load_layers(layercodes=c("BO2_tempmean_bdmin"), equalarea=FALSE, rasterstack=TRUE)
#moyenne annuelle de 2000-2014
sal<-load_layers(layercodes=c("BO2_salinitymean_bdmin"), equalarea=FALSE, rasterstack=TRUE)
#moyenne annuelle de 2000-2014

#data.frame selon les coordonnees des sites desirer
my.sites <- data.frame(Id=donnees_base$ID, Lat=donnees_base$coordonnee_y, Lon=donnees_base$coordonnee_x)

#extraction donnees pour chacun des points
my.sites.physico<- data.frame(temperature=extract(temp,my.sites[,2:3]), salinite=extract(sal,my.sites[,2:3]))

#ajout des donnees dans le document de donnees_bas
donnees_complet<-cbind(donnees_base, my.sites.physico)
colnames(donnees_complet)[14] <- "temp_moy" 
colnames(donnees_complet)[15] <- "sal_moy" 

#####################################
#manipulation des donnees


#calcul diversite
#pk est-ce que le resultat est le mm pour chacune des colonnes commme si elle 
#espece mobile
for (i in 1 :nrow(mobile)){
    mobile$nb_espece[i]<- ncol(mobile[i,-1]!=0)
    i=i+1
}

#espece peumobile
for (i in 1 :nrow(peumobile)){
  peumobile$biodiversite[i]<- rowSums(mobile[,6:15] !=0)
  i=i+1
}


#totale par espece pour une la ligne de cage
tot<-(apply(mobile[,c(2:18)], 2, sum))


#pourcentage recouvrement biotique



