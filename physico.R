#instalation package
install.packages("sdmpredictors")
install.packages("leaflet")

#lecture library
library(sdmpredictors)
library(leaflet)


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
physico<-load_layers(layercodes=c("BO2_tempmean_bdmin","BO2_salinitymean_bdmin"), equalarea=FALSE, rasterstack=TRUE)

#data.frame selon les coordonnees des sites desirer
my.sites <- data.frame( ID=c(donnees_base$ID[1:32]), Lon=c(donnees_base$coordonnee_x[1:32]) , Lat=c(donnees_base$coordonnee_y[1:32]) )
my.sites

#extraction donnees pour chacun des points
my.sites.physico <- data.frame(temp.sal=extract(physico,my.sites[,2:3]))
my.sites.physico  

#ajout des donnees dans le document de donnees_bas
donnees_complet<-cbind(donnees_base, my.sites.physico)
donnees_complet


