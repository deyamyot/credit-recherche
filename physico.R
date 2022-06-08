#instalation package
install.packages("sdmpredictors")
install.packages("leaflet")

#lecture library
library(sdmpredictors)
library(leaflet)


#extraction de donnees de bio-oracle
layers_physico<-list_layers(dataset="Bio-ORACLE")

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

temp<-load_layers(BO2_tempmean_bdmin)
physico<-load_layers(layercodes=c("BO2_tempmean_bdmin","BO2_salinitymean_bdmin"), equalarea=FALSE, rasterstack=TRUE)
