# Margit Reiersen
# Master våren 2019
# Utvalg av bønder til intervju. ####
utvalg2018<-read.csv("2018.csv", sep = ";")
install.packages("tidyverse")
library(tidyverse)
set.seed(42) #Starter fra samme sted når den finner randomme.

utvalg2018$kommunenr <- as.factor(utvalg2018$kommunenr) #Gjør kommunenr kategoriske.
#Velge ut de akktuelle kommunene:
bios.kommuner <- utvalg2018 %>% filter(kommunenr %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411))

#Velge ut de 30+30 Intervjuobjektene.
intervju<-sample_n(bios.kommuner, 60)                   
#test<- sample_n(bios.kommuner, 10) #Test for å se om de blir like som de 10 første. Negativt.                                   

write.csv(intervju, file = "intervjuobjekt.csv") #Lagre det nye datasettet
write.csv(bios.kommuner, file = "860 kommuner.csv")

#bønder i de ulike kommunene ####
Austerheim <- subset(utvalg2018, kommunenr==1264)#30 Bønder
Fedje <- subset(utvalg2018, kommunenr==1265) # 1 bonde
Radoy<- subset(utvalg2018, kommunenr==1260) #157 Bønder
Lindaas<- subset(utvalg2018, kommunenr==1263) #234 bønder
Gulen <-subset(utvalg2018, kommunenr==1411) #96 bønder
Meland <-subset(utvalg2018, kommunenr==1256) #69 bønder
Masfjorden <- subset(utvalg2018, kommunenr==1266)#70 bønder
Oygarden <-subset(utvalg2018, kommunenr==1259) #23 bønder
Vaksdal <- subset(utvalg2018, kommunenr==1251)#48 bønder
Osteroy<- subset(utvalg2018, kommunenr==1253)#121 bønder
Modalen<- subset(utvalg2018, kommunenr==1252)#11 bønder


citation()
citation(package = "tidyverse")