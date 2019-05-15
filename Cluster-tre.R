#Jobbing med analyse av data fra intervjuene
library(tidyverse)

#UTVALG OG INTERVJUOBJEKT (TILSVARER KOMMUNEDATA)
#skal se på tall for alle 60 utvalgte
utvalg.60<- read.csv("intervjuobjekt.csv", sep = ";")
colSums(utvalg.60>0, na.rm = TRUE)
mean(utvalg.60$fulldyrket,na.rm = TRUE) #55,78
median(utvalg.60$fulldyrket,na.rm = TRUE) #34
mean(utvalg.60$totalareal) #142,81
mean(utvalg.60$p120_hoest, na.rm = TRUE)#14,58
mean(utvalg.60$p145_vaar, na.rm = TRUE) #42,59
mean(utvalg.60$p121_hoest, na.rm = TRUE) #6,6
mean(utvalg.60$p140_hoest, na.rm = TRUE) #105

#Jobbe med produksjonstilskuddsdataene til de 29 som ble intervjuet
prod.intervju <-read.csv("intervjuobjekt 29 prod.info.csv", sep = ";",dec = ",")
colSums(prod.intervju>0, na.rm = TRUE)#Finner antall i de ulike driftsformene. 
#Gjennomsnittet av dyr i de ulike kategoriene er regnet ut i excel
median(prod.intervju$fulldyrket,na.rm = TRUE)#50


#INTERVJU-SVAR
#Laster inn alle svarene på intervjuene
library(readxl)
resultat.intervju<- read_excel("Svar på intervju.xlsx")
#resultat.intervju<- read.csv2("Svar på intervju 1.csv", sep = ";",na.strings = c("NA","Na","na"), dec = ",") 

#Plukker ut de kolonene som skal være med i clusteranalyse. 
Intervju.60.prod<-read.csv("intervjuobjekt 60 prod med navn.csv",sep = ";") #Produksjonsdata
names(resultat.intervju)
resultat.uttrekk<-resultat.intervju %>% select(-c(Dato,tot.areal,Leie,Leie.hva,Hvem.hjelper,Hva.betyr.bærekraftig.utvikling.for.deg.,Rådgivningsinstanser,Oppsøkt.råd,Netverk..Hvilke,E..annet,Energisparing,E..Strøm,E.Hvilke,Vannkilde,Utnytter.energi,Utmarksressurser,Avlinger,Artsmangfold,Innmark.Kunst.husdyr,Gjø..Husdyr.hva,Gjø..Kunst.type,Tiltak.jordsmonn,Driftsformer,Produkter.til.salgs,Selger.til,Utfordringer.for.et.fremtidsrettet.bærekraftig.landbruk.,Tiltak.for.B.på.gården,Klimapåvirkning.hvordan,Forberedelse.klima.hvilke,Tilføyelse))%>% left_join(select(Intervju.60.prod,Intervjuobjekt,fulldyrket:innmarksbeite), by="Intervjuobjekt") #Velger ut de kollonene som ikke skal være med (alt som ikke er nummer eller binære data) + legger til fulldyrka, overflatedyrke, innmarksbeite til dataene.

#dele i prediktor og respons
names(resultat.uttrekk)
respons.interessant <- resultat.uttrekk %>% 
  select (-c(Intervjuobjekt,Alder,Kommunenr,Kjønn,Pros..Jobb, Bonde,fulldyrket,overflatedyrket,innmarksbeite))%>% 
  mutate_if(is.character,factor)
#Her trekkes det som er prediktorer fra.
respons.interessant

#Lager distanse matrix med Gower
install.packages("cluster")
library(cluster)
d<-daisy(respons.interessant, metric = "gower")
d
class(d)

HCcomplit <- hclust(d, method = "ward.D",members = NULL)
plot(HCcomplit)
cutree(HCcomplit, k=3)
cutree(HCcomplit, k=4)

install.packages("vegan")
library(vegan)
PCoA1<-capscale(d~1)
PCoA1 #Denne viser variasjon som er forklart med de ulike aksene. 
plot(PCoA1)
screeplot(PCoA1, bstick = TRUE)
#scoresPCoA<-scores(PCoA1, choices = 1:2,display = "sites")
#scoresPCoA

#c(Alder,Kommunenr,Kjønn,Pros..Jobb, Bonde,fulldyrket,overflatedyrket,innmarksbeite)
#Prediktorene




#VENNDIAGRAM AV UTDANNING
#test på venndiagram til utdanningsgraf
source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
library(limma)
library(tidyverse)
install.packages("ggforce")
library(ggforce)

utdanning <- resultat.intervju %>% select (c(Landbrukutd.:Praktisk.utd.))
venn(utdanning)#virket ikke... 

#Nytt forsøk
install.packages("VennDiagram")
library(VennDiagram)
venn.plot <- draw.triple.venn(
  area1 = 76,#Praktis utdannelse
  area2 = 38, #Høyere utdannelse
  area3 = 35, #Landbruksutdannelse
  n12 = 24,
  n23 = 10,
  n13 = 21,
  n123 = 7,
  category = c("Praktisk utd.", "Høyere utd.", "Landbruks utd."),
  fill = c("blue", "red", "green"),
  lty = "blank",
  cex = 2,
  cat.cex = 1.5)
venn.plot


#OPPSUMMERING AV ANDRE SPØRSMÅL
table(resultat.intervju$FN.B.mål)
table(resultat.intervju$NordH..Bios)
table(resultat.intervju$Kommunen)
table(resultat.intervju$Del.av.nettverk)
mean(resultat.intervju$Samfunn.i.bygda)
mean(resultat.intervju$Faglig.tidskrift)
mean(resultat.intervju$Sikkerhet.vann)
mean(resultat.intervju$Utmark.Godt.utnyttet)

table(resultat.intervju$Påvirket.av.kilma)
table(resultat.intervju$Frykter.påvirkning.klima)
table(resultat.intervju$Grep.forbedredelse.klima)
