#Jobbing med analyse av data fra intervjuene
library(tidyverse)

#UTVALG OG INTERVJUOBJEKT (TILSVARER KOMMUNEDATA)####
#skal se på tall for alle 60 utvalgte
utvalg.60 <- read.csv("intervjuobjekt.csv", sep = ",")
utvalg.60<- as_tibble(utvalg.60)
utvalg.60
colSums(utvalg.60 > 0, na.rm = TRUE) #HER ER DET ET ELLER ANNET. ER KOLLONENE DEFINERT RETT? FAKTOR OL.?
mean(utvalg.60$fulldyrket, na.rm = TRUE) #55,78
median(utvalg.60$fulldyrket, na.rm = TRUE) #34
mean(utvalg.60$totalareal) #142,81
mean(utvalg.60$p120_hoest, na.rm = TRUE)#14,58
mean(utvalg.60$p145_vaar, na.rm = TRUE) #42,59
mean(utvalg.60$p121_hoest, na.rm = TRUE) #6,6
mean(utvalg.60$p140_hoest, na.rm = TRUE) #105

#Jobbe med produksjonstilskuddsdataene til de 29 som ble intervjuet
prod.intervju <-
  read.csv("intervjuobjekt 29 prod.info.csv",
           sep = ";",
           dec = ",")
prod.intervju<- as_tibble(prod.intervju)
prod.intervju

colSums(prod.intervju > 0, na.rm = TRUE)#Finner antall i de ulike driftsformene.
#Gjennomsnittet av dyr i de ulike kategoriene er regnet ut i excel ->DETTE MÅ KANSKJE STÅ I R ?
median(prod.intervju$fulldyrket, na.rm = TRUE)#50

#INTERVJU-SVAR Ordinasjon ####
#Laster inn alle svarene på intervjuene
library(readxl)
resultat.intervju <- read_excel("Svar på intervju.xlsx")

#Plukker ut de kolonene som skal være med i ordinasjon og clusteranalyse. ####
Intervju.60.prod <-
  read.csv("intervjuobjekt 60 prod med navn.csv", sep = ";") #Produksjonsdata
names(resultat.intervju)
resultat.uttrekk <-
  resultat.intervju %>% select(
    -c(
      Dato,
      tot.areal,
      Leie,
      Leie.hva,
      Hvem.hjelper,
      Hva.betyr.bærekraftig.utvikling.for.deg.,
      Rådgivningsinstanser,
      Oppsøkt.råd,
      Netverk..Hvilke,
      E..annet,
      Energisparing,
      E..Strøm,
      E.Hvilke,
      Vannkilde,
      Utnytter.energi,
      Utmarksressurser,
      Avlinger,
      Artsmangfold,
      Innmark.Kunst.husdyr,
      Gjø..Husdyr.hva,
      Gjø..Kunst.type,
      Tiltak.jordsmonn,
      Driftsformer,
      Produkter.til.salgs,
      Selger.til,
      Utfordringer.for.et.fremtidsrettet.bærekraftig.landbruk.,
      Tiltak.for.B.på.gården,
      Klimapåvirkning.hvordan,
      Forberedelse.klima.hvilke,
      Tilføyelse
    )
  ) %>% left_join(select(Intervju.60.prod, Intervjuobjekt, fulldyrket:innmarksbeite),
                  by = "Intervjuobjekt") #Velger ut de kollonene som ikke skal være med (alt som ikke er nummer eller binære data) + legger til fulldyrka, overflatedyrke, innmarksbeite til dataene.


#dele i prediktor og respons####
names(resultat.uttrekk)
respons.interessant <- resultat.uttrekk %>%
  select (
    -c(
      Intervjuobjekt,
      Alder,
      Kommunenr,
      Kjønn,
      Pros..Jobb,
      Bonde,
      fulldyrket,
      overflatedyrket,
      innmarksbeite
    )
  ) %>%
  mutate_if(is.character, factor)
#Her trekkes det som er prediktorer fra.
respons.interessant

#Lager distanse matrix med Gower
install.packages("cluster")
library(cluster)
d <- daisy(respons.interessant, metric = "gower")
d
class(d)

HCcomplit <- hclust(d, method = "ward.D", members = NULL)
plot(HCcomplit)
rect.hclust(HCcomplit, k = 3)
cluster.3 <-cutree(HCcomplit, k = 3) #velger 3 cluster
#cutree(HCcomplit, k = 4)
#cutree(HCcomplit, k = 5)

install.packages("vegan")
library(vegan)
PCoA1 <- capscale(d ~ 1)
PCoA1 #Denne viser variasjon som er forklart med de ulike aksene.
screeplot(PCoA1, bstick = FALSE) #velger å bruke de to første. Ikke en klar brudd mellom støy og klar variasjon
plot(PCoA1)

col <- c("red2", "green4", "mediumblue")
col[cluster.3]
#https://stackoverflow.com/questions/12436902/overlaying-clustering-results-on-an-ordination

#Sett inn cluster TO DO
#chull -> linje rundt. 



#Prediktor
Prediktor.interesant<-select(resultat.uttrekk,Alder,Kommunenr,Kjønn,Pros..Jobb, Bonde,fulldyrket,overflatedyrket,innmarksbeite)%>%mutate(Kommunenr=factor(Kommunenr))

set.seed(568)
efit <- envfit(PCoA1, Prediktor.interesant, permutations = 999)
efit
plot(PCoA1)#baseplot
plot(efit)#everything
plot(efit, p.max = .05, col = "green") #just significant ones
install.packages("ggvegan") #ikke tilgjengelig... 
 
#Til spiderweb diagram ####
library(tidyverse)
spider.df<- select(resultat.uttrekk, Drive.med.matproduksjon:lokal.matproduksjon.Vestlandet,B.viktighet) #6 kolonner.
#Stolt.bonde og Respekt.som.bonde er binære kategoriske. gjøre de om til nei=0 og ja=1
spider.df<-as_tibble(spider.df)
spider.df


spider.df <-
  spider.df %>% mutate(
    Stolt.bonde = recode(Stolt.bonde, "ja" = 6, "nei" = 0),
    Respekt.som.bonde = recode(Respekt.som.bonde, "ja" = 6, "nei" = 0)) %>% 
  rename(
    `Stolt som bonde` = Stolt.bonde,
    `Drive med matproduksjon` = Drive.med.matproduksjon,
    `Viktigheten av bærekraft` = B.viktighet,
    `Lokal matproduksjon på Vestlandet` = lokal.matproduksjon.Vestlandet,
    `Følt respekt som bonde` = Respekt.som.bonde
  )
spider.df

spider.mean.df <- spider.df %>% 
  rownames_to_column() %>%  
  gather(key = variable, value = measurment,-rowname) %>% 
  group_by(variable) %>% 
  summarise(mean = mean(measurment))
spider.mean.df
#inn her ska eg vist prøve ggradar. for at det skal se penere ut
#installed.packages("ggradar")
#library(ggradar)

spid <-
  ggplot(spider.mean.df, aes(x = variable, y = mean, group = 1)) + 
  geom_point(size=2) + 
  geom_line(colour= "green1") + 
  coord_polar(start = pi) + 
  annotate("text", x= rep(0.5,3), y= c(2,4,6), label = c("2", "4","6" ))+
  ylim(0,6) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title = element_blank())
spid

library(grid)
gt <- ggplot_gtable(ggplot_build(spid))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

#VENNDIAGRAM AV UTDANNING ####
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


#OPPSUMMERING AV ANDRE SPØRSMÅL ####
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
