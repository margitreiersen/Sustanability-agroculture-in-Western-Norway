#Meir statistikk må tydeligvis til i masteren. 
#Cluster av produksjonsdataene
library(readr)
library(tidyverse)
p2018<-read.csv("2018.csv", sep = ";")
p2018<-p2018 %>% filter(kommunenr %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411))
names(p2018)
p2018_respons <- p2018 %>% select(-c(soeknads_aar,saksbehandlende_kommune,orgnr,orgnavn,gaardsnummer,bruksnummer,festenummer))

library(vegan)
dist_p2018 <- vegdist(p2018_respons, method = "manhattan")
dist_p2018

HCcomplit <- hclust(dist_p2018, method = "complete", members = NULL)
plot(HCcomplit)


#turnover gjennom år av bønder i NH
p2013aug <- read_csv2("2013aug.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411)) %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune))  

p2015jan<- read_csv2("2015jan.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411))%>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 
  
p2014jan<- read_csv2("2014jan.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411))%>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 
  
p2014aug<- read_csv2("2014aug.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411))%>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 
  
p2016jan <- read_csv2("2016jan.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411)) %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 

read_csv2("2016aug.csv")%>% filter(is.na(as.numeric(kommune)))

p2016aug<- read_csv2("2016aug.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411)) %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune))
  
p2017jan <- read_csv2("2017jan.csv") %>% 
  filter(kommunenummer %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411)) %>% 
  select(kommune=kommunenummer, orgnr=organisasjonsnummer) %>% mutate(kommune = as.integer(kommune))

p2018.tb<- p2018 %>% as_tibble() %>% select(kommune=kommunenr, orgnr)

org.y<- bind_rows(`2013` = p2013aug, 
          `2014` = p2014jan,
          `2015` = p2015jan,
          `2016` = p2016jan,
          `2017` = p2017jan, 
          `2018` = p2018.tb,
          .id = "year") %>% 
  mutate(year = as.integer(year), orgnr = as.integer(orgnr))

install.packages("codyn")
library(codyn)
org.y<- org.y %>% mutate(abundance = 1)
app<- turnover(org.y, 
         time.var = "year",
         species.var = "orgnr",
         abundance.var = "abundance",
         metric = "appearance")
ggplot(app, aes(x=year, y=appearance))+ geom_point() + geom_line()
disapp<- turnover(org.y, 
         time.var = "year",
         species.var = "orgnr",
         abundance.var = "abundance",
         metric = "disappearance")
ggplot(disapp, aes(x=year, y=disappearance))+ geom_point() + geom_line()
turn.gg<- bind_rows(Appearance = app %>% rename(app_disapp= appearance),
          Disapperance = disapp %>% rename(app_disapp= disappearance),
          .id = "turnover")
ggplot(turn.gg, aes(x=year, y=app_disapp, color= turnover))+ geom_point() + geom_line()

org.y %>% group_by(year) %>% count()
f<-setdiff(p2018$orgnr, p2017jan$orgnr)
p2018 %>% as_tibble() %>%  mutate(nybonde = orgnr %in% f ) %>% ggplot(aes(x= nybonde, y= fulldyrket)) + geom_boxplot()

#Tester for hele landet ####
p2013augN <- read_csv2("2013aug.csv") %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune))  
p2015janN<- read_csv2("2015jan.csv") %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 
p2014janN<- read_csv2("2014jan.csv") %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 
p2016janN <- read_csv2("2016jan.csv") %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 
p2017janN <- read_csv2("2017jan.csv") %>% 
  select(kommune=kommunenummer, orgnr=organisasjonsnummer) %>% mutate(kommune = as.integer(kommune))

p2018N.tb<- read.csv("2018.csv", sep = ";") %>% as_tibble() %>% select(kommune=kommunenr, orgnr)

orgN.y<- bind_rows(`2013` = p2013augN, 
                  `2014` = p2014janN,
                  `2015` = p2015janN,
                  `2016` = p2016janN,
                  `2017` = p2017janN, 
                  `2018` = p2018N.tb,
                  .id = "year") %>% 
  mutate(year = as.integer(year), orgnr = as.integer(orgnr))

orgN.y %>% group_by(year) %>% count()


orgN.y<- orgN.y %>% mutate(abundance = 1)
appN<- turnover(orgN.y, 
               time.var = "year",
               species.var = "orgnr",
               abundance.var = "abundance",
               metric = "appearance")
ggplot(appN, aes(x=year, y=appearance))+ geom_point() + geom_line()
disappN<- turnover(orgN.y, 
                  time.var = "year",
                  species.var = "orgnr",
                  abundance.var = "abundance",
                  metric = "disappearance")
ggplot(disappN, aes(x=year, y=disappearance))+ geom_point() + geom_line()
turnN.gg<- bind_rows(Appearance = appN %>% rename(app_disapp= appearance),
                    Disapperance = disappN %>% rename(app_disapp= disappearance),
                    .id = "turnover")
ggplot(turnN.gg, aes(x=year, y=app_disapp, color= turnover))+ geom_point() + geom_line()
