#Meir statistikk må tydeligvis til i masteren. 
#Cluster av produksjonsdataene
library(readr)
library(tidyverse)
p2018<-read.csv("2018.csv", sep = ";")
p2018<-p2018 %>% filter(kommunenr %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411))
names(p2018)

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
  
p2016jan <- read_csv2("2016jan.csv") %>% 
  filter(kommune %in% c(1264, 1265,1260, 1263, 1256, 1266, 1259, 1251, 1252, 1253, 1411)) %>% 
  select(kommune, orgnr) %>% mutate(kommune = as.integer(kommune)) 

read_csv2("2016aug.csv")%>% filter(is.na(as.numeric(kommune)))
  
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
#ggplot(app, aes(x=year, y=appearance))+ geom_point() + geom_line()
disapp<- turnover(org.y, 
         time.var = "year",
         species.var = "orgnr",
         abundance.var = "abundance",
         metric = "disappearance")
#ggplot(disapp, aes(x=year, y=disappearance))+ geom_point() + geom_line()
turn.gg<- bind_rows(Appearance = app %>% rename(app_disapp= appearance),
          Disapperance = disapp %>% rename(app_disapp= disappearance),
          .id = "turnover")
ggplot(turn.gg, aes(x=year, y=app_disapp, color= turnover))+ 
  geom_point() + 
  geom_line()+ 
  labs(y= "Turnover tall", x= "År", colour = "Endring")

#Teste hva som er forskjellen på nybonde i 2018 og de som også søkte i 2017
org.y %>% group_by(year) %>% count()
f<-setdiff(p2018$orgnr, p2017jan$orgnr)
ny.bonde.2018<-p2018 %>% as_tibble() %>%  mutate(nybonde = orgnr %in% f )
ggplot(ny.bonde.2018, aes(x= nybonde, y= fulldyrket)) + 
  geom_boxplot() + 
  labs(y= "Fulldyrket", x = "Ny bonde 2018")
ggplot(ny.bonde.2018, aes(x= nybonde, y= totalareal)) + 
  geom_boxplot() +
  labs(y= "Totalareal innmark", x = "Ny bonde 2018")

library(nlme)
s.lme <- lme(fulldyrket~nybonde, data= ny.bonde.2018, random = ~1|kommunenr)
anova(s.lme) #ikke signifikant forskjell med Fulldyrket
s2.lme <- lme(totalareal~nybonde, data= ny.bonde.2018, random = ~1|kommunenr)
anova(s2.lme) #signifikant :-) 
summary(s2.lme)


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
#ggplot(appN, aes(x=year, y=appearance))+ geom_point() + geom_line()
disappN<- turnover(orgN.y, 
                  time.var = "year",
                  species.var = "orgnr",
                  abundance.var = "abundance",
                  metric = "disappearance")
#ggplot(disappN, aes(x=year, y=disappearance))+ geom_point() + geom_line()
turnN.gg<- bind_rows(Appearance = appN %>% rename(app_disapp= appearance),
                    Disapperance = disappN %>% rename(app_disapp= disappearance),
                    .id = "turnover")
ggplot(turnN.gg, aes(x=year, y=app_disapp, color= turnover))+ geom_point() + geom_line()+ labs(color= "Endring", y= "Turnover nummer", x= "År")

#Prøve å plotte hvordan utvikklingen av antall bønder har vært i hele landet 1999-2018
j.bedrift.N<- read_excel("Jordbruksbedrifter-2.xlsx", 2) %>% 
  gather(key = year, value = Jordbruksbedrifter, -kommune) %>% 
  mutate(year = as.integer(year), Jordbruksbedrifter = as.integer(Jordbruksbedrifter))

#j.bedrift.N %>% group_by(year) %>% summarise(sum=sum(!is.na(Jordbruksbedrifter)))

j.bedrift.N %>% ggplot(aes(x=year, y= Jordbruksbedrifter)) + geom_point() + labs(x= "År", y= "Jordbruksbedrifter") #+ geom_line() #denne kan ha problemer med datamengden
