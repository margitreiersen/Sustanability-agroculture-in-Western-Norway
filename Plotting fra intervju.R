library(readxl)
library(nlme)
Driftsformer.df <- read_excel("Driftsformer.xlsx")
library(tidyverse)
Driftsformer.df<-Driftsformer.df %>% arrange(Kategori,desc(Antall)) %>% mutate(n=1:n()) %>% mutate(Driftsform=reorder(Driftsform,n))
p1 <- ggplot(Driftsformer.df, aes(Driftsform,fill=Kategori, y=Antall) )
p1 <- p1+  geom_col() + coord_flip()
p1<- p1 + theme(axis.title = element_blank())
p1

resultat.intervju <- read_excel("Svar på intervju.xlsx")
# PLOTTING AV ALDER, BONDE, OG KJØNN ####
bakgrunn<- ggplot(data=select(resultat.intervju,Kjønn, Alder, Bonde), aes(Bonde, Alder, colour=Kjønn))
bakgrunn<- bakgrunn + geom_point()+ coord_equal()
bakgrunn <- bakgrunn + stat_smooth(method="lm", formula=y~x, se=F, mapping = aes(group=1),show.legend = FALSE, color="black")
bakgrunn <- bakgrunn + labs(y="Alder på bonden", x="Bonde i antall år")
bakgrunn

#Reggresjon av linjen
bakgrunn.lm<-lm(Alder~Bonde, data=resultat.intervju)
anova(bakgrunn.lm)
summary(bakgrunn.lm) #y=0.73x+37.17 , p-verdi 2,58e-07
par(mfrow=c(2,2))
plot(bakgrunn.lm)

#Plotte Belastning på helse, yrke bonde og kjønn ####
Belastning<- ggplot(data=select(resultat.intervju,Kjønn, Bonde, Belastning.helse), aes(Bonde, Belastning.helse,colour=Kjønn) )
Belastning<- Belastning + geom_jitter(height = 0.1)+geom_smooth(method="lm", formula=y~x, se=F)
Belastning <- Belastning + labs(x="Jobbet som bonde (år)", y="Belastning på helsen")
Belastning
#Plotte Belastning på helse, alder og kjønn
Belastning<- ggplot(data=select(resultat.intervju,Kjønn, Alder, Belastning.helse), aes(Alder, Belastning.helse,colour=Kjønn) )
Belastning<- Belastning + geom_jitter()+geom_smooth(method="lm", formula=y~x, se=F)
Belastning <- Belastning + labs(x="Alder på bonden", y="Belastning på helsen")
Belastning

Belastning<- ggplot(data=select(resultat.intervju,Kjønn, Alder, Belastning.helse), aes(Alder, Belastning.helse) )
Belastning<- Belastning + geom_jitter()+geom_smooth(method="lm", formula=y~x, se=F)
Belastning <- Belastning + labs(x="Alder på bonden", y="Belastning på helsen")
Belastning

#LAGER GRAFTER TIL SPØRSMÅLENE
#Leieaktører ####
Leie<- ggplot(data=select(resultat.intervju,Aktører), aes(Aktører) )
Leie<- Leie + geom_bar(aes(Aktører))
Leie<- Leie + labs(x= "Leieaktører (antall jordeiere)", y= "Antall bønder")
Leie

#gård i drift ####
Drift<- ggplot(data=select(resultat.intervju,Lenge.gården.i.drift), aes(Lenge.gården.i.drift) )
Drift<- Drift + geom_histogram(binwidth = 10) #BLIR DENNE RETT Å BRUKE? ELLER ER GEOM-BAR BEDRE?
Drift<- Drift + labs(x= "Antatt drift framover (år)", y= "Antall") 
Drift

#Jordbruksbedrifter og jordbrukseiendom ####
library(readxl)
j.areal<- read_excel("jordbruksareal_ tot.xlsx")
jordbruksareal.sto<- j.areal %>% 
  gather(key = Kommune, value = Jordbruksareal, -År) %>% 
  mutate(År = as.numeric(År))
j.areal %>% mutate(År = as.numeric(År)) %>% 
  gather(key = Kommune, value = Jordbruksareal, -År) %>% 
  ggplot(aes(x=År, y= Jordbruksareal, colour=Kommune)) + geom_point() + geom_line() + labs(y= "Jordbruksareal (dkk)")

j.bedrift <- read_excel("jordbruksbedrifter_tot.xlsx")
jordbruksbedrifter.sto<- j.bedrift %>% 
  gather(key = Kommune, value = Jordbruksbedrifter, -år) %>% 
  mutate(år = as.numeric(år))  
jordbruksbedrifter.sto %>% 
  ggplot(aes(x=år, y= Jordbruksbedrifter, colour=Kommune)) + geom_point() +geom_line() + labs(x= "År")


areal.bedrift.sto<- jordbruksbedrifter.sto %>% 
  full_join(jordbruksareal.sto, by = c("år"="År", "Kommune"= "Kommune")) %>% 
  mutate(areal.per.bedrift = Jordbruksareal/Jordbruksbedrifter )

areal.bedrift.sto %>% 
  ggplot(aes(x=år, y= areal.per.bedrift, colour= Kommune)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y~poly(x,2), se = F, mapping = aes(group=1), show.legend = FALSE, color="black")+
  labs(y="Gjennomsnitt Areal/bonde (dkk)", x="År")


library(nlme)
areal.lme <- lme(areal.per.bedrift~år, data = areal.bedrift.sto, random = ~1|Kommune, na.action = na.omit, method = "ML") 
areal2.lme <- lme(areal.per.bedrift~poly(år,2, raw= TRUE), data = areal.bedrift.sto, random = ~1|Kommune, na.action = na.omit, method = "ML") 
areal3.lme <- lme(areal.per.bedrift~poly(år,2, raw= TRUE), data = areal.bedrift.sto, random = ~1|Kommune, na.action = na.omit, method = "ML", correlation = corAR1())

anova(areal.lme, areal2.lme, areal3.lme)
anova(areal3.lme) #hva betyr <.0001
summary(areal3.lme) #masse advarsler

anova(areal.lme)




