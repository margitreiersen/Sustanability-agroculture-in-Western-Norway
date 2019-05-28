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

# bakgrunn.glm<-glm(Alder~Bonde, data=resultat.intervju)
# anova(bakgrunn.glm)
# summary(bakgrunn.glm)

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
