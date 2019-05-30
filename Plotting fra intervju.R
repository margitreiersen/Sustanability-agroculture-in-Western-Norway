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

#test av GLM
bakgrunn.glm<-glm(Alder~Bonde, data=resultat.intervju, family = poisson())
anova(bakgrunn.glm)
summary(bakgrunn.glm)
pred <- predict(bakgrunn.glm, newdata = data.frame(Bonde=1:50), se.fit = TRUE)
exp(pred$fit)
fit.reg <- tibble(mean=exp(pred$fit),Bonde= 1:50, up=exp(pred$fit + 1.96 * pred$se.fit),down=exp(pred$fit - 1.96 * pred$se.fit))

bakgrunn<- ggplot(data=select(resultat.intervju,Kjønn, Alder, Bonde), aes(Bonde, Alder, colour=Kjønn))+
  geom_point()+ coord_equal()+ 
  #stat_smooth(method="lm", formula=y~x, se=F, mapping = aes(group=1),show.legend = FALSE, color="black")+
  labs(y="Alder på bonden", x="Bonde i antall år") +
  geom_line(aes(Bonde, y = mean), data  = fit.reg, inherit.aes = FALSE) + 
  geom_ribbon(data = fit.reg, aes(Bonde, ymax = up, ymin = down), inherit.aes = FALSE, alpha= 0.3)



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

j.bedrift <- read_excel("jordbruksbedrifter_tot.xlsx")
j.bedrift %>% 
  gather(key = Kommune, value = Jordbruksbedrifter, -år) %>% 
  mutate(år = as.numeric(år)) %>% 
  group_by(år, Kommune) %>% 
  ggplot(aes(x=år, y= Jordbruksbedrifter)) + geom_point()
jordbruksbedrifter.sto<- j.bedrift %>% 
  gather(key = Kommune, value = Jordbruksbedrifter, -år) %>% 
  mutate(år = as.numeric(år))

areal.bedrift.sto<- jordbruksbedrifter.sto %>% 
  full_join(jordbruksareal.sto, by = c("år"="År", "Kommune"= "Kommune")) %>% 
  mutate(areal.per.bedrift = Jordbruksareal/Jordbruksbedrifter )

areal.bedrift.sto %>% 
  ggplot(aes(x=år, y= areal.per.bedrift, colour= Kommune)) + 
  geom_point() + 
  geom_smooth(method="lm", formula=y~poly(x,2), mapping = aes(group=1),show.legend = FALSE, color="black")+
  labs(y="Gjennomsnitt Areal/bonde (dkk)", x="År")
#her har jeg brukt en andregradslikning. Husk å brgrunn og sjekk ut hvordan dette kan testes for om det er det som er riktig.

areal.lm <- lm(areal.per.bedrift~år, data = areal.bedrift.sto) 
areal2.lm <- lm(areal.per.bedrift~poly(år,2), data = areal.bedrift.sto) # + I(år^2) ? Tester dette for andregrads - regresjon?
anova(areal.lm, areal2.lm, test= "F")
#Bør det brukes andre modeller?
# Bør denne vektlegges ? 

#Bør også lage en graf på endringen av jordbruksareal over tid. Her er det mest interessant å se på utviklingen i de ulike kommune opp mot hverandre.
j.areal %>% 
  gather(key = Kommune, value = Jordbruksareal, -År) %>% 
  ggplot(aes(x=År, y= Jordbruksareal, colour=Kommune)) + geom_point()


par(mfrow(2,2))
plot(areal.lm)
