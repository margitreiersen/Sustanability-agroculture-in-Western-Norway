#Informasjon fra produksjonstilskudd
#Biosfæreområdet 11 kommuner####
bios.kommuner #860 bønder totalt
mean(bios.kommuner$fulldyrket,na.rm = TRUE) #55,1
mean(bios.kommuner$totalareal) #155,0
mean(bios.kommuner$p120_hoest, na.rm = TRUE)#18,6
bios.kommuner$p120_hoest # Finn måte å regne ut dette på.
mean(bios.kommuner$p145_vaar, na.rm = TRUE) #45,9
bios.kommuner$p145_vaar #Finn måte å regne ut dette på.
median(bios.kommuner$fulldyrket,na.rm = TRUE) #34
colSums(bios.kommuner>0, na.rm = TRUE) # YES! fant den som gir hvor mange det er i hver kolonne! :D 

#Austrheim ####
Austerheim <- subset(utvalg2018, kommunenr==1264)#30 Bønder
mean(Austerheim$fulldyrket,na.rm = TRUE) #77,3
mean(Austerheim$totalareal) #171,4
mean(Austerheim$p120_hoest, na.rm = TRUE)#14
Austerheim$p120_hoest #Antall 3
mean(Austerheim$p145_vaar, na.rm = TRUE) #51,6
Austerheim$p145_vaar #Antall 23
mean(Austerheim$p121_hoest, na.rm = TRUE) #24,8
median(Austerheim$fulldyrket,na.rm = TRUE)#48,5

#Fedje ####
Fedje <- subset(utvalg2018, kommunenr==1265) # 1 bonde
mean(Fedje$fulldyrket,na.rm = TRUE) #13
mean(Fedje$totalareal) #230
mean(Fedje$p120_hoest, na.rm = TRUE)#0
Fedje$p120_hoest #Antall 0
mean(Fedje$p145_vaar, na.rm = TRUE) #153
Fedje$p145_vaar #Antall 1
median(Fedje$fulldyrket,na.rm = TRUE)

#Radøy ####
Radoy<- subset(utvalg2018, kommunenr==1260) #157 Bønder
mean(Radoy$fulldyrket,na.rm = TRUE) #37,4
mean(Radoy$totalareal) #169,7
mean(Radoy$p120_hoest, na.rm = TRUE)#22,4
Radoy$p120_hoest #Antall 25
mean(Radoy$p145_vaar, na.rm = TRUE) #50,9
Radoy$p145_vaar #Antall 108
median(Radoy$fulldyrket,na.rm = TRUE)

#Lindås ####
Lindaas<- subset(utvalg2018, kommunenr==1263) #234 bønder
mean(Lindaas$fulldyrket,na.rm = TRUE) #44,9
mean(Lindaas$totalareal) #147,8
mean(Lindaas$p120_hoest, na.rm = TRUE)#15,9
Lindaas$p120_hoest #Antall 24
mean(Lindaas$p145_vaar, na.rm = TRUE) #46,6
Lindaas$p145_vaar #Antall 180
median(Lindaas$fulldyrket,na.rm = TRUE)

#Gulen ####
Gulen <-subset(utvalg2018, kommunenr==1411) #96 bønder
mean(Gulen$fulldyrket,na.rm = TRUE) #79,2
mean(Gulen$totalareal) #186,0
mean(Gulen$p120_hoest, na.rm = TRUE)#18,5
Gulen$p120_hoest #Antall 37
mean(Gulen$p145_vaar, na.rm = TRUE) #53,5
Gulen$p145_vaar #Antall 44
median(Gulen$fulldyrket,na.rm = TRUE)#67,5

#Meland ####
Meland <-subset(utvalg2018, kommunenr==1256) #69 bønder
mean(Meland$fulldyrket,na.rm = TRUE) #55,8
mean(Meland$totalareal) #138,3
mean(Meland$p120_hoest, na.rm = TRUE)#14,4
Meland$p120_hoest #Antall 11
mean(Meland$p145_vaar, na.rm = TRUE) #34,7
Meland$p145_vaar #Antall 50
median(Meland$fulldyrket,na.rm = TRUE)#27

#Masfjorden ####
Masfjorden <- subset(utvalg2018, kommunenr==1266)#70 bønder
mean(Masfjorden$fulldyrket,na.rm = TRUE) #53,8
mean(Masfjorden$totalareal) #102,3
mean(Masfjorden$p120_hoest, na.rm = TRUE)#14,7
Masfjorden$p120_hoest #Antall 10
mean(Masfjorden$p145_vaar, na.rm = TRUE) #47,7
Masfjorden$p145_vaar #Antall 58
median(Masfjorden$fulldyrket,na.rm = TRUE)#35

#Øygarden
Oygarden <-subset(utvalg2018, kommunenr==1259) #23 bønder
mean(Oygarden$fulldyrket,na.rm = TRUE) #31,2
mean(Oygarden$totalareal) #116,8
mean(Oygarden$p120_hoest, na.rm = TRUE)#11,5
Oygarden$p120_hoest #Antall 2
mean(Oygarden$p145_vaar, na.rm = TRUE) #72,6
Oygarden$p145_vaar #Antall 21
median(Oygarden$fulldyrket,na.rm = TRUE)#16

#Vaksdal ####
Vaksdal <- subset(utvalg2018, kommunenr==1251)#48 bønder
mean(Vaksdal$fulldyrket,na.rm = TRUE) #85,7
mean(Vaksdal$totalareal) #131,6
mean(Vaksdal$p120_hoest, na.rm = TRUE)#20,6
Vaksdal$p120_hoest #Antall 14
mean(Vaksdal$p145_vaar, na.rm = TRUE) #43,9
Vaksdal$p145_vaar #Antall 37
median(Vaksdal$fulldyrket,na.rm = TRUE)#50,5

#Osterøy ####
Osteroy<- subset(utvalg2018, kommunenr==1253)#121 bønder
mean(Osteroy$fulldyrket,na.rm = TRUE) #61,9
mean(Osteroy$totalareal) #176,1
mean(Osteroy$p120_hoest, na.rm = TRUE)#21,9
Osteroy$p120_hoest #Antall 21
mean(Osteroy$p145_vaar, na.rm = TRUE) #30,0
Osteroy$p145_vaar #Antall 86
median(Osteroy$fulldyrket,na.rm = TRUE)#36

#Modalen ####
Modalen<- subset(utvalg2018, kommunenr==1252)#11 bønder
mean(Modalen$fulldyrket,na.rm = TRUE) #103,1
mean(Modalen$totalareal) #164,6
mean(Modalen$p120_hoest, na.rm = TRUE)#12
Modalen$p120_hoest #Antall 2
mean(Modalen$p145_vaar, na.rm = TRUE) #57,4
Modalen$p145_vaar #Antall 5
colSums(Modalen>0, na.rm = TRUE)
median(Modalen$fulldyrket,na.rm = TRUE)#87

