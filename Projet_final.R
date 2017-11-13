#vrai projet
rm(list=objects())
install.packages("openair")
install.packages("riem")
library(openair)
library(readr)
library(riem)

#importation des données sur site AURN :

mydata<-importAURN(site="my1",year=2006,pollutant="all")
anemometre  <- read_delim("~/Desktop/MaryleboneRoadSonic2006.csv/MaryleboneRoadSonic2006-Tableau 1.csv", 
                            ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)
Traffic <- read_delim("~/Desktop/MaryleboneRoadTrafficCount2006/MaryleboneRoadTrafficCount2006-Tableau 1.csv", 
                                                       ";", escape_double = FALSE, trim_ws = TRUE)
SpeedT <- read_delim("~/Desktop/MaryleboneRoadTrafficSpeed2006/MaryleboneRoadTrafficSpeed2006-Tableau 1.csv", 
                                                       ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                                       trim_ws = TRUE)
weatherdata<- riem_measures("EGLC",date_start="2006-01-01",date_end="2006-12-31")

#Ajustage de donnée

SpeedT<-SpeedT[,c(1:3)] # Heure par heure
anemometre<-anemometre[,c(1,2,3,4)] # par quart d'heure
anemometre<-anemometre[seq(1, 35040, 4),] # on passe en heure par heure
Traffic$CLS1=Traffic$CLS1+Traffic$CLS2+Traffic$CLS3+Traffic$CLS4+Traffic$CLS5+Traffic$CLS6 #on cumule tous les types de véhicules# par heure
Traffic<-Traffic[,c(1,2,3)] # on efface les autres colonnes
weatherdata<-weatherdata[,c(1,2,6,7,11)] #Dwpf = point de rosée Temperature in Fahrenheit, typically @ 2 meters , relh: Relative Humidity in %,alti: Pressure altimeter en pouces
mydata<-mydata[,c(1,4,9)]# on prend la particule pm2.5 et pm10

# Check le nombre de Na
summary(mydata)

#on remplace les NA par la moyenne pour les particules (Y) :
mydata$pm10[which(is.na(mydata$pm10))]=mean(mydata$pm10[!is.na(mydata$pm10)])
mydata$pm2.5[which(is.na(mydata$pm2.5))]=mean(mydata$pm2.5[!is.na(mydata$pm2.5)])

summary(anemometre)

#on remplace les NA par la moyennes dans les variables anenometre (vitesse du vent, direction du vent et temperature) :
#1800 NA a la base 

anemometre$`WindSpeed(m/s)`[which(is.na(anemometre$`WindSpeed(m/s)`))]=mean(anemometre$`WindSpeed(m/s)`[!is.na(anemometre$`WindSpeed(m/s)`)])
anemometre$`WindDirection(deg)`[which(is.na(anemometre$`WindDirection(deg)`))]=mean(anemometre$`WindDirection(deg)`[!is.na(anemometre$`WindDirection(deg)`)])
anemometre$`Temperature (degC)`[which(is.na(anemometre$`Temperature (degC)`))]=mean(anemometre$`Temperature (degC)`[!is.na(anemometre$`Temperature (degC)`)]) 


# On remplace les Na pour la densité du traffic :
summary(Traffic)
Traffic$CLS1[which(is.na(Traffic$CLS1))]=mean(Traffic$CLS1[!is.na(Traffic$CLS1)])
Traffic<-Traffic$CLS1[which(Traffic$Lane=="MG1")]

# On remplace les Na pour la vitesse du traffic :
summary(SpeedT)
SpeedT$`AverageSpeed (km/h)`[which(is.na(SpeedT$`AverageSpeed (km/h)`))]=mean(SpeedT$`AverageSpeed (km/h)`[!is.na(SpeedT$`AverageSpeed (km/h)`)])
SpeedT<-SpeedT$`AverageSpeed (km/h)`[which(SpeedT$Lane=="MG1")] # Il manque le mois d'aout et septembre , 5 premier jour d'avril jusqu'au 6 eme a 16h(non inclus),13 au 20 decembre



#Interpolation de Weather data
summary(weatherdata)
head(weatherdata)


par(mfrow=c(3,1))
plot(weatherdata$valid,weatherdata$dwpf,col="red",type="l")
plot(weatherdata$valid,weatherdata$relh,col="blue",type="l")
plot(weatherdata$valid,weatherdata$alti,col="green",type="l")
#NOA : ce que le prof a dit mais je sais plus pk :)


#Point de Rosée (Fareneight) :
#on prend des points equiréparties entre 11h20 du 1 er janvier et le 31 decembre minuit (365*24*6-11*6-2-11*6-1)
l=approx(as.POSIXct(weatherdata$valid),weatherdata$dwpf,method="linear",n=52425)

a=mean(weatherdata$dwpf[1:10])
b=mean(weatherdata$dwpf[9445:9453])# moyenne locale
RoseP<-numeric()
Z<-numeric()
W<-numeric()
W[1:12]=a
Z[1:11]=b
RoseP=c(W,l$y[seq(5,52425,6)],Z) # le vecteur des points de rosée fini

# %d'humudité :
l2=approx(as.POSIXct(weatherdata$valid),weatherdata$relh,method="linear",n=52425)
a2=mean(weatherdata$relh[1:10])
b2=mean(weatherdata$relh[9445:9453])

Humi<-numeric()
Z2<-numeric()
W2<-numeric()
W2[1:12]=a2
Z2[1:11]=b2
Humi=c(W2,l2$y[seq(5,52425,6)],Z2) # Vecteur fini de l'humidité

# Pressure Altimetre
l3=approx(as.POSIXct(weatherdata$valid),weatherdata$alti,method="linear",n=52425)
a3=mean(weatherdata$alti[1:10])
b3=mean(weatherdata$alti[9445:9453])

Pression<-numeric()
Z3<-numeric()
W3<-numeric()
W3[1:12]=a3
Z3[1:11]=b3
Pression=c(W3,l3$y[seq(5,52425,6)],Z3)


#Création du dataframe nettoyé :
PP<-data.frame(anemometre$`Date&Time`,mydata$pm2.5,mydata$pm10,anemometre$`WindSpeed(m/s)`,anemometre$`WindDirection(deg)`,anemometre$`Temperature (degC)`,Pression,Humi,RoseP)
names(PP)<-c("Dates/heures(UTC/GMT )","particule pm2.5 (Y1)","particule pm10(Y2)","WVitesse(X1)","WDirection(X2)","Temperature(X3)","Pression(X4)","humidité(X5)","Point de rosée(X6)")
