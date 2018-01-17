#vrai projet

#Il faut trouver des valeur de traffic heures par heures ou journalier suffit


rm(list=objects())
install.packages("openair")
install.packages("riem")
install.packages("yarrr")
install.packages("zoo")
library(zoo)

library(mgcv)
library(mgcViz)
library(tidyverse)
library(caret)
library(openair)
library(readr)
library(riem)
library(gridExtra)
library(yarrr)

#librari des RF :
library(ranger)
library(rpart)
library(tree)
library(plotmo)
library(rpart.plot)
library(caret)
library(party)
library(randomForest)
library(Rborist)
library(magrittr)
library(MASS)


#importation des données sur site AURN :

# On importe les polluants du comté de Lewisham :
mydata<-importAURN(site="LW1",year=c(2002,2003,2004,2005,2006),pollutant="o3") 

#On importe les données de l'aéroport de Londres (2-3 km au nord est de LW) :
EGLCweatherdata<- riem_measures("EGLC",date_start="2002-01-01",date_end="2006-12-31")


#ouest de LONDRES (ouest de Londres) aéroport d'heathrow 20km environ
EGLLweatherdata<- riem_measures("EGLL",date_start="2002-01-01",date_end="2006-12-31") #10min pour importer

#sknt vitesse du vent en knots, drct= direction du vent à partir du nord, temp en fareneight
#Ajustage de donnée
EGLCweatherdata<-EGLCweatherdata[,c(1,2,5,6,7,8,9,11)]
EGLLweatherdata<-EGLLweatherdata[,c(1,2,8,9)]#Dwpf = point de rosée Temperature in Fahrenheit, typically @ 2 meters , relh: Relative Humidity in %,alti: Pressure altimeter en pouces
mydata<-mydata[,c(1,2)]# on prend l'o3 , moins de NA

#Definition de l'o3 :
#L'ozone (O3) résulte ainsi de la transformation chimique de l'oxygène au contact d'oxydes d'azote et d'hydrocarbures, en présence de rayonnement ultra-violet
#solaire et d'une température élevée. L'ozone ainsi que d'autres polluants photochimiques (les PAN ou nitrates de peroxyacétyle, aldéhydes, cétones...) constituent
#le smog, ce nuage brunâtre qui stagne parfois au-dessus des grandes villes comme Paris.

#La formation d'ozone nécessite un certain temps durant lequel les masses d'air se déplacent.
#Ce qui explique pourquoi les niveaux d'ozone sont plus soutenus en zone rurale autour de la région parisienne 
#que dans l'agglomération parisienne où leur précurseurs ont été produits.


#On s'occupe de la variable Y (na et interpolation) 365*24*5+24
summary(mydata)
l9=approx(as.POSIXct(mydata$date),mydata$o3,method="linear",n=43824)
ozone<-numeric()
ozone<-c(l9$y[9:43789])



#Interpolation de Weather data
par(mfrow=c(3,1))
plot(EGLCweatherdata$valid,EGLCweatherdata$dwpf,col="red",type="l")
plot(EGLCweatherdata$valid,EGLCweatherdata$relh,col="blue",type="l")
plot(EGLCweatherdata$valid,EGLCweatherdata$alti,col="green",type="l")



#Point de Rosée (Fareneight) :
#on prend des points equiréparties entre 7h20 du 1 er janvier2002 et le 30 midi 50 (365*24*6*5+24*6-7*6-2-11*6-1-24*6)
l=approx(as.POSIXct(EGLCweatherdata$valid),EGLCweatherdata$dwpf,method="linear",n=262689)
RoseP<-numeric()
RoseP=c(l$y[seq(5,262689,6)]) # le vecteur des points de rosée fini
################################################
#### %d'humudité :
l2=approx(as.POSIXct(EGLCweatherdata$valid),EGLCweatherdata$relh,method="linear",n=262689)
Humi<-numeric()
Humi=c(l2$y[seq(5,262689,6)]) # Vecteur fini de l'humidité
#######################################################
###### Pressure Altimetre
l3=approx(as.POSIXct(EGLCweatherdata$valid),EGLCweatherdata$alti,method="linear",n=262689)
Pression<-numeric()
Pression=c(l3$y[seq(5,262689,6)])
######################################################

#####Température : 
l4=approx(as.POSIXct(EGLCweatherdata$valid),EGLCweatherdata$tmpf,method="linear",n=262689)
Temperature<-numeric()
Temperature=c(l4$y[seq(5,262689,6)])

#######################################################

###### Vitesse du vent :
l5=approx(as.POSIXct(EGLCweatherdata$valid),EGLCweatherdata$sknt,method="linear",n=262689)
VitesseV<-numeric()
VitesseV=c(l5$y[seq(5,262689,6)])
######################################################


###### Direction du Vent :
l6=approx(as.POSIXct(EGLCweatherdata$valid),EGLCweatherdata$drct,method="linear",n=262689)
DirectionV<-numeric()
DirectionV=c(l6$y[seq(5,262689,6)])
#####################################################

###########################################
#Direction du vent Heathrow :365*24*6*5+24*6-8-24*6-1
summary(EGLLweatherdata)
l7=approx(as.POSIXct(EGLLweatherdata$valid),EGLLweatherdata$drct,method="linear",n=262791)
DirectionV2<-numeric()
DirectionV2=c(l7$y[seq(5,262791,6)])
DirectionV2<-DirectionV2[7:43787]
############################################

###########################################
#Vitesse du vent Heathrow :365*24*6*5-8-24*6-1
l8=approx(as.POSIXct(EGLLweatherdata$valid),EGLLweatherdata$sknt,method="linear",n=262791)
VitesseV2<-numeric()
VitesseV2=c(l8$y[seq(5,262791,6)])
VitesseV2<-VitesseV2[7:43787]

##########################################


#Création du dataframe nettoyé : 1km/h=0.5399 noeuds, 89F=31 C , 26.85=-3C, 
PP<-data.frame(mydata$date[9:43789],ozone,VitesseV,DirectionV,VitesseV2,DirectionV2,Temperature,Pression,Humi,RoseP)
names(PP)<-c("Dates","Ozone","WVitesseNE","WDirectionNE","WVitesseO","WDirectionO","Temperature","Pression","humidite","Point_de_rosee")

##################################################
#On regarde les valeurs aberrantes à la main :

plot(PP$WVitesseO)
for (i in 1:43781){
  if (PP$WVitesseO[i] >60){PP$WVitesseO[i]<-median(PP$WVitesseO)}
}
#################################################


########################################################################################
remove(c,mydata,weatherdata1,weatherdata2,a,l9,ozone,a2,a3,a4,a5,a6,a7,a8,b,b2,b3,b4,b5,b6,DirectionV,DirectionV2,Humi,i,l,l2,l3,l4,l5,l6,l7,l8,Pression,RoseP,Speed,Temperature,Traff,VitesseV,VitesseV2,W,W2,W3,W4,W5,W6,W7,W8,Z,Z2,Z3,Z4,Z5,Z6)

######################
#Extraction de la matrice de test et de validation :
PPtraining<-PP[1:35056,]
PPtest<-PP[35056:43781,]

######################

############################
#Mesures d'erreurs :
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=10))
}
################################
################################
#Bagging 
eq <- Ozone ~ WVitesseNE +WDirectionNE+WVitesseO+WDirectionO+Temperature+Pression+humidite+Point_de_rosee
rpart0<-rpart(eq, data= PPtraining,control=c(maxsurrogate=6))
rpart0.forecast<-predict(rpart0,newdata=PPtest)
mape(PPtest$Ozone,PPtest$Ozone)
rmse(PPtest$Ozone-rpart0.forecast)
plot(PPtest$Ozone)
lines(rpart0.forecast,col="blue")






####################################
#Random Forest :
eq <- Ozone ~ WVitesseNE +WDirectionNE+WVitesseO+WDirectionO+Temperature+Pression+humidite+Point_de_rosee 
rf0 <- randomForest(eq,ntree=200,data=PP, importance=TRUE)
rf0.fitted <- predict(rf0,newdata=PP)
rf0.forecast <- predict(rf0,newdata=PP)
rmse(PP$Ozone-rf0.fitted)
mape(PP$Ozone,rf0.forecast)
imp <- importance(rf0, type = 1, scale=T)
varImpPlot(rf0, type=1)
rf0$importance # montre l'importance des variables
names(rf0)
plot(rf0$mse)

rf0$ntree
plot(rf0)  ###oob error
