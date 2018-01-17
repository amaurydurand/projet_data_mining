rm(list=objects())

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

################# VITESSE ##################################
# il y a 6 mesures : MG1 le plus proche, MG6 le plus loin
# ce sont des mesures de vitesse moyenne en km/h
# les mesures sont par default les unes apres les autres
# je les mets dans les colonnes differentes

files = c("Data/MaryleboneRoadTrafficSpeed2002.csv",
          "Data/MaryleboneRoadTrafficSpeed2003.csv",
          "Data/MaryleboneRoadTrafficSpeed2004.csv",
          "Data/MaryleboneRoadTrafficSpeed2005.csv",
          "Data/MaryleboneRoadTrafficSpeed2006.csv",
          "Data/MaryleboneRoadTrafficSpeed2007.csv")
car_speed <- data.frame(DateTime=character(),
                        MG1=double(), 
                        MG2=double(), 
                        MG3=double(),
                        MG4=double(),
                        MG5=double(),
                        MG6=double())

for (file in files){
  speed <- read_delim(file, col_names=TRUE, delim=',')
  speed_MG1 <- speed[which(speed$Lane=="MG1"),][,c(2,3)]
  names(speed_MG1)[2] <- "MG1"
  speed_MG2 <- speed[which(speed$Lane=="MG2"),][,c(2,3)]
  names(speed_MG2)[2] <- "MG2"
  speed_MG3 <- speed[which(speed$Lane=="MG3"),][,c(2,3)]
  names(speed_MG3)[2] <- "MG3"
  speed_MG4 <- speed[which(speed$Lane=="MG4"),][,c(2,3)]
  names(speed_MG4)[2] <- "MG4"
  speed_MG5 <- speed[which(speed$Lane=="MG5"),][,c(2,3)]
  names(speed_MG5)[2] <- "MG5"
  speed_MG6 <- speed[which(speed$Lane=="MG6"),][,c(2,3)]
  names(speed_MG6)[2] <- "MG6"
  

  cur_car_speed <- speed_MG1
  for (t in list(speed_MG2, speed_MG3, speed_MG4, speed_MG5, speed_MG6)){
    cur_car_speed <- inner_join(cur_car_speed, t, by="DateTime")  
  }
  car_speed <- rbind(car_speed, cur_car_speed)
}

write.table(car_speed, 'Data/car_speed2002-2007.csv', row.names=FALSE, sep=',')

################# DECOMPTE ##################################
# il y a 6 mesures : MG1 le plus proche, MG6 le plus loin
# il y a 6 catégories :
# CLS1	Motorcycle
# CLS2	Car or Light Van (<5.2m)
# CLS3	Car and trailer
# CLS4	Rigid Lorry, Heavy Van (>=5.2m) or mini-bus
# CLS5	Articulated Lorry
# CLS6	Bus or Coach
# les mesures sont par default les unes apres les autres
# je les mets dans les colonnes differentes
rm(list=objects())

files = c("Data/MaryleboneRoadTrafficCount2002.csv",
          "Data/MaryleboneRoadTrafficCount2003.csv",
          "Data/MaryleboneRoadTrafficCount2004.csv",
          "Data/MaryleboneRoadTrafficCount2005.csv",
          "Data/MaryleboneRoadTrafficCount2006.csv",
          "Data/MaryleboneRoadTrafficCount2007.csv")
traffic_count <- data.frame(DateTime=character(),
                            MG1_CLS1=integer(), 
                            MG1_CLS2=integer(), 
                            MG1_CLS3=integer(), 
                            MG1_CLS4=integer(), 
                            MG1_CLS5=integer(), 
                            MG1_CLS6=integer(), 
                            MG2_CLS1=integer(), 
                            MG2_CLS2=integer(), 
                            MG2_CLS3=integer(), 
                            MG2_CLS4=integer(), 
                            MG2_CLS5=integer(), 
                            MG2_CLS6=integer(), 
                            MG3_CLS1=integer(), 
                            MG3_CLS2=integer(), 
                            MG3_CLS3=integer(), 
                            MG3_CLS4=integer(), 
                            MG3_CLS5=integer(), 
                            MG3_CLS6=integer(), 
                            MG4_CLS1=integer(), 
                            MG4_CLS2=integer(), 
                            MG4_CLS3=integer(), 
                            MG4_CLS4=integer(), 
                            MG4_CLS5=integer(), 
                            MG4_CLS6=integer(), 
                            MG5_CLS1=integer(), 
                            MG5_CLS2=integer(), 
                            MG5_CLS3=integer(), 
                            MG5_CLS4=integer(), 
                            MG5_CLS5=integer(), 
                            MG5_CLS6=integer(), 
                            MG6_CLS1=integer(), 
                            MG6_CLS2=integer(), 
                            MG6_CLS3=integer(), 
                            MG6_CLS4=integer(), 
                            MG6_CLS5=integer(), 
                            MG6_CLS6=integer())

for (file in files){
  count <- read_delim(file, col_names=TRUE, delim=',')
  count_MG1 <- count[which(count$Lane=="MG1"),][,-1]
  count_MG2 <- count[which(count$Lane=="MG2"),][,-1]
  count_MG3 <- count[which(count$Lane=="MG3"),][,-1]
  count_MG4 <- count[which(count$Lane=="MG4"),][,-1]
  count_MG5 <- count[which(count$Lane=="MG5"),][,-1]
  count_MG6 <- count[which(count$Lane=="MG6"),][,-1]
  
  for (j in seq(2, length(names(count_MG1)))){
      names(count_MG1)[j] <- paste("MG1", names(count_MG1)[j], sep='_')
  }
  for (j in seq(2, length(names(count_MG2)))){
    names(count_MG2)[j] <- paste("MG2", names(count_MG2)[j], sep='_')
  }
  for (j in seq(2, length(names(count_MG3)))){
    names(count_MG3)[j] <- paste("MG3", names(count_MG3)[j], sep='_')
  }
  for (j in seq(2, length(names(count_MG4)))){
    names(count_MG4)[j] <- paste("MG4", names(count_MG4)[j], sep='_')
  }
  for (j in seq(2, length(names(count_MG5)))){
    names(count_MG5)[j] <- paste("MG5", names(count_MG5)[j], sep='_')
  }
  for (j in seq(2, length(names(count_MG6)))){
    names(count_MG6)[j] <- paste("MG6", names(count_MG6)[j], sep='_')
  }
  
  cur_traffic_count <- count_MG1
  for (t in list(count_MG2, count_MG3, count_MG4, count_MG5, count_MG6)){
    cur_traffic_count <- inner_join(cur_traffic_count, t, by="DateTime")  
  }
  traffic_count <- rbind(traffic_count, cur_traffic_count)
}

write.table(traffic_count, 'Data/traffic_count2002-2007.csv', row.names=FALSE, sep=',')

############ VISUALISATIONS #################################
rm(list=objects())
speed <- read_delim('Data/car_speed2002-2007.csv', col_names=TRUE, delim=',')
count <- read_delim('Data/traffic_count2002-2007.csv', col_names=TRUE, delim=',')
summary(speed)
speed$DateTime <- as.POSIXct(speed$DateTime)
summary(speed)
plot(as.POSIXct(speed$DateTime), speed$MG1)

