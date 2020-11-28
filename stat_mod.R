lana.pro <- read.csv("codnew4.csv")
library(dbplyr)
library(dplyr)
library(DescTools)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(MASS)
library(CATT)
#ניתוחים סטטיסטיים
median(lana.pro$date.b) #הצגת חציון שנת הלידה של מי שענה על הסקר
mean(lana.pro$sum.hour)# ממוצע שעות שבועיות
sd(lana.pro$techno.1) # סטיית תקן של לימוד מקונות

#מבחנים סטטיסטיים

CATT(lana.pro$sex,lana.pro$techno)

#גרפים

boxplot(lana.pro$sex,lana.pro$internet,col = "red")

pie.aera <- table(lana.pro$area)
names(pie.aera) <- c("JR" , "NR" ,  "HA" ,  "CE" ,  "TA" , "ST" , "JS")
pie(pie.aera)

hist.mosad <- lana.pro$mosad
names(hist.mosad) <- c("COL" , "UNI")
hist(hist.mosad,col = "red")

hist(lana.pro$sex,col = "red")

ggplot(data=lana.pro,mapping=aes(x=veteq ,y=techno))+
  geom_jitter()+
  stat_summary(fun.y=median,colour="1",size=2,geom="point")



