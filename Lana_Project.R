library(CATT)
library(dbplyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(htmlTable)
library(htmltools)
library(HTMLUtils)
library(htmlwidgets)
library(knitr)
library(markdown)
library(MASS)
library(rmarkdown)
library(tidyverse)
library(scatterplot3d)
library(rgl)
library(plotrix)
#Database placement
lana.pro <- read.csv("codnew4.csv") 

##statistic analysis
# median year of birth of the person who answered the survey
median(lana.pro$date.b) 
# Average weekly hours
mean(lana.pro$sum.hour)
## Standard deviation of learning from shopping
sd(lana.pro$techno.1)

# The Cochran-Armitage Trend Test
CATT(lana.pro$sex,lana.pro$techno)

#https://he.wikipedia.org/wiki/%D7%9E%D7%91%D7%97%D7%9F_%D7%A7%D7%95%D7%A7%D7%A8%D7%90%D7%9F-%D7%90%D7%A8%D7%9E%D7%99%D7%98%D7%90%D7%96%27

## Graphs


pie.aera <- table(lana.pro$area)
names(pie.aera) <- c("Jerusalem" , "North" ,  "Haifa" ,  "Center" ,  "Tel-Aviv" , "South" , "Jehuda & Samaria")
pie3D(pie.aera,labels=names(pie.aera),explode=0.1,
      main="Pie Chart of Counties")

hist(lana.pro$date.b ,main ="Age distribution" ,xlab = "year", ylab = "people")

ggplot(data=lana.pro,mapping=aes(x=veteq ,y=techno))+
  geom_jitter(position=position_jitter(0.2))+
  labs(title="Seniority vs technological capabilities",
       x="Seniority",
       y = "technological capabilities")


ggplot(data = lana.pro,aes(migzar))+
  geom_bar()+
  labs(title="Educational institution sector",
       x="Sector",col="darkblue")
     
ggplot(data=lana.pro,mapping=aes(comfort ,tadirut.2 ))+
  geom_smooth()+
  labs(title="The frequency of system use depends on convenience",
       x="Comfort",
       y = "Frequency of system")
  


ggplot(lana.pro)+
  geom_col(fill = "lightblue",
           color = "black",
           aes(x = sum.hour ,y = veteq))+
  geom_label(aes(x = sum.hour,
                 y = veteq,
                 label = paste0(round(veteq, 0), "%"))) +
  ggtitle("Effect of study seniority on the number of frontal hours",
          subtitle = "Based on 2020 survey")

ggplot(data = lana.pro, aes(sum.hour,num.lev))+  
  geom_violin(fill="blue")+
  labs(title="Number of hours distributed over the number of classes",
       x="Amount of frontal hours",
       y = "Num of classes")+
  theme_classic()



ggplot(data = lana.pro, aes(sex ,color="steelblue"))+
  geom_bar()+
  labs(title="Distribution by gender",
       x="Gender")


ggplot(data = lana.pro,aes(COVID.19 , merahok))+
  geom_raster()+
  labs(title="Has online learning strengthened the control of online teaching?",
       x="Online learning",
       y = "Future application")

ggplot(data = lana.pro,aes(mosad,cuors))+
  geom_jitter(position=position_jitter(0.2))+
  labs(title="Using Online Courses - Universities vs. Colleges",
       x="Type of institution",
       y = "Online courses")

boxplot(lana.pro$techno.1, main = "Study hours online during the Corona",
        xlab = "Hours",
        ylab = "COVID 19",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE)


qqplot(x = lana.pro$sum.hour,
       y = lana.pro$techno.1,
       xlab = "Frontal hours in routine",
       ylab = "Online hours during the Corona",
       main = "The relationship between school hours in the routine and the Corona period")
