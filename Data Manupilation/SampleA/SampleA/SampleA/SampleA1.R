#-----------------------Importing data frame from Excel

library(readxl)
SampleA1 <- read_excel("C:/Users/R00194707/Desktop/SampleA/SampleA/SampleA1.xlsx")
View(SampleA1)


#----------------------Checking if the variable is factor

is.factor(SampleA1$Location)  ### FALSE
SampleA1$Location<-factor(SampleA1$Location)

#-----------------------Checking if the variable is numeric

is.numeric(SampleA1$Ozone)  ### TRUE

#-------------------------------Frequency Table

install.packages("summarytools")
library(summarytools)
duration = SampleA1$Ozone   # the eruption durations 
x<-round(sd(duration))
(c0<-cut(SampleA1$Ozone,5))
ctable(SampleA1$Location,c0)
d<-ctable(SampleA1$Location,SampleA1$Ozone)



require(stats)
view(d)



#----------------------------




#--------------------------------------Grouping

install.packages("psych")
library(psych)
view(d)
describe(SampleA1$Ozone)
describeBy(SampleA1$Ozone,SampleA1$Location)

#-------------------------Graphical-----------
#----------------------------------Boxplot----
#-------------------------two variables-------
install.packages("ggplot2")
library(ggplot2)
(bp00<-ggplot(SampleA1,aes(x = SampleA1$Location,y = SampleA1$Ozone,))+
  stat_boxplot(geom = "errorbar")+
  geom_boxplot(color = "red", fill = "orange", alpha = 0.2))
# now edditing
(bp01<-bp00+
    labs(x = "Location", y = "Ozone Reading (ppm)")+
    coord_cartesian(ylim = c(0,25))+
    scale_y_continuous(breaks = seq(0,25,5))+
    theme(text = element_text(size = 11)))

#---------------------Numerical----------------
#------------------------------two variables---
#---------------------Summarise----------------

install.packages("dplyr")
library("dplyr")
(StatsOzone_Location<-SampleA1 %>% group_by(Location) %>% summarise("Sample size" = n(),
                                         Mean = round(mean(Ozone)),
                                         "Standard Deviation" = round(sd(Ozone)),
                                         Median = median(Ozone),
                                         "1st quartile" = quantile(Ozone,0.25),
                                         "3rd quartile" = quantile(Ozone,0.75),
                                         Min = round(min(Ozone)),
                                         Max = round(max(Ozone))))
(StatsOzone_Location<-round(StatsOzone_Location,0))


t(StatsOzone_Location)

