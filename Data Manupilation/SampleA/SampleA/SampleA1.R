#-----------------------Importing data frame from Excel

library(readxl)
SampleA1 <- read_excel("C:/Stats in R/workshop 3/SampleA/SampleA/SampleA1.xlsx")
View(SampleA1)

#----------------------Checking if the variable is factor

is.factor(SampleA1$Location)  ### FALSE
SampleA1$Location<-factor(SampleA1$Location)

#-----------------------Checking if the variable is numeric

is.numeric(SampleA1$Ozone)  ### TRUE

#-------------------------------Frequency Table

install.packages("summarytools")
library(summarytools)
ctable(SampleA1$Location,SampleA1$Ozone)
cozone<-cut(SampleA1$Ozone,5)
ctable(SampleA1$Location,cozone)
install.packages("xlsx")
library(xlsx)
write.xlsx(ctable(SampleA1$Location,cozone),
SampleA1 <- read_excel("C:/Users/R00194707/Desktop/Data Analysis with R/Workshop 2/SampleA/ctable.xlsx"))
       
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
(StatsOzone_Location01<-SampleA1 %>% group_by(Location) %>% summarise("Sample size" = n(),
                                         Mean = mean(Ozone),
                                         "Standard Deviation" = sd(Ozone),
                                         Median = median(Ozone),
                                         "1st quartile" = quantile(Ozone,0.25),
                                         "3rd quartile" = quantile(Ozone,0.75),
                                         Min = min(Ozone),
                                         Max = max(Ozone)))
(StatsOzone_Location02<-round(StatsOzone_Location01,1))
t(StatsOzone_Location)
#----------------------Export to excle---------
install.packages("xlsx")
library("xlsx")
#write.xlsx(t(StatsOzone_Location),
#"C:/Users/NIMA/Desktop/Data Analysis with R/Workshop 2/SampleA/descriptive statistics.xlsx")
#---------------------Histogram----------------
#######-----------two variable---Age by gender---######
library(ggplot2)
(hg0<-ggplot(SampleA1,aes(x = Ozone))+
   geom_histogram(breaks = seq(0,25,5))+
   facet_grid(~Location))
(hg1<-hg0+
    labs(x = "Ozone readings (ppm)", y = "Frequency")+ ## label the axes
    coord_cartesian(xlim = c(0,25), ylim = c(0,20))+
    scale_x_continuous(breaks = seq(0,25,5))+
    scale_y_continuous(breaks = seq(0,20,5))+ ## scale the x, y-axes
    theme_bw()) ## change outine
