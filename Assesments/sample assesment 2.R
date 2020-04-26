student<-c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17')
no_Aids<-c(50, 60, 58, 72, 36, 51, 49, 49, 25, 52, 41, 32, 58, 39, 25, 40, 61)
with_Aids<-c(58, 70, 60, 73, 40, 63, 54, 60, 29, 57, 66, 37, 50, 48, 80, 65, 70)
df<-data.frame(student, no_Aids, with_Aids)
View(df)


# need to change data from long to wide
library(tidyr)
Paired<-gather(df,type,quality,2:3)
View(Paired)


# properties
is.factor(Paired$type)
Paired$type<-factor(Paired$type,levels=c("no_Aids","with_Aids"))
is.numeric(Paired$quality)


# explore
library(ggplot2)
ggplot(Paired,aes(x=type, y=quality))+stat_boxplot(geom = "errorbar")+geom_boxplot()+
  labs(x = "", y = "Quality Score (out of 100)")


# normality
library("dplyr")
library(psych)
Norm4<-Paired %>% group_by(type) %>% summarise("Sample size"=n(),Mean = mean(quality), 
                                               Median = median(quality), Skewness=skew(quality),"Min"=min(quality), "Max"=max(quality),
                                               "Normally distributed"=ifelse(shapiro.test(quality)$p.value>0.05,"Yes","No"))
t(Norm4)

library(effsize)
# paired t test
t.test(Paired$quality ~ Paired$type, paired=TRUE)
##wilcox.test(Paired$Height ~ Paired$Type, paired=TRUE,conf.int=T,exact=F) # outputs CI as well
cohen.d(Paired$quality ~ Paired$type)

