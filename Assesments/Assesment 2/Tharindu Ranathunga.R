##Setting the dataframe

Subject<-c(1, 2, 3, 4, 5, 6, 7)
EPO<-c(12, 5, 11, 11, 9, 18, 10)
Control<-c(7, 3, 4, 6, 3, 13, 8)

df<-data.frame(Subject, EPO, Control)
View(df)

# need to change data from long to wide
library(tidyr)
Paired<-gather(df,Type,Endurance,2:3)
View(Paired)


# properties
is.factor(Paired$Type)
Paired$Type<-factor(Paired$Type,levels=c("EPO","Control"))
is.numeric(Paired$Endurance)


# explore
library(ggplot2)
ggplot(Paired,aes(x=Type, y=Endurance))+stat_boxplot(geom = "errorbar")+geom_boxplot()+
  labs(x = "", y = "Endurance Score (out of 20)")


# normality check 
library(dplyr)
(Norm<-Paired%>%group_by(Type) %>% 
    summarise("sample size" = n(),
              Mean = round(mean(Endurance),2),
              "95% CI (lower)" = round(mean(Endurance)-(1.96*sd(Paired$Endurance)/sqrt(n())),2),
              "95% CI (upper)" = round(mean(Endurance)+(1.96*sd(Paired$Endurance)/sqrt(n())),2),
              "Standard Deviation" = round(sd(Endurance),2),
              Median = median(Endurance),
              Min = min(Endurance),
              Max = max(Endurance),
              Skewness = round(skew(Endurance),4),
              "Normally distributed" = 
                ifelse(shapiro.test(Endurance)$p.value>0.05,"Yes","No")))
t(Norm)


# Related T test 

library(effsize)
# paired t test
(Ttest<-t.test(Paired$Endurance ~ Paired$Type, paired=TRUE))
##wilcox.test(Paired$Height ~ Paired$Type, paired=TRUE,conf.int=T,exact=F) # outputs CI as well
(effect<-cohen.d(Paired$Endurance ~ Paired$Type))


Column1<-c("Mean difference","95% CI (lower)","95% CI (upper)",
              "p-value","Effect size")
(Column2<-c(round(Norm$Mean[1]-Norm$Mean[2],2),
            round(Ttest$conf.int,2),
            round(ifelse(Ttest$p.value<0.0005,"<0.0005",Ttest$p.value),4),
            round(effect$estimate,2)))


Table<-data.frame(Column1, Column2)
View(Table)
