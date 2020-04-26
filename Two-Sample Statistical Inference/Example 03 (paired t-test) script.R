
########################## OPEN NEW SCRIPT
# Import paired t-test
library(readxl)
Dep <- read_excel("C:/Stats in R/Workshop 04/Datasets.xlsx", 
                     sheet = "Paired t-test")
View(Dep)

# need to change data from long to wide
library(tidyr)
Paired<-gather(Dep,Type,Height,2:3)
View(Paired)

# properties
is.factor(Paired$Type)
Paired$Type<-factor(Paired$Type,levels=c("Reported","Measured"))
is.numeric(Paired$Height)

# explore
ggplot(Paired,aes(x=Type, y=Height))+stat_boxplot(geom = "errorbar")+geom_boxplot()+
  labs(x = "", y = "Height (inches)")+
coord_cartesian(ylim=c(65, 85)) + scale_y_continuous(breaks=seq(65,85,5)) 

# normality
Norm4<-Paired %>% group_by(Type) %>% summarise("Sample size"=n(),Mean = mean(Height), 
                Median = median(Height), Skewness=skew(Height),
                "Normally distributed"=ifelse(shapiro.test(Height)$p.value>0.05,"Yes","No"))
t(Norm4)

# Proceed leaving outlier in
t.test(Paired$Height ~ Paired$Type, paired=TRUE)
wilcox.test(Paired$Height ~ Paired$Type, paired=TRUE,conf.int=T,exact=F) # outputs CI as well
cohen.d(Paired$Height ~ Paired$Type, paired=TRUE)

# Remove the third subject
library(dplyr)
Paired2<-filter(Paired,Case != "C") # create new dataframe with subject C removed

# copy and paste old script and change Paired to Paired2
ggplot(Paired2,aes(x=Type, y=Height))+stat_boxplot(geom = "errorbar")+
  geom_boxplot()+labs(x = "", y = "Height (inches)")+
  coord_cartesian(ylim=c(65, 75)) + scale_y_continuous(breaks=seq(65,75,1)) #+

# normality
Norm5<-Paired2 %>% group_by(Type) %>% summarise("Sample size"=n(),Mean = mean(Height), 
                    Median = median(Height), Skewness=skew(Height),
                    "Normally distributed"=ifelse(shapiro.test(Height)$p.value>0.05,"Yes","No"))
t(Norm5)

# test and effect size
t.test(Paired2$Height ~ Paired2$Type, paired=TRUE)
wilcox.test(Paired2$Height ~ Paired2$Type, paired=TRUE,conf.int=T,exact=F) # outputs CI as well
cohen.d(Paired2$Height ~ Paired2$Type, paired=TRUE)
