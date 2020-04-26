### importing IQ Data
library(ggplot2)
library(readxl)
DF<-read_excel("C:/Users/R00182486/Desktop/Workshop 04/Datasets.xlsx")


#Apply the three checks. followed with test
# 1: histogram is bell shaped

library(psych)
range(DF$IQ)
ggplot(DF, aes(x=DF$IQ))+geom_histogram(breaks=seq(70, 140,10))+theme_bw()+
  labs(x="IQ score", y="Frequency")+scale_y_continuous(breaks=seq(0,14,2))+
  scale_x_continuous(breaks=seq(40,140,10))






# 2: mean roughly equal to median
# 3: -1<skewness<1
# 4: Do the test
library(psych)
library(dplyr)
(Norm<-DF %>% summarise("sample size"=n(),mean = mean(IQ),"standard deviation"=sd(IQ),              
                        Median = median(IQ), "Skewness" = skew(IQ),
                        "Normaly Distributed"=ifelse(shapiro.test(IQ)$p.value>0.05,"Yes","No")))
t(Norm)




#one sample t-test

(result<-t.test(DF$IQ, mu=100)) #2-tailed test
t.test(DF$IQ, mu=100, alternative = "less")
t.test(DF$IQ, mu=100, alternative = "greater")
wilcox.test(DF$IQ, mu=100, conf.int = 0.95, exact=F)




# Excercise

library(readxl)
breaks <- read_excel("C:/Users/R00182486/Desktop/Workshop 04/Datasets.xlsx", 
                       +     sheet = "Brakes")
View(breaks)


(Norm<-breaks %>% summarise("sample size"=n(),mean = mean(Diameter),"standard deviation"=sd(Diameter),              
                        Median = median(Diameter), "Skewness" = skew(Diameter),
                        "Normaly Distributed"=ifelse(shapiro.test(Diameter)$p.value>0.05,"Yes","No")))

t(Norm)

(result<-t.test(breaks$Diameter, mu=322))



