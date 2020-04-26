####-------------------Creating your own data frame
Student<-seq(1:17)
NVA<-c(50,60,58,72,36,51,49,49,25,52,41,32,58,39,25,40,61)
WVA<-c(58,70,60,73,40,63,54,60,29,57,66,37,50,48,80,65,70)
Lecture<-data.frame(Student,NVA,WVA)
View(Lecture)
####-------------------Change data from wide to long
library(tidyr)
LectureL<-gather(Lecture,Occasion,Quality,2:3)
View(LectureL)
####--------------------------properties check
is.factor(LectureL$Occasion)
LectureL$Occasion<-factor(LectureL$Occasion,levels=c("WVA","NVA"))
is.numeric(LectureL$Quality)
####------------------------------Boxplot
ggplot(LectureL,aes(x=Occasion, y=Quality))+stat_boxplot(geom = "errorbar")+geom_boxplot()+
  labs(x = "", y = "Quality (Score)")+
  coord_cartesian(ylim=c(20, 85)) + scale_y_continuous(breaks=seq(20,85,5))
####-----------------------------normality
library(dplyr)
(Norm<-LectureL%>%group_by(Occasion) %>% 
    summarise("sample size" = n(),
              Mean = round(mean(Quality),1),
              "95% CI (lower)" = round(mean(Quality)-(1.96*sd(LectureL$Quality)/sqrt(n())),1),
              "95% CI (upper)" = round(mean(Quality)+(1.96*sd(LectureL$Quality)/sqrt(n())),1),
              "Standard Deviation" = round(sd(Quality),2),
              Median = median(Quality),
              Min = min(Quality),
              Max = max(Quality),
              Skewness = round(skew(Quality),4),
              "Normally distributed" = 
                ifelse(shapiro.test(Quality)$p.value>0.05,"Yes","No")))
t(Norm)
####--------------------Related t-test and Effect size
# test and effect size
(result<-t.test(LectureL$Quality ~ LectureL$Occasion, paired=TRUE))
(effect<-cohen.d(LectureL$Quality ~ LectureL$Occasion,paired=TRUE))

Statistics<-c("Mean difference","95% CI (lower)","95% CI (upper)",
              "p-value","Effect size")
m1<-mean(LectureL$Quality[LectureL$Occasion=="WVA"])
m2<-mean(LectureL$Quality[LectureL$Occasion=="NVA"])
(Output<-c(m1-m2,result$conf.int,ifelse(result$p.value<0.0005,"<0.0005",result$p.value),effect$estimate))
(Table<-data.frame(Statistics,Output))
(Output_rounded<-c(round(m1-m2,2),round(result$conf.int,2),
                   round(result$p.value,4),round(effect$estimate,3)))
(Table<-data.frame(Statistics,Output_rounded))