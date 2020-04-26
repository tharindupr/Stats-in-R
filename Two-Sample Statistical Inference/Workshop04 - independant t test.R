### importing IQ Data
library(readxl)
Indep <- read_excel("C:/Users/R00182486/Desktop/Workshop 04/Datasets.xlsx", 
                   sheet = "Independent t-test")
View(Indep)


### see properties of data
(is.factor(Indep$Group))
Indep$Group<-factor(Indep$Group, levels=c("Placebo", "Caffeine"))
is.numeric(Indep$Score)
### is a factor
### set as a factor

### do the descriptive statisicts

(bp1<-ggplot(Indep, aes(x=Group, y=Score))+stat_boxplot(geom="errorbar")+geom_boxplot())+
  labs(x="Group", y="Score")

##outliers are present through investigation oytliers are found be typos 
(min(Indep$Score[Indep$Group=="Caffeine"]))

Indep$Score[Indep$Score==2]<-12 #let us say thi is a type shoule have been 12

(max(Indep$Score[Indep$Group=="Placebo"]))

Indep$Score[Indep$Score==20]<-12 #let us say thi is a type shoule have been 12

(bp1<-ggplot(Indep, aes(x=Group, y=Score))+stat_boxplot(geom="errorbar")+geom_boxplot())+
  labs(x="Group", y="Score")




### checking for the Normality

(Norm<-Indep %>% group_by(Group) %>% summarise("sample size"=n(),mean = mean(Score),
                                               "standard deviation"=sd(Score),              
                                                Median = median(Score), 
                                               "Skewness" = skew(Score),
                                                "Normaly Distributed"=ifelse(shapiro.test(Score)$p.value>0.05,"Yes","No")))

t(Norm)


### hormoginity of varience

library(car)
leveneTest(Score ~ Group, center=mean, data=Indep)

### depeing on the above result
### independent t-test (if mesures are normally distributed)

result<-t.test(Score ~ Group, var.equal=TRUE, data=Indep)

# effect size (Standarized mean difference)

library(effsize)
effect<-cohen.d(Indep$Score ~ Indep$Group)




#### creating the table

statistics<-c("Mean difference", "95% CI", "95% CI (upper)", "p-value", "Effect Size")

m1<-mean(Indep$Score[Indep$Group=="Placebo"])
m2<-mean(Indep$Score[Indep$Group=="Caffeine"])
output<-c(m1-m2, result$estimate, result$p.value, effect$estimate)

ouput_rounded<-c(round(m1-m2,2), round(result$conf.int, 3), round(result$p.value, 4), round(effect$estimate))

Table<-data.frame(statistics, ouput_rounded)

