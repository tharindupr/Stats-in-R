library(readxl)
Repeated<- read_excel("C:/Stats in R/Workshop 5/W5 Datasets V2.xlsx", 
                         sheet = "Repeated")
View(Repeated)

###CHECK THE DATA PROPERTIES , MISSING VALUES
## append a subject ID... From the work
(n<- dim(Repeated[1])) # sample-size
Patient<-(1:n)
library(dplyr)
Repeated<-mutate(Repeated,Patient)


####Ordering variables (if desired)
(cn<-dim(Repeated)[2])
Repeated<-Repeated[,c(cn,1:cn-1)]

#select data of interest
library(dplyr)
Rep<-select(Repeated,Patient, Treatment=`Treatment group`,
            Baseline=`Oral condition at the initial stage`,
            "Week 02"= `Oral condition at the end of week 02`,
            "Week 04"= `Oral condition at the end of week 04`,
            "Week 06"= `Oral condition at the end of week 06`)

## Convert to long format
library(tidyr)
(Long <- gather(Rep,Time,Oral, 3:6))
view(Long)

### First explore the data
##STEP 01 : Check for properties
is.factor(Long$Time)
Long$Time<-factor(Long$Time, levels = c("Baseline","Week 02", "Week 04", "Week 06")) #specify order of levels

Long$Treatment<-factor(Long$Treatment, levels = c("Placebo", "Aloe Juice"))
is.numeric(Long$Oral)


###STEP 02: Numerical Descriptive Statistics
#next line won't work because of missing data
(Stats<-Long %>% group_by(Treatment, Time) %>% summarise("Sample size"=n(), "Mean"= mean(Oral),
                                                         "Standard Deviation"=round(sd(Oral),1),
                                                         "Median"= median(Oral),
                                                         "1st quartile"=quantile(Oral,0.25),
                                                         "3rd quartile"= quantile(Oral,0.75),
                                                         "Min"=min(Oral),
                                                         "Max"=max(Oral)))

### Locating the missing value patients... from workshop 03
Long[!complete.cases(Long),]

##TWO OPTIONS
#1. Per protocol (PP) analysis: remove patients from study and complete analysis with 23 patients
Long23<-na.omit(Long) #Removes patients with missing data
#2. Intention to Treat (ITT) analysis: impute data values for patients with
# missing values e.e., LOCF
Long$Oral[Long$Patient==22 & Long$Time =="Week 06"]<-Long$Oral[Long$Patient==22 & Long$Time == "Week 04"]
Long$Oral[Long$Patient==24 & Long$Time =="Week 06"]<-Long$Oral[Long$Patient==24 & Long$Time == "Week 04"]


#now return the numerical descriptive statistics (ITT analysis)

(Stats<-Long %>% group_by(Treatment, Time) %>% summarise("Sample Size"=n(), "Mean"=round(mean(Oral),2),
                                                         "Standard deviation"=round(sd(Oral),1),
                                                         "Median"=median(Oral),
                                                         "1st quartile"=quantile(Oral,0.25),
                                                         "3rd quartile"=quantile(Oral,0.75),
                                                         "Min" =min(Oral),
                                                         "Max" = max(Oral)))

t(Stats)

#STEP 03: Graphical descriptive statistics (ITT ANALYSIS)


library(ggplot2)
(g<-ggplot(Long,aes(x=Time, y=Oral, fill=Treatment))+stat_boxplot(geom = "errorbar")+
    geom_boxplot()+labs(x="", y = "Oral Condition"))
(g1<-g+coord_cartesian(ylim = c(3,19))+scale_y_continuous(breaks = seq(3,19,2))+
    theme(text = element_text(size = 15)))


#SECOND Check that the assumptions are not violated (ITT analysis)
#Step 01: tests of normality
library(psych)
(Norm <- Long %>% group_by(Treatment, Time) %>% summarise("Sample Size"=n(),
                                                          "Mean"= mean(Oral),
                                                          "Median"=median(Oral),
                                                          "Skewness"=skew(Oral),
                                                          "Normally Distributed"
                                                          =ifelse (shapiro.test(Oral)$p.value>0.05,"Yes", "No"),
                                                          "p-value" = round((shapiro.test(Oral)$p.value),4)))
t(Norm)


###HOMOGENITY OF VARIANCE
library(biotools) #needs data to be (matrix, factor)
###return Long to Wide
Wide<-spread(Long,Time,Oral)
boxM(Wide[,c(3:6)],Wide$Treatment)#needs data to be (matrix,factor)-- no missing values allowed


###THIRD Create a linear model and perform an ANOVA (ITT analysis)
#Option 01: If conditions placed on normality and homogenity of variances are violated
#then it might be necessary to transform the data

#Option:02 If conditions are not violated
library(ez)
Long$Patient<-as.factor(Long$Patient)
(res1<-ezANOVA(Long, dv= Oral, wid=Patient, between = Treatment, within = Time))


####FOURTH Effects plot (ITT analysis)
##Main effects
#Time

library(ggpubr)
ggline(Long, y="Oral", x ="Time", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y="95% CI of oral Condition",x="")+
  coord_cartesian(ylim = c(6,12)) +scale_y_continuous(breaks = seq(6,12,0.5))

#Treatment
library(ggpubr)
ggline(Long,y="Oral", x ="Treatment", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y="95% CI of oral Condition",x="")+
  coord_cartesian(ylim = c(7.5,10))+scale_y_continuous(breaks = seq(7.5,10,0.25))

#Simple Main Effects (interaction effects)
library(ggpubr)
ggline(Long, y="Oral", x ="Time",color = "Treatment", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y= "Mean oral Condition", x="")+
  coord_cartesian(ylim = c(5,14))+ scale_y_continuous(breaks = seq(5,14,1))


#### Fifth Pairwise Comparison
#Tukey
library(afex)
#not interested in the aov_car p-values....
#this is just an approach of doing pairwise comparisons
(res2<-aov_car(Oral ~ Time* Treatment +Error(Patient/Time), data=Long))
library(emmeans)
###Main effects
##Time
emmeans(res2, ~Time) %>% pairs(adjust="Tukey")

#Treatment
emmeans(res2, ~Treatment) %>% pairs (adjust="Tukey")

###Simple Main effect (interaction effect)

emmeans(res2, ~Time|Treatment) %>%pairs(adjust="Tukey")
emmeans(res2, ~Treatment|Time) %>%pairs(adjust="Tukey")


###Sixth Effect Size .... Obtained as ges when running the ezANOVA
(res1<-ezANOVA(Long,dv=Oral,wid=Patient,between = Treatment,within = Time))