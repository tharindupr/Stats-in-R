library(readxl)
Repeated <- read_excel("C:/Stats in R/Assesment 3/Assessment 03 Dataset 2020.xlsx")
View(Repeated)

######################### pre processing,


## append a subject ID... 
(n<- dim(Repeated[1])) # sample-size
Subject<-(1:n)
library(dplyr)
Repeated<-mutate(Repeated,Subject)


####Ordering variables (if desired)
(cn<-dim(Repeated)[2])
Repeated<-Repeated[,c(cn,1:cn-1)]
View(Repeated)


library(dplyr)
Rep<-select(Repeated,
            "Subject"=`Subject`,
            "Group"= `Group`,
            "Baseline"= `Baseline`,
            "Month 01"= `1 month`,
            "Month 02"= `2 months`)

View(Rep)

# Convert to long format
library(tidyr)
(Long<-gather(Rep,Time,Cholesterol_Level,3:5))


######################### FIRST:EXPORE DATA


### FIRST: Exploring the data.
# Step 01: Check properties
is.factor(Long$Time)
Long$Time<-factor(Long$Time, levels = c("Baseline", "Month 01",
                                        "Month 02"))

Long$Group<-factor(Long$Group, levels = c("Exercise & Diet", "Diet"))
is.numeric(Long$Cholesterol_Level)



#Step 02: numerical descriptive statistics
(Stats<-Long %>% group_by(Group, Time) %>% summarise("Sample Size"=n(), 
                                                     "Mean"=round(mean(Cholesterol_Level),1),
                                                      "95% CI (lower)"= round(t.test(Cholesterol_Level,mu=0)$conf.int[1],2),
                                                      "95% CI (upper)"= round(t.test(Cholesterol_Level,mu=0)$conf.int[2],2),
                                                      "Standard deviation"=round(sd(Cholesterol_Level),2),
                                                      "1st quartile"=quantile(Cholesterol_Level,0.25),
                                                      "3rd quartile"=quantile(Cholesterol_Level,0.75),
                                                      "Min" =min(Cholesterol_Level),
                                                      "Max" = max(Cholesterol_Level)))
t(Stats)



#STEP 03: Graphical descriptive statistics (ITT ANALYSIS)

library(ggplot2)
(g<-ggplot(Long,aes(x=Time, y=Cholesterol_Level, fill=Group))+stat_boxplot(geom = "errorbar")+
    geom_boxplot()+labs(x="", y = "Cholesterol_Level (mg/dL)"))
(g1<-g+coord_cartesian(ylim = c(165,250))+scale_y_continuous(breaks = seq(165,250,10))+
    theme(text = element_text(size = 15)))



######################### SECOND Check that the assumptions are not violated (ITT analysis)


#Step 01: tests of normality
library(psych)
(Norm <- Long %>% group_by(Group, Time) %>% summarise("Sample Size"=n(),
                                                       "Mean"= round(mean(Cholesterol_Level),1),
                                                       "Median"=round(median(Cholesterol_Level),1),
                                                       "Skewness"=round(skew(Cholesterol_Level),3),
                                                       "Normally Distributed"
                                                       =ifelse (shapiro.test(Cholesterol_Level)$p.value>0.05,"Yes", "No"),
                                                       "p-value" = round((shapiro.test(Cholesterol_Level)$p.value),4)))
t(Norm)


###HOMOGENITY OF VARIANCE
library(biotools) #needs data to be (matrix, factor)
###return Long to Wide
Wide<-spread(Long,Time,Cholesterol_Level)
boxM(Wide[,c(3:5)],Wide$Group)#needs data to be (matrix,factor)-- no missing values allowed




######################### THIRD Create a linear model and perform an ANOVA (ITT analysis)
#Option 01: If conditions placed on normality and homogenity of variances are violated
#then it might be necessary to transform the data

#Option:02 If conditions are not violated
library(ez)
Long$Subject<-as.factor(Long$Subject)
(res1<-ezANOVA(Long, dv= Cholesterol_Level, wid=Subject, between = Group, within = Time))



######################### FOURTH Effects plot (ITT analysis)
##Main effects
#Time

library(ggpubr)
ggline(Long, y="Cholesterol_Level", x ="Time", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y="95% CI of Cholesterol_Level (mg/dL)",x="")

#Treatment
library(ggpubr)
ggline(Long,y="Cholesterol_Level", x ="Group", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y="95% CI of Cholesterol_Level (mg/dL)",x="")


#Simple Main Effects (interaction effects)
library(ggpubr)
(g2<-ggline(Long, y="Cholesterol_Level", x ="Time",color = "Group", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y= "95% CI of Cholesterol_Level (mg/dL)", x=""))


library(gridExtra)
windows(16,10)
grid.arrange(g1,g2,ncol=2)





######################### Fifth Pairwise Comparison
#Tukey
library(afex)
#not interested in the aov_car p-values....
#this is just an approach of doing pairwise comparisons
(res2<-aov_car(Cholesterol_Level ~ Time* Group +Error(Subject/Time), data=Long))
library(emmeans)
###Main effects
##Time
emmeans(res2, ~Time) %>% pairs(adjust="Tukey")

#Gender
emmeans(res2, ~Group) %>% pairs (adjust="Tukey")

###Simple Main effect (interaction effect)

emmeans(res2, ~Time|Group) %>%pairs(adjust="Tukey")

emmeans(res2, ~Group|Time) %>%pairs(adjust="Tukey")


###Sixth Effect Size .... Obtained as ges when running the ezANOVA
(res1<-ezANOVA(Long,dv=Cholesterol_Level,wid=res2,between = Group,within = Time))



