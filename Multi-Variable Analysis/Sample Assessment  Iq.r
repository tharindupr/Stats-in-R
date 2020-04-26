library(readxl)
Repeated <-read_excel("C:/Stats in R/Workshop 5/Sample Assessment 03 Dataset.xlsx")

### pre processing,
library(dplyr)
Rep<-select(Repeated,
            "Patient"=`patid`,
            "Gender"= `gender`,
            "Baseline"= `wgt0`,
            "Month 01"= `wgt1`,
            "Month 02"= `wgt2`,
            "Month 03"= `wgt3`,
            "Month 04"= `wgt4`)
View(Rep)


# Convert to long format
library(tidyr)
(Long<-gather(Rep,Time,Weight,3:7))


### FIRST: Exploring the data.
# Step 01: Check properties
is.factor(Long$Time)
Long$Time<-factor(Long$Time, levels = c("Baseline", "Month 01",
                                        "Month 02", "Month 03", "Month 04"))

Long$Gender<-factor(Long$Gender, levels = c("Female", "Male"))
is.numeric(Long$Weight)


# Step 02: numeric descriptive statistics
# next line won't work becuase of missing data

#Step 02: numerical descriptive statistics
(Stats<-Long %>% group_by(Gender, Time) %>% summarise("Sample Size"=n(), "Mean"=round(mean(Weight),1),
                                                      
                                                      #"Median"=median(Weight),
                                                      #"95% CI (lower)"= round(mean(Weight)-qt(0.975,df=n()-1)*sd(Weight)/sqrt(n()),1),
                                                      #"95% CI (upper)"= round(mean(Weight)+qt(0.975,df=n()-1)*sd(Weight)/sqrt(n()),1),
                                                      
                                                      "95% CI (lower)"= round(t.test(Weight,mu=0)$conf.int[1],2),
                                                      "95% CI (upper)"= round(t.test(Weight,mu=0)$conf.int[2],2),
                                                      "Standard deviation"=round(sd(Weight),2),
                                                      "1st quartile"=quantile(Weight,0.25),
                                                      "3rd quartile"=quantile(Weight,0.75),
                                                      "Min" =min(Weight),
                                                      "Max" = max(Weight)))
t(Stats)



#STEP 03: Graphical descriptive statistics (ITT ANALYSIS)

library(ggplot2)
(g<-ggplot(Long,aes(x=Time, y=Weight, fill=Gender))+stat_boxplot(geom = "errorbar")+
    geom_boxplot()+labs(x="", y = "Weight (lbs)"))
(g1<-g+coord_cartesian(ylim = c(140,260))+scale_y_continuous(breaks = seq(140,260,10))+
    theme(text = element_text(size = 15)))




#SECOND Check that the assumptions are not violated (ITT analysis)
#Step 01: tests of normality
library(psych)
(Norm <- Long %>% group_by(Gender, Time) %>% summarise("Sample Size"=n(),
                                                       "Mean"= round(mean(Weight),1),
                                                       "Median"=round(median(Weight),1),
                                                       "Skewness"=round(skew(Weight),3),
                                                       "Normally Distributed"
                                                       =ifelse (shapiro.test(Weight)$p.value>0.05,"Yes", "No"),
                                                       "p-value" = round((shapiro.test(Weight)$p.value),4)))
t(Norm)


###HOMOGENITY OF VARIANCE
library(biotools) #needs data to be (matrix, factor)
###return Long to Wide
Wide<-spread(Long,Time,Weight)
boxM(Wide[,c(3:7)],Wide$Gender)#needs data to be (matrix,factor)-- no missing values allowed





###THIRD Create a linear model and perform an ANOVA (ITT analysis)
#Option 01: If conditions placed on normality and homogenity of variances are violated
#then it might be necessary to transform the data

#Option:02 If conditions are not violated
library(ez)
Long$Patient<-as.factor(Long$Patient)
(res1<-ezANOVA(Long, dv= Weight, wid=Patient, between = Gender, within = Time))
(psphere<-res1$`Mauchly's Test for Sphericity`)
# verdict based on p-value
(p<-ifelse(psphere$p>0.05, "Yes","No"))
# produce "nice" summary
(`Sphericity assumed`<-paste0(p," (p = ",round(psphere$p,4),")")) #paste0() is like concatenate in Excel
# append to t(Norm) as a new row - i.e., row bind
(new<-rbind(t(Norm),`Sphericity assumed`))


####FOURTH Effects plot (ITT analysis)
##Main effects
#Time

library(ggpubr)
ggline(Long, y="Weight", x ="Time", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y="95% CI of Weights",x="")

#Treatment
library(ggpubr)
ggline(Long,y="Weight", x ="Gender", add = c("mean_ci"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y="95% CI of Weights",x="")


#Simple Main Effects (interaction effects)
library(ggpubr)
ggline(Long, y="Weight", x ="Time",color = "Gender", add = c("mean"),size = 1)+theme_gray()+
  theme(text = element_text(size = 15))+
  labs(y= "Mean Weights", x="")
########################
library(ggpubr)
(mp<-ggline(Long,y="Weight", x ="Time", color= "Gender",add = c("mean_ci"),size = 1)+theme_gray())
(g2<-mp+ theme(text=element_text(size = 15))+
    labs(y="95% CI of Weight (lbs)", x="")+
    coord_cartesian(ylim = c(140,240))+scale_y_continuous(breaks = seq(140,240,10)))

#MEAN PLOT BETTER DESCRIPTION WHERE DIFF ARE , R CONDITION INC
library(gridExtra)
windows(16,10)
grid.arrange(g1,g2,ncol=2)

#coord_cartesian(ylim = c(5,14))+ scale_y_continuous(breaks = seq(5,14,1))


#### Fifth Pairwise Comparison
#Tukey
library(afex)
#not interested in the aov_car p-values....
#this is just an approach of doing pairwise comparisons
(res2<-aov_car(Weight ~ Time* Gender +Error(Patient/Time), data=Long))
library(emmeans)
###Main effects
##Time
emmeans(res2, ~Time) %>% pairs(adjust="Tukey")

#Gender
emmeans(res2, ~Gender) %>% pairs (adjust="Tukey")

###Simple Main effect (interaction effect)

emmeans(res2, ~Time|Gender) %>%pairs(adjust="Tukey")

emmeans(res2, ~Gender|Time) %>%pairs(adjust="Tukey")


###Sixth Effect Size .... Obtained as ges when running the ezANOVA
(res1<-ezANOVA(Long,dv=Weight,wid=res2,between = Gender,within = Time))
