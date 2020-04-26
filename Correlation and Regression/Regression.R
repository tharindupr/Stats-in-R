library(readxl)
FD <- read_excel("C:/Stats in R/Workshop 06/W6 Dataset-1.xlsx", 
                           sheet = "Example01")
View(FD)


# FIRST format explore the data
# Step 01: format the data
# change Fire Damage names to make script more manageable (if desired)
names(FD)<-c("Distance", "Damage")


### append a case number ... from workshop 03
(n<-dim(FD)[1]) # sample size
caseNum<-seq(1:n)

library(dplyr)
FD<-mutate(FD, caseNum)

#### ordering the variables

(cn<-dim(FD)[2])
FD<-FD[,c(cn, 1:cn-1)]

# Step 02: check properties
prop<-c() # setting up a vector/variable
for(i in 2:cn){
  prop[i]<-is.numeric(FD[[i]])
}
prop

# Step 03: Explore the data
### Stacking data easiest way to manage this.
library(tidyr)
Long<-gather(FD, Variable, Value, 2:3)

(Stats<-Long %>% group_by(Variable) %>% summarise("Sample Size"=n(), 
                                                     "Mean"=round(mean(Value),1),
                                                     "Standard deviation"=round(sd(Value),2),
                                                     "1st quartile"=quantile(Value,0.25),
                                                     "3rd quartile"=quantile(Value,0.75),
                                                     "Min" =min(Value),
                                                     "Max" = max(Value)))
t(Stats)




# SECOND scatterplot (using original datafrane) 
library(ggplot2) 
(g<-ggplot(FD,aes(x=Distance,y=Damage))+geom_point()) 
(g1<-g+labs(x="Distance (km)", y="Fire Damage (???'000)")+ #label axes 
    coord_cartesian(xlim=c(0.5,6.5),ylim=c(5,40))+ #scale axes 
    scale_x_continuous(breaks=seq(0.5,6.5,0.5))+ 
    scale_y_continuous(breaks=seq(5,40,5))) 
(g2<-g1+theme(text = element_text(size=15), # increasing font size 
             axis.text.x = element_text(angle = 45, hjust = 1))) # rotating angle of text 
g2+geom_smooth(method=lm,se=T) # including regression line 



# THIRD correlation

#Step 01: tests of normality
library(psych)
(Norm <- Long %>% group_by(Variable) %>% summarise("Sample Size"=n(),
                                                      "Mean"= round(mean(Value),1),
                                                      "Median"=round(median(Value),1),
                                                      "Skewness"=round(skew(Value),3),
                                                      "Normally Distributed"
                                                      =ifelse (shapiro.test(Value)$p.value>0.05,"Yes", "No"),
                                                      "p-value" = round((shapiro.test(Value)$p.value),4)))
t(Norm)

# step 02 : correlation test

library(psych)

corr.test(FD)

 # [,1] excludes CaseNum
(res1<-corr.test(FD[, -1])) #defualts to Pearson and displayes correlation and p-value
(res2<-corr.test(FD[,-1], method="spearman")) #specify non parametrict test, if required

round(res1$r[1,2],4)
ifelse(res1$p[1,2]<0.0001, "<0.0001", round(res1$p[1,2],4))

# FOURTH regression (if correlation is significant)
fit<-lm(Damage~Distance, FD) #returns regression coefficients

summary(fit) #returns more info


coefficients(fit)  #model coefficients/parameters
confint(fit, level=0.95) #CIs for model coefficients/parameters
round(fitted(fit), 2) #predicted values
round(residuals(fit), 2) #residuals



#######################
# sample output
Measures<-c("Pearson correlation coefficient", "R^2", "Regression coefficients",
            "Intercept", "Fire Damage()", "95 % CI (lower)", "95 % CI (upper)")

res1 #correlation output
(r<-round(res1$r[2],4)) 
(p<-ifelse(res1$p[2]<0.0005, "<0.0005", round(res1$p[2], 4)))

(r2<-summary(fit)$r.squared)
(r2_per<-paste0(round(r2*100, 2), "%"))
(CI_Lower <- round(confint(fit, level=0.95)[2],4))
(CI_Upper <- round(confint(fit, level=0.95)[4],4))

(Intercept<-round(coefficients(fit)[1], 4))
(Slope<-round(coefficients(fit, level=0.95)[2], 4))
Statistics<-c(r, r2_per, "", Intercept, Slope, CI_Lower, CI_Upper)
p_value<-c(p, "","", "", p, "", "")

(df<-data.frame(Measures, Statistics, p_value))



# FIFTH Residuals
# normal
range(residuals(fit)) # to have an idea of the spead of the data from # breaks in histogram
# fortify() converts fit to a dataframe

(n<-ggplot(fortify(fit), aes(x=.resid)) + geom_histogram(breaks=seq(-3.5, 3.5, 1)))
(final_n<-n+labs(x="Residuals (e'000')", y="Frequency")+
    coord_cartesian(xlim=c(-3.5, 3.5)) + scale_x_continuous(breaks=seq(-3.5, 3.5, 1))+
    geom_vline(xintercept = 0, linetype="dashed", color="green", size=1)+ #different from 0
    theme(text = element_text(size=15)))

shapiro.test(residuals((fit))) #normality of residuals
t.test(residuals(fit), mu=0) # different from zero?

# Random 
# fortify() converts fit to a dataframe 
(r<-ggplot(fortify(fit), aes(x = .fitted, y = .resid)) + geom_point()) #better looking 
(final_r<-r+labs(x="Fitted (???'000)", y="Residuals (???1000)")+
    coord_cartesian(xlim=c(5,40),ylim=c(-3.5,3.5))+scale_x_continuous(breaks=seq(5,40,5))+ 
    scale_y_continuous(breaks=seq(-3.5,3.5,1))+ 
    geom_hline(yintercept=0, linetype="dashed", color = "green", size=1)+ 
    theme(text = element_text(size=15))) 

library(lmtest)
dwtest(fit) #randomness of residuals


#plot assumption together
library(gridExtra)
windows(16, 10)
grid.arrange(final_n, final_r)
