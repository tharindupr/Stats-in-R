library(readxl)
AS <- read_excel("C:/Stats in R/Assesment 4/Sample Assessment 04 Dataset.xlsx")
#View(AS)

#names(FD)<-c("Distance", "Damage")

#First Format and Explore the data
#Step 01: format the data
(n<-dim(AS)[1]) #SAMPLE SIZE
CaseNum<-(1:n)
library(dplyr)
AS<-mutate(AS,CaseNum)

###ordering variables
(cn<-dim(AS)[2])
AS<-AS[,c(cn,1:cn-1)]

##Step 02: Check properties
prop<-c() # setting up a vector variable
for(i in 2:cn)
{
  prop[i]<-is.numeric(AS[[i]])
}
prop


#Step 03: Explore the data
#numerical descriptive analysis
#stacking data is the easiest way to manage this
library(tidyr)
Long<-gather(AS,Variable,Value,2:6)
Stats<-Long %>% group_by(Variable) %>% summarise("Sample Size"=n(),
                                                 "Mean"=round(mean(Value),2),
                                                 #"95% CI (lower)"= round(t.test(Value,mu=0)$conf.int[1],2),
                                                 # "95% CI (upper)"= round(t.test(Value,mu=0)$conf.int[2],2),
                                                 "Standard deviation"=round(sd(Value),3),
                                                 "Median"= round(median(Value),2),
                                                 "1st quartile"=round(quantile(Value,0.25),2),
                                                 "3rd quartile"=round(quantile(Value,0.75),2),
                                                 "Min" =round(min(Value),2),
                                                 "Max" =round(max(Value),2))
t(Stats)



## SECOND Scatterplot (using original dataframe)
library(tidyr)
library(ggplot2)
(gs<-AS[-1] %>% gather(-`Sales of Men's Clothing`, key=var, value = "value") %>%
    ggplot(aes(x=value, y = `Sales of Men's Clothing`))+ geom_point()+
    facet_wrap(~ var, scales = "free")+theme_bw())
gs+labs(x="Predictors", y = "Sales of Men's Clothing (???'000)")+
  geom_smooth(method = "lm")+ #includes regression line
  theme(text = element_text(size = 15))

###THIRD CORRELATION
#Step 01: Tests of normality (return to long format)
# H0: No difference exists between the measurements and hypothetical distribution
#HA: A difference exists between the measurements and hypothetical distribution
#######Reason------ Now check that given measurements are ND or not,
#to determine the correlation coefficient that we will use...
# if ND =Yes Do Peasrson Correlation Coeffiecient (parametric test) otherwise if not ND
#DO spearmans rank coorelation (non-parametric test)
library(psych)
(Norm <- Long %>% group_by(Variable) %>% summarise("Sample Size"=n(),
                                                   "Mean"= round(mean(Value),2),
                                                   "Median"=round(median(Value),2),
                                                   "Skewness"=round(skew(Value),3),
                                                   "Normally Distributed"
                                                   =ifelse (shapiro.test(Value)$p.value>0.05,"Yes", "No"),
                                                   "p-value" = round((shapiro.test(Value)$p.value),4)))
t(Norm)
library(xlsx)
#write.xlsx(t(Norm),
#"C:/Users/R00194707/Desktop/Data Analysis with R/Workshop 6/W6part2.xlsx")

##STEP 02: Correlation TEST
library(psych)
#[-1] excludes CaseNum
# by running corr.test() we get two matrices 1) Correlation Matrix 2) the corresponding probabilities
(res1<-corr.test(AS[,-1])) #defaults to Pearson #res1 is our correlation matrix

#correlation of response variable with predictors
(out<-rbind(t(round(res1$r[1,-1],4)),t(round(res1$p[1,-1],4))))
# H0: No correlation exists between two variables------------------------****
#HA: A correlation exists between two variables---------------------------*****
# p<=alpha then Reject H0 i.e. reject the claim that no correlation exists b/w 2 var
# if p>alpha then fail to reject H0 i.e., fail to reject the claim that no correlation exists b/w 2 var
#res1$r[1,-1] # it remove annual sales from start

###investigate if muti-collinearity present
(cm<-corr.test(AS[,3:6])$r) #focuses on predictors only #runoff the correlation on explanatory variables
# correlation matrix of predictors #not needed on report
library(Matrix)
round(tril(cm),4) #low triangular -----------important #cm is correlation matrix

#write.xlsx()


###FOURTH Regression (if correlation is significant)
#for ease of typing make name of variables shorter
names(AS)<-c("CaseNum", "Sales", "Pages","PhoneLines","Printing","CSR")


#Full Model-----------------------***********
#H0: No di???erence exists between the slope of the regression line and an horizontal line (i.e., ?? = 0).
#H1: A di???erence exists between the slope of the regression line and an horizontal line (i.e., ?? 6= 0).
fit1<-lm(Sales~Pages+PhoneLines+Printing+CSR, AS) #lm(Sales~. , data = AS[,-1])
summary(fit1)

#Check for multi-collinearity
library(faraway)
(v1<-vif(fit1))

#remove Outlets
fit2<-lm(Sales~RA+PI+AA+Supervisors,AS) #lm(Sales ~. , data = AS[,c(-1,-3)])
summary(fit2)
(v2<-vif(fit2))

#remove insignificant contributor
fit3<-lm(Sales~RA+PI+AA,AS) #lm(Sales ~. , data = AS[,c(-1,-3,-7)])
summary(fit3)
(v3<-vif(fit3))

###summarising steps to selecting model
Names<-c("Intercept","No. retail outlets", "No. registered automobiles", "Personal income",
         "Average age of autos", "No. supervisors")
Model<-c(rep(1,6), rep(2,5), rep(3,4))

#adjusted r squared output
r2_per1<-paste0(round(summary(fit1)$adj.r.squared*100,2), "%")
r2_per2<-paste0(round(summary(fit2)$adj.r.squared*100,2), "%")
r2_per3<-paste0(round(summary(fit3)$adj.r.squared*100,2), "%")
rsquared<-c(r2_per1,rep("",5), r2_per2, rep("",4), r2_per3, rep("",3))

Variables<-c(Names, Names[-2], Names[c(-2,-6)])

#regression coefficients
Coeff<-c(summary(fit1)$coefficients[,1],summary(fit2)$coefficients[,1],summary(fit3)$coefficients[,1])
Coeff<-round(Coeff,4)


#p-values
pvalues<-c("", ifelse(summary(fit1)$coefficients[-1,4]<0.0005,"<0.0005",
                      round(summary(fit1)$coefficients[-1,4],4)),
           "", ifelse(summary(fit2)$coefficients[-1,4]<0.0005,"<0.0005",
                      round(summary(fit2)$coefficients[-1,4],4)),
           "", ifelse(summary(fit3)$coefficients[-1,4]<0.0005,"<0.0005",
                      round(summary(fit3)$coefficients[-1,4],4)))
VIF<-c("", round(v1,4),"",round(v2,4),"",round(v3,4))

(Frame<-data.frame(Model,rsquared,Variables, Coeff, pvalues, VIF))

#Standardised regression coefficients to compare coefficients
library(QuantPsyc)
(src1<-lm.beta(fit1)) #model 01
(src2<-lm.beta(fit2)) #model 02
(src3<-lm.beta(fit3)) #model 03

#combine measurements into one vector/variables
Standardised<-c("",round(src1,4),"",round(src2,4),"",round(src3,4))
Frame<-mutate(Frame,Standardised) #append to model selection dataframe
### re-order variables
(Frame<-Frame[,c(1:4,7,5:6)])

####################Stepwise Regression
library(MASS)
#uses fit1 -i.e., the full model
fit4 <- stepAIC(fit1, direction = "both", trace = F) #CHOOSE the best model by AIC
summary(fit4)
##############################


coefficients(fit3) #model coefficients
confint(fit3, level = 0.95) #CIs for model parameters
##############################################################

#Sample output of overall result

Labels<-c("Overall", "Intercept", "No. registered automobiles", "Personal income", "Average age of automobiles")
(output<-sumary(fit3))
########################Overall
##################adjusted r squared
(r2<-output$adj.r.squared)
(r2_per<-paste0(round(r2*100,2),"%"))
(adj_r<-c(r2_per,"","","",""))
######################################### regression coefficients
(Coeff<-c("",round(coefficients(fit3),4)))
#################################### 95% CI (lower)
(Lower<-c("","",round(confint(fit3, level = 0.95)[-1,1],4)))
#################################### 95% CI (upper)
(Upper<-c("","",round(confint(fit3, level = 0.95)[-1,2],4)))

###############################Standardised regression coefficients
(Stand_Coeff<-c("","",round(src3,4)))
####################################### p-values
##########################overall p-values
lmp<- function(modelobject){
  if (class(modelobject)!="lm") stop("Not an object of class 'lm' ")
  f<- summary(modelobject)$fstatistic
  p<- pf(f[1],f[2],f[3],lower.tail = F)
  attributes(p) <- NULL
  return(p)
}
(p<-ifelse(lmp(fit3)<0.0005, "<0.0005", round(lmp(fit3),4)))

#####################individual p-values
(pvalues<-c(p,"",
            ifelse(output$coefficients[-1,4]<0.0005,"<0.0005",round(output$coefficients[-1,4],4))))
(df<-data.frame(Labels,adj_r,Coeff,Lower,Upper,Stand_Coeff,pvalues))


#Residuals
#normal
range(residuals(fit3)) #to have idea of the spread of data # breaks in histogram
#fortify () converts fit to a dataframe

(mn<-ggplot(fortify(fit3),aes(x=.resid))+geom_histogram(breaks=seq(-2,2,1)))

(final_mn<-mn+labs(x="Residuals (???'000)", y="Frequency")+
    coord_cartesian(xlim = c(-2,2))+scale_x_continuous(breaks = seq(-2,2,1))+
    geom_vline(xintercept = 0, linetype= "dashed", color= "green", size =1)+ #different from zero
    theme(text = element_text(size = 15)))

shapiro.test(residuals(fit2)) #normality of residuals
# H0: No difference exists between the residuals and hypothetical distribution
#HA: A difference exists between the residuals and hypothetical distribution

t.test(residuals(fit2), mu=0) #different from zero?
# H0: No difference between measurement about zero (measurement on average is zero)
# H1: Difference exists between measurements about zero
#Random
#fortify() converts fit to a dataframe
(mr<-ggplot(fortify(fit3), aes(x= .fitted, y = .resid)) + geom_point()) #better looking
(final_mr <- mr+labs(x="Fitted (???'000)" , y = "Residuals (???'000)")+
    coord_cartesian(xlim = c(5,45), ylim = c(-2,2))+scale_x_continuous(breaks = seq(5,45,5))+
    scale_y_continuous(breaks = seq(-2,2,1))+
    geom_hline(yintercept = 0, linetype = "dashed", color = "green", size =1)+
    theme(text = element_text(size = 15)))


library(lmtest)
#HO = there is no autocorrelation among the residuals
# ( that means the residuals are not related to themselves, that means residuals are random)
#HA= there is auto-correlation among the residuals
#(which means that there is a pattern among residuals, which means residuals are not random)
# we like the H0, We hope to fail to reject the H0 P>0.05, we want
#our residuals are random, there is no autocorrelation among them, there is no pattern among them, good thing
dwtest(fit3) #test for randomness of rsiduals

#plot assumptions together
library(gridExtra)
windows(16,10)
grid.arrange(final_mn,final_mr)
# our model statistfied the assumptions

######################################
#sample output
nd<-round(shapiro.test(residuals(fit3))$p.value, 4)
zm<-round(t.test(residuals(fit3),mu=0)$p.value,4)
dw<-round(dwtest(fit3)$p.value,4)

Statistics<-c("Normally Distributed", "Zero mean", "Random")
Outcome<-c(ifelse(nd>0.05, "Yes", "No"),
           ifelse(zm>0.05,"Yes", "No"),
           ifelse(dw>0.05,"Yes", "No"))
p_value<-c(nd,zm,dw)
(DF<-data.frame(Statistics,Outcome,p_value))