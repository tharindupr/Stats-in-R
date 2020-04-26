library(readxl)
Diet<- read_excel("C:\\Users\\r00171555\\Desktop\\R_CIT\\Lab02\\Diet_study.xlsx")

#Indexing ...rows*columns
Diet[2,3]
Diet[4:7,5]
Diet[1:5,4:5]
Diet[2,]
Diet[,3]
Diet[,-3]
Diet[Diet$Gender=="Female",]

#Sorting/ranking a dataframe
ascend<-Diet[order(Diet$tg0),]
descend<-Diet[order(-Diet$tg0),]
(ascend2<-Diet[order(Diet$gender,Diet$tg0),])

#Add a patient ID variable
n<-dim(Diet)[1]
Patient<-LETTERS[1:n]
library(dplyr)
Diet<-mutate(Diet, Patient)

#### ordering variables (if desired)  
dim(Diet)[2]
Diet<-Diet[,c(13,1:12)]

#### renaming variables (if desired)  
library(data.table)
setnames(Diet,old="gender",new="Gender")

############# checking data types
## issue with categorical variable
is.factor(Diet$Gender) # returns FALSE
Diet$Gender<-as.factor(Diet$Gender)
table(Diet$Gender) # mixed classification
Diet$Gender[Diet$Gender=="M"]<-"Male"
table(Diet$Gender) ## M still present due to when Gender was declared as a factor
Diet$Gender<-factor(Diet$Gender,labels=c("Female","Male"))

## issue with quantitative variable
is.numeric(Diet$age) # returns FALSE
table(Diet$age) # figure out what the problem is
Diet$age[Diet$age=="sixty"]<-60 # replacing data  
Diet$age<-as.numeric(Diet$age)

### might want to create a grouping variable for age  
Age_group<-ifelse(Diet$age<50,1,ifelse(Diet$age<60,2,3)) 

## append of Diet dataframe
library(dplyr)
Diet<-mutate(Diet,Age_group)
View(Diet)

## define new variable as a factor
Diet$Age_group<-factor(Diet$Age_group,levels=c(1,2,3),labels=c("40-49","50-59","60-69"))
### for example the relationship between age and gender  
(t1<-table(Diet$Gender,Diet$Age_group))
### might want to merge columns  
(new<-cbind(t1[4], t1[,2]+t1[,3]))
### might want to label headings of table  
(new_df<-data.frame(new))
names(new_df)<-c("40-49","50-69")
new_df
## locating missing value patients and outliers
#	where are the missing values
Diet[!complete.cases(Diet),]
#	replace by LOCF
Diet$tg2[Diet$Patient=="0"]<-Diet$tg1[Diet$Patient=="0"]


#	where are the outliers
#	check variables individually
ggplot(Diet,aes(y=tg0))+stat_boxplot(geom = "errorbar")+geom_boxplot() 
ggplot(Diet,aes(y=tg1))+stat_boxplot(geom = "errorbar")+geom_boxplot()


#	check collectively through a sequence of scatteplots
#	sequence of boxplots
windows(20,16) # output plots in a separate window  
par(mfrow=c(2,5)) # outline the structure of the plots
for(i in seq(3, length(Diet), 1)) boxplot(Diet[[i]],xlab = names(Diet[i]))

#	what is the exact outlier value for tg1?  
Diet$tg1[Diet$tg1>200]
#	replace 1030 with 103
Diet$tg1[Diet$tg1-1030]<-103


############# filtering data frame  
## subset for Males only
library(dplyr)
Males<-filter(Diet,Gender == "male")
## subset for Females that are less than 55 years old  
Females<-filter(Diet,Gender=="Female" & age < 55)
## selecting some variables only  
Trig<-select(Diet,Patient,Gender,tg0,tg1,tg2,tg3,tg4)  
Trig<-select(Diet,Patient,Gender,"Baseline"=tg0,"1 month"=tg1,"2 months"=tg2,
             "3 months"=tg3,"4 months"=tg4) # can give variables new names
## change from wide to long - i.e., stack the measurements...
### often needed for repeated measures test
library(tidyr)
Long<-gather(Trig,Time,Triglyceride,3:7)
#	(dataframe,key,name of new stacked variable,columns to be stacked)
#	other variables automatically taken care of
## change from long to wide - i.e., reverse of previous  
wide<-spread(Long,Time,Triglyceride) # (dataframe,key,value)...
### other variables automatically taken care of
