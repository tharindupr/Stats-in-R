library(readxl)
fitness_test<- read_excel("C:/Stats in R/workshop 2/Fitness.xlsx")

#### checking if a variable properties are correct
is.factor(Fitness$Gender) # FitnessSGender dataframe called first, then variable
Fitness$Gender <- as.factor(Fitness$Gender) # setting as a factor

is.factor(Fitness$Hand) #checking if variable is a factor
Fitness$Hand <- as.factor(Fitness$Hand) #sets variable as a factor

is.factor(Fitness$Exercise) #checking if variable is a factor
Fitness$Exercise<-as.factor(Fitness$Exercise)

is.factor(Fitness$Hand) #checking if variable is a factor
Fitness$Hand<-as.factor(Fitness$Hand)

is.numeric(Fitness$Age)
is.numeric(Fitness$Height)
is.numeric(Fitness$Pulse)

################## Qualitative data
############## Numerical
###### single variable
## frequency table
table(Fitness$Gender) # not a great table
## the following produces a nicer table
library(summarytools)
freq(Fitness$Gender) # can copy & paste to Excel
## then convert to table by selecting data - Text to columns

###### two variables
ctable(Fitness$Gender,Fitness$Exercise)

############## Graphical
######## Bar chart
#### single variable...gender
library(ggplot2)
(bc0<-ggplot(Fitness,aes(x=Gender))+geom_bar()+theme_light()) ### default ... needs bettering
### label and scale axes
(bc1<-bc0+labs(x = "Gender", y = "Frequency")+coord_cartesian(ylim=c(0, 55))+
    scale_y_continuous(breaks=seq(0,55,5)))
### might want to enlarge text
(bc2<-bc1+theme(text = element_text(size=15)))
### might want to label bars
(bc3<-bc2+ annotate("text", x = "Female", y = 41, label = "40",size=5)+
    annotate("text", x = "male", y = 55, label = "54",size=5))
### might want to change colours of bars
(bc4<-bc3+geom_bar(fill=c("red", "blue")))

#### two variable
(sbc0<-ggplot(Fitness,aes(x=Gender,fill=Exercise))+
    geom_bar()+theme_light()) ### default stacked bar chart
(cbc0<-ggplot(Fitness,aes(x=Gender,fill=Exercise))+
    geom_bar(position="dodge")+theme_light()) ### default clustered bar chart
### might want to better
(cbc1<-cbc0+labs(x = "Gender", y = "Frequency")+coord_cartesian(ylim=c(0, 40))+
    scale_y_continuous(breaks=seq(0,40,5))+theme(text = element_text(size=15)))


####### pie chart
### nothing specific in ggplot for pie chart
### a good alternative is creating a dataframe on frequency
gr<-c("Male","Female")
val<-c(54,40)
(pval<-round(val/sum(val)*100,1))
df<-data.frame(gr,pval)
(p<- ggplot(df, aes(x="", y=pval, fill=gr))+ geom_bar(width = 1, stat = "identity"))
(pie <- p + coord_polar("y", start=0)+ scale_fill_brewer(palette="Blues")+
    theme_minimal())

library(scales)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"),
    legend.text=element_text(size=14)
  )

(p0<-pie + scale_fill_brewer("Blues") + blank_theme + theme(axis.text.x=element_blank())+
    geom_text(aes(label = ifelse(pval < 0.05-max(pval), "", paste0(round(pval,1), "%"))),
              position = position_stack(vjust = 0.5),size=5.5)+
    theme(legend.title = element_blank())+
    labs(title="Gender"))

############## Graphical
######## Histogram
#### single variable...Age
range(Fitness$Age) # get an idea

(hg0<-ggplot(Fitness,aes(x=Age))+geom_histogram(breaks=seq(15,55,10)))
(hgl<-hg0+ labs(x = "Age (years)", y = "Frequency")+ #label the axes
    coord_cartesian(xlim=c(15,55),ylim=c(0, 80)) +scale_x_continuous(breaks=seq(15,55,10))+
    scale_y_continuous(breaks=seq(0,80,10))+ #scale the x, y-axes
    theme_bw()) # change outline

#### two variables...Age by gender
(hg00<-ggplot(Fitness,aes(x=Age))+geom_histogram(breaks=seq(15,55,10))+facet_grid(~Gender))
(hg01<-hg00+ labs(x = "Age (years)", y = "Frequency")+ #label the axes
    coord_cartesian(xlim=c(15,55),ylim=c(0, 50)) +scale_x_continuous(breaks=seq(15,55,10))+
    scale_y_continuous(breaks=seq(0,50,5))+ #scale the x, y-axes
    theme_bw()) # change outline

######## soxplot
#### single variable
(bp0<-ggplot(Fitness,aes(x="",y=Age))+stat_boxplot(geom = "errorbar")+geom_boxplot())
### now editting
(bp1<-bp0+labs(x = "", y = "Age (years)")+coord_cartesian(ylim=c(15,55))+
    scale_y_continuous(breaks=seq(15,55,5))+theme(text = element_text(size=11)))


############## Numerical
#### single statistics
### mean
mean(Fitness$Age) # average age
mean(Fitness$Age[Fitness$Gender=="Female"]) #average age of females
by(Fitness$Age,Fitness$Gender,mean) # average age of both genders
round(by(Fitness$Age,Fitness$Gender,mean),1) # round result to one decimal place

### median
median(Fitness$Age)
by(Fitness$Age,Fitness$Gender,median)

### standard deviation
sd(Fitness$Age)
by(Fitness$Age,Fitness$Gender,sd)

### interquartile range
IQR(Fitness$Age)
by(Fitness$Age,Fitness$Gender,IQR)

### output numerous descrptive statistics
library(psych)
describe(Fitness$Age) # various statistics for age
describeBy(Fitness$Age,Fitness$Gender) # various statistics for age by gender



### tailor output to your needs
library(dplyr)
(statsAge<-Fitness %>% summarise("sample size"=n(),mean = mean(Age),"standard deviation"=sd(Age),                Median = median(Age), "1st quartile"=quantile(Age, 0.25),
                                 Median = median(Age), "1st quartile"=quantile(Age, 0.25),
                                 "3rd quartile"=quantile(Age, 0.75), min=min(Age), Max=max(Age)))

## transpose output
t(statsAge)

## export to Excel
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.6')
library(xlsx)
write.xlsx(t(statsAge),"C:\\Users\\r00171555\\Desktop\\R_CIT\\Lab01\\oelete0.xlsx")

## split by gender
(statsAge_Gen<-Fitness %>% group_by(Gender) %>% summarise("sample size"=n(),mean = mean(Age),
                                                          "standard deviation"=sd(Age),
                                                          Median = median(Age), "1st quartile"=quantile(Age, 0.25),
                                                          "3rd quartile"=quantile(Age, 0.75), Min=min(Age), Max=max(Age)))

## transpose output
t(statsAge_Gen)

# need to do a test of normality to determine what statistics we should use.
shapiro.test(Fitness$Age)
by(Fitness$Age,Fitness$Gender,shapiro.test)

# output p-value only
(result<-by(Fitness$Age,Fitness$Gender,shapiro.test))
round(result$Female$p.value,4)
round(result$Male$p.value,4)
