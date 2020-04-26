#Repeated Measures

library(readxl)
Repeated <- read_excel("C:/Stats in R/Workshop 5/W5 Datasets V2.xlsx", sheet = "Repeated")
View(Repeated)

#1) Check the data properties , missing values etc
#### Append a subject ID.... from Workshop 03
(n<-dim(Repeated)[1]) # sample size # no. of rows it tell
Patient<-seq(1:n)
library(dplyr)
Repeated<-mutate(Repeated,Patient) #attach patient with dataframe

##### Ordering Variables (if desired) #put patient id in start
(cn<-dim(Repeated)[2])
Repeated<-Repeated[,c(cn,1:cn-1)]

##########################We have missing values in paired data, how u handle it coz its imp

######Select data of interest###### df,pid,
Rep<-select(Repeated,Patient,Baseline = `Oral condition at the initial stage`,
            "Week 02"=`Oral condition at the end of week 02`,
            "Week 04"= `Oral condition at the end of week 04`,
            "Week 06" = `Oral condition at the end of week 06`)
View(Rep)
######Convert to long format
library(tidyr)
(Long<-gather(Rep,Time,Oral,2:5))


####First Explore the data
###Step 01: Check properties
is.factor(Long$Time)
Long$Time<-factor(Long$Time,levels = c("Baseline","Week 02","Week 04", "Week 06")) #Specify order of levels
is.numeric(Long$Oral)

###Step 02: numerical descriptive statistics
#next line won't work because of missing data
(Stats<-Long %>% group_by(Time) %>% summarise("Sample Size"=n(), "Mean"=mean(Oral),
                                              "Standard deviation"=sd(Oral),
                                              "Median"=median(Oral),
                                              "1st quartile"=quantile(Oral,0.25),
                                              "3rd quartile"=quantile(Oral,0.75),
                                              "Min" =min(Oral),
                                              "Max" = max(Oral)))
view(Stats)

###locating missing value patients .... from workshop 03
Long[!complete.cases(Long),]


### Two options
# 1. per protocol (pp) analysis: remove patients from study and complete analysis with 23 patients
#Long23<-na.omit(Long) #remove patients with missing data

#2. Intention to Treat (ITT) analysis: impute data values for patients with
#missing values - e.g., LOCF
Long$Oral[Long$Patient==22 & Long$Time =="Week 06"]<-Long$Oral[Long$Patient==22 & Long$Time == "Week 04"]
Long$Oral[Long$Patient==24 & Long$Time =="Week 06"]<-Long$Oral[Long$Patient==24 & Long$Time == "Week 04"]


#now return the numerical descriptive statistics (ITT analysis)

(Stats<-Long %>% group_by(Time) %>% summarise("Sample Size"=n(), "Mean"=mean(Oral),
                                              "Standard deviation"=sd(Oral),
                                              "Median"=median(Oral),
                                              "1st quartile"=quantile(Oral,0.25),
                                              "3rd quartile"=quantile(Oral,0.75),
                                              "Min" =min(Oral),
                                              "Max" = max(Oral)))
t(Stats)