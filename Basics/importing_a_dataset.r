library(readxl)

Fitness <- read_excel("C:/Stats in R/workshop 2/Fitness.xlsx")

View(Fitness)


is.factor(Fitness$Gender)  
Fitness$Gender<-as.factor(Fitness$Gender) 


is.factor (Fitness$Hand) 
Fitness$Hand <- as.factor(Fitness$Hand)  



is.factor(Fitness$Exercise)  
Fitness$Exercise <- as.factor(Fitness$Exercise)


is.numeric(Fitness$Age)
is.numeric(Fitness$Height)
is.numeric(Fitness$Pulse)


### create a frequency table
addmargins(table(Fitness$Gender))



library(summarytools)

freq(Fitness$Gender)

ctable(Fitness$Gender, Fitness$Exercise)



### graphical 

library(ggplot2)

### bar chart

(bc0<-ggplot(Fitness,aes(x=Gender))+geom_bar()+theme_light())

###label and scale axes
(bc1<-bc0+labs(x="Gender", y="Frequency")+coord_cartesian(ylim=c(0,55))+scale_y_continuous(breaks=seq(0,55,5)))

### enlarge the text
(bc2<-bc1+theme(text=element_text(size=15)))


### labeling the bars
(bc3<-bc2+annotate("text", x="Female", y=41, label="40", size=5)+annotate("text", x="Male", y=55, label="54", size=5))


### changing the colors
(bc4<-bc3+geom_bar(fill=c("red", "blue")))


### two variables
(sbc0<-ggplot(Fitness, aes(x=Gender, fill=Exercise))+geom_bar()+theme_light())

(cbc0<-ggplot(Fitness, aes(x=Gender, fill=Exercise))+geom_bar(position="dodge")+theme_light())


### more better

(cbc1<-cbc0+labs(x="Gender", y="Frequency")+coord_cartesian(ylim=c(0,40))+ 
    scale_y_continuous(breaks=seq(0,40,5))+theme(text=element_text(size=15)))



### histogram on age


range(Fitness$Age)

(hg0<-ggplot(Fitness,aes(x=Age))+geom_histogram(breaks=seq(15,55,10)))
(hg1<-hg0+labs(x="Age", y="Frequency")+
    coord_cartesian(xlim=c(15,55), ylim=c(0,80))+ scale_x_continuous(breaks=seq(15,55,5))+
    scale_y_continuous(breaks=seq(0,80,10))+theme_bw())


### two variable
(hg0<-ggplot(Fitness,aes(x=Age))+geom_histogram(breaks=seq(15,55,10))+facet_grid(~Gender))
(hg1<-hg0+labs(x="Age (Years)", y="Frequency")+
    coord_cartesian(xlim=c(15,55), ylim=c(0,80))+ scale_x_continuous(breaks=seq(15,55,10))+
    scale_y_continuous(breaks=seq(0,80,10))+theme_bw())

### box plot

(bp0<-ggplot(Fitness, aes(x="", y=Age))+stat_boxplot(geom="errorbar")+geom_boxplot())


(bp0<-ggplot(Fitness, aes(x=Gender, y=Age))+stat_boxplot(geom="errorbar")+geom_boxplot())




### Numerical 

mean(Fitness$Age)
mean(Fitness$Age[Fitness$Gender=="Female"])

by1<-by(Fitness$Age, Fitness$Gender, mean)

round(by1, 1)

median(Fitness$Age)

by(Fitness$Age, Fitness$Gender, sd)

IQR(Fitness$Age)


library(psych)

describe(Fitness$Age)


