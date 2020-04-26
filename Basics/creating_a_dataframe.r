### how to create a dataframe

Subject<-seq(1:7)
Gender<-c(rep("Male",3), NA, rep("Female", 3))
Age<-c(23,34,32,54,65,45,35)
(Weight<-c(67.5,NA,63.7,56.8,89.9,87.5,77.5))
Trial<-data.frame(Subject, Gender, Age, Weight)
View(Trial)



round(mean(Trial$Age),1)


mean(Trial$Weight) #wont output result becuase of the missing value

mean(Trial$Weight, na.rm = T)

