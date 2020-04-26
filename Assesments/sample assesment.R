
### creating the dataframe
airport<-c(rep("Shanon",20), rep("Cork",20))
quality<-c(11,13, 18, 3, 15, 9, 20, 15, 17, 13,
           17, 13, 12, 24, 7, 5, 17, 10, 8, 14,
           11, 10, 20, 4, 12, 8, 16, 14, 13, 15,
           23, 11, 16, 22, 6, 12, 17, 7, 12, 19)


df<-data.frame(airport, quality)


### frequency distribution table

level<-cut(df$quality,c(0,5,10,15,20,25))



library(dplyr)
level<-ifelse(df$quality<5,1,ifelse(df$quality<10,2,ifelse(df$quality<15,3,ifelse(df$quality<20,4,5))))

df<-mutate(df,level)
View(df)
#### define new variable as a factor
df$level<-factor(df$level, levels=c(1,2,3,4,5),labels=c("0-5","5-10","10-15","15-20","20-25"))


View(df)

ctable(df$airport, df$level)


### creating the box plot
library(ggplot2)
(bp1<-ggplot(df,aes(x=airport,y=quality))+stat_boxplot(geom = "errorbar")+geom_boxplot())

(bp1<-bp1+labs(x = "Airport", y = "Air quality (ppm)"))




### describing data
library(dplyr)
(stats<-df %>% group_by(airport) %>%  summarise("sample size"=n(), Mean = mean(quality),
                            "Standard Deviation" = sd(quality),
                            Median = median(quality),
                            "1st quartile" = quantile(quality,0.25),
                            "3rd quartile" = quantile(quality,0.75),
                            Min = min(quality),
                            Max = max(quality),
                            Skewness = 3*(Mean-Median)/sd(quality),
                            "Normally Distributed"=if(Skewness<1&&Skewness>-1){"Yes"}else{"No"}))

t(stats)




### p value test
# need to do a test of normality to determine what statistics we should use.
shapiro.test(df$quality)
by(df$quality,df$airport,shapiro.test)

# output p-value only
(result<-by(df$quality,df$airport,shapiro.test))
round(result$Cork$p.value,4)
round(result$Shanon$p.value,4)

