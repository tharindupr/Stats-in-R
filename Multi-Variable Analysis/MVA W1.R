library(readxl)
ANOVA <- read_excel("C:/Stats in R/Workshop 5/W5 Datasets V2.xlsx", 
                    sheet = "ANOVA")
View(ANOVA)

#First Explore the data------------------------------------->
#Step 01: Check Properties
is.factor(ANOVA$Group) #Group is factor and has 5 subcategories
ANOVA$Group <- factor(ANOVA$Group,levels=c("Placebo","Drug A","Drug B","Drug C","Drug D"))
is.numeric(ANOVA$Score)

#Step 02: numerical descriptive statistics
library(dplyr)
(Stats<-ANOVA %>% group_by(Group) %>% summarise("Sample size"= n(), Mean = mean(Score),
                                                "Standard deviation"=sd(Score),
                                                "Median"=median(Score),
                                                "1st quartile"=quantile(Score,0.25),
                                                "3rd quartile"=quantile(Score,0.75),
                                                Min=min(Score),
                                                Max=max(Score)))
t(Stats)

#Step 03: graphical descriptive statistics
library(ggplot2) # 5 groups in total # we check data is correct or not
(plot1<-ggplot(ANOVA,aes(x=ANOVA$Group,y=ANOVA$Score))+stat_boxplot(geom="errorbar")+geom_boxplot()+
    coord_cartesian(ylim = c(0,25))+labs(x="Group", y="Score")+scale_y_continuous(breaks = seq(0,25,5))+
    theme(text = element_text(size=15)))
#take b, c and d u r happy
#p and A are somehow same
#a is different from b,c and d  (post hoc test)
#p is different from b, c and d  (post hoc test)
#if u have large sample size then a small difference is statistically significant among groups


#SECOND Check that the assumptions are not violated----------------------->
#Step 01: tests of normality
library(psych)
(Norm<-ANOVA %>% group_by(Group) %>% summarise("Sample size"=n(), Mean=mean(Score),
                                               Median =median(Score), Skewness=skew(Score),
                                               "Normally distributed"= ifelse(
                                                 shapiro.test(Score)$p.value>0.05,"Yes", "No")))

t(Norm)
#anova is robust test, it can certain amount of skewness in data

############--------------------------------------##############################
#Step 02: test of homogenity of variances# looking at the range # we have 2 groups distance
#from min to max distance are the same, those are equally spread # variance is the same

# Outliers in group 4 # variance is not equal# Reject Null hypothesis p < 0.05

library(car)
leveneTest(ANOVA$Score ~ ANOVA$Group, center=mean)
# null hypothesis: H0=variance are equal: (REJECT THE CLAIM P<=0.05)
#must contain the condition of equality
# alternative : HA= variance are not equal (Fail  to reject null hypothesis p>0.05)
#-------------------------------------------------#######################################

#Third apply the appropriate test
# Option 01: ANOVA if conditions are not violated
result <- aov(Score ~ Group, data = ANOVA)
summary(result)
#-------------------------------------------------------------################################
# Option 02: If variance are not assumed
library(car)
#When we do ONE-WAY ANOVA
# H0: nO Difference between the means : p<= level of significance then reject H0
# H1: A difference exist between at least 2 # if u reject the H0 then it doesnot mean all the groups are different
#we reject the claim that no difference exist betwen the overall means, statistically insignificant results
#If p ??? ??, then reject H0 - i.e., reject the claim that no di???erence exists between the overall means;
#If p > ??, then fail to reject H0 - i.e., fail to reject the claim that no di???erence exists between the overall means.

oneway.test(ANOVA$Score ~ ANOVA$Group, var.equal = F)
#Option 03: Kruskal Wallis if the condition of normality is violated
kruskal.test(ANOVA$Score ~ ANOVA$Group)



# ANOVA: TELLS US basic comparison of variances, sq of SD is variance
#ANOVA stand for (analysis of variance), you compare the variances
#Two ways to compare the variances:
#First way : if u hav 2 meaurements and u want to check whether they are same , U subtract them, if they r same
#answer should be 0
#Second way: you divide then, if u get 1, they are same
#it look variance between measurements
# 87.88 is ur variance (mean sq) , we have 5 means, find variance and get 87.88 var,
# VAR: FRACTION(sum of sq)how ur data deviates from middle/ degree of freedom (no of mearurement less 1)
# 1ST Line of aov() till mean sq:::RELATES THE VAR BETWEEN GROUPS, WE HAVE 5 GROUPS THATS WHY WE HAVE DF = 4
# var= 351.5/4 = 87.88
# now look variance within groups, In placebo DF /9 (WITH IN EACH GROUP)ADD THEM

#COMPARE VAR BETWEEN GROUPS against variance within the groups****************
#and it gives different answer
#anova does division divide (87.88/9.67= F-value) if F close to 1  it means no difference
# away from 1 , more significant your diffrence will be, it heavily depends on your sample size
# our Fvalue is 9.085  than it means our difference is significant





# FOURTH Main effects plot (plot of the means)----------------------------->
#how we figure out where the difference is post-hoc test tell us this
#so we have to go for visual stuff
#anova means we are comparing means, so we go for plot means
#ploting a mean =,- + error, margin of error give me confidence interval # main effects plot
#margin of error= arrowbarrows, sd, Standard error etc, we use ggline(function)

library(ggpubr)
(mp<- ggline(ANOVA, y= "Score", x= "Group", add= c("mean_ci"), size = 1)+theme_gray())
(plot2<-mp+ theme(text=element_text(size = 15))+
    labs(y="95% CI of Score", x = "Group")+
    coord_cartesian(ylim=c(6,18))+scale_y_continuous(breaks = seq(6,18,2)))
# mean_ci : ci means confidence interval
#mean_iqr: not normally distribute d = interquartile range
#arrowbarrows show CI on y axis

library(gridExtra) # open graph in another window# we merge boxplot and arrowbarrow graph together
windows(16,10)
grid.arrange(plot1,plot2,ncol=2) # put plot1 and plot 2 in 2 columns, i can put them in row


# Fifth Posthoc analysis----------------------------------------------->
#H0: No difference exists between individual means
#H1: A difference exists between the individual means
pairwise.t.test(ANOVA$Score, ANOVA$Group, p.adj= "holm") # WE HAVE 5 GROUP, I GET 10 P VALUES # diff is sig
pairwise.t.test(ANOVA$Score,ANOVA$Group, p.adj="bonferroni") # srict, diff is insignificant (p value is greater)


TukeyHSD(result) #Tukey... result is the name of the aov output # meanA-PLACEBO


# YOU MIGHT ONLY WANT TO TEST AGAINST A CONTROL i.e. Dunnetts test is required
library(DescTools)
DunnettTest(ANOVA$Score ~ ANOVA$Group, control="Placebo")

#Sixth effect size of overall ANOVA Result # how good the diff is
#Strength of result
library(DescTools)
EtaSq(result)
