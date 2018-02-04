#ANOVA-HW#1 - Andrew Wong

#Assignment: Use the data in arsenic.csv on ecourseware. It's a measure of the arsenic content in the parts 
#per billion for a random sample of wells from North Texas. Use this data to test whether the avg. amount of arsenic
#in the wells in North Texas exceeds U.S., EPA maximum contaminant levels of 10ppb using the methods developed here

#Due 2/5 @ 5:30pm. Upload an R file that runs and the comments that include your conclusion. 

#Import Data
library(readr)
arsenic <- read_csv("C:/Users/Andrew Wong/Downloads/arsenic.csv")
View(arsenic)

#H0: mu = 10 , HA: mu > 10

#EDA
#Check assumptions 1. normal 2. independence
#normal assumption - two diviations: 1. outliers (Use nonpara or robust for outliers) 2. non-normality
#Check for outliers
boxplot(arsenic$C1) #Maybe some outliers on the top

#Check for normality
qqnorm(arsenic$C1) #Definitely doesn't look like a 45 Degree straight line, might be because of several extreme points on the right tail

shapiro.test(arsenic$C1) #P value is below extremely low. Below .001. So not normal

#Check for serial correlation (Check for dependent)
plot(arsenic$C1) #No patterns, but some extreme values on the top

##What time of non-normality (skewness or kurtotis?)
library(e1071)

skewness(arsenic$C1) #3.56 , and our range for skewdness was 0<G1<.7. This is outside that range. So we have skewness
kurtosis(arsenic$C1) #15.04997, and this is outside the range of -.5<G2<4. So kurtosis

#mean and variance
mean(arsenic$C1) #mean = 9.73
var(arsenic$C1)  #Var = 132.89

#Try a transformation to "fix" skewness

y <- log(arsenic$C1) #or sqrt()
qqnorm(y) #Looks lot better
shapiro.test(y) #p value is below .05 so still not normal

skewness(y) #Still outside the range.(-.177) So skewness still a problem
kurtosis(y) #Inside range now (-.318). So kurtosis is okay now

#Try sqrt transformation instead
y <- sqrt(arsenic$C1) #or sqrt()
qqnorm(y) #Looks worse than doing the log transformation
shapiro.test(y) #p value is below .05 so still not normal

skewness(y) #Still outside the range.(1.5) So skewness still a problem
kurtosis(y) #Inside range now (3.95). So kurtosis is okay now

#Since even transformation won't work let's try non-parametric methods
#First try sign test
#H0: median = 10 and HA: median > 10
median(arsenic$C1) #7.1
mean(arsenic$C1) #9.73. Since median and mean are close, the skewness could be because those points to the right are outliers
#Count the number of values greater than 10
x <- sum(arsenic$C1 >10)
n <- length(arsenic$C1)
x #37
n #102

binom.test(x,n, p = .5, alternative = "greater") #p value is above .05 (.99) so don't reject H0

##Another test we could use is a the wilcoxon test
wilcox.test(arsenic$C1, alternative = "greater", mu = 10) # p value is high (.99) so don't reject H0


#Conclusion: The question was to see if the wells in North Texas had arsenic levels higher than 10bbp. 
#The null hypothesis was mean = 10 and the altnernative hypothesis is mean >10. I checked for normality 
#using qqnorma and the shapiro.test and found the data not to be normal (Low p value and weird qqplot).
#I also checked for dependence and didn't find anything there. #Then I checked for what might be casuing the non-normality.
#Both skewness and kurtosis were a problem. (Both values were outside "okay" range)# To "fix" this I tried
#using the log and sqrt transformation. Both helped with kurtosis, but didn't "fix" skewness. So instead
#I tried using non-parametric methods. I used the sign test and got a p value of .99 (H0: median = 10, Ha: median >10),
#which tells me not to reject H0 and that the problem with non-normality was probably due to outliers. To double check
#I used the wilcoxon test too. I also got a p value of .99. This tells me that I should not say that the arsenic 
#levels in the wells in North Texas are above the okay amount of 10bbp, but I should also say that there are several wells
#that have really extreme values. The gov't should look into fixing those wells instead of fixing all the wells in North Texas. 
