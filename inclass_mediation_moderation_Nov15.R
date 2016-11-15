####INTERACT/MEDIATION HANDOUT 
library(tidyverse)

##load data
my_data <- read_csv("LectureData6060.csv")

glimpse(my_data)

analytic.data <- my_data

analytic.data <- analytic.data %>% mutate(x.centered=as.numeric(scale(Anxiety,center=T,scale=F)) )
analytic.data <- analytic.data %>% mutate(z.centered=as.numeric( scale(Preparation,center=T,scale=F)) )
##do anxiety and preparation interact to predict exam scores 

##regression
interaction.regression <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)
#using exam as criteria, x centered, y centered
#my data is the data i created, if any row has a missing value, drop the row: na.action=na.exclude
#scale means do i change the SD to 1 - False is no, True is yes

library(apaTables)
apa.reg.table(interaction.regression)

##using blocks 
#another approach
block1 <- lm(Exam ~ x.centered + z.centered,data=analytic.data, na.action=na.exclude)
block2 <- lm(Exam ~ x.centered + z.centered + I(x.centered*z.centered), data=analytic.data, na.action=na.exclude)
apa.reg.table(block1, block2)

#delta r squared IS significant, but ...when you do the CIs you see that CI for the sr2 overlaps with zero [-.02, .09]
#signifiance is based on change in R

#we like R2 because you can turn it into percentages 
#3% of the 34% is due to interaction (.03)

##Regression results using Exam as the criterion
###Predictor       b       b_95%_CI  beta    beta_95%_CI sr2  sr2_95%_CI     r
#(Intercept) 55.58** [52.39, 58.76]                                           
#x.centered -2.33** [-3.93, -0.73] -0.24 [-0.41, -0.08] .06 [-.02, .14] -.24*
#z.centered  4.74**   [3.14, 6.34]  0.50   [0.33, 0.67] .25  [.10, .39] .50**
  
  
#(Intercept) 55.58** [52.46, 58.70]                                           
#x.centered -2.15** [-3.73, -0.57] -0.23 [-0.39, -0.06] .05 [-.02, .12] -.24*
#z.centered  4.56**   [2.99, 6.14]  0.48   [0.31, 0.64] .23  [.09, .36] .50**
#I(x.centered * z.centered)   0.91*   [0.08, 1.73]  0.18   [0.02, 0.35] .03 [-.02, .09]      

#Fit        Difference
#R2 = .307**                  
#95% CI[.15,.43]                  

#R2 = .340**   Delta R2 = .03*
  #95% CI[.18,.45] 95% CI[-.02, .09]

###making a graph
##getting the line on the surface at +1 standard deviation 
sd.z <- sd(analytic.data$z.centered, na.rm=TRUE)
analytic.data <- analytic.data %>% mutate(z.centered.at.plus.1SD = z.centered - sd.z)
simple.slope.plus.1SD <- lm(Exam ~ x.centered + z.centered.at.plus.1SD + I(x.centered*z.centered.at.plus.1SD),
                            data=analytic.data, na.action=na.exclude) 
                            summary(simple.slope.plus.1SD)
                            
apa.reg.table(simple.slope.plus.1SD)
#can ignore all the z centered lines in the table

##getting the line on the surface at -1 standard deviation 
analytic.data <- analytic.data %>% mutate(z.centered.at.minus.1SD=z.centered + sd.z)
simple.slope.minus.1SD <- lm(Exam ~ x.centered + z.centered.at.minus.1SD
                             + I(x.centered*z.centered.at.minus.1SD), data=analytic.data,na.action=na.exclude)

apa.reg.table(simple.slope.minus.1SD)
#can ignore all the z centered lines in the table

##3D graph take above lines from the apa table and plus into one line of 3D graph equation 

####MEDIATION

mediation.data <- analytic.data %>% select(Exam, Preparation, Anxiety)
head(mediation.data)
#thinkig now this is more of a mediator situation

#load psych package
#psych::mediate()
#insert column numbers (not the variable names)

#Exam Preparation Anxiety
#<dbl>       <dbl>   <dbl>
#1 43.958        7.94   22.29
#2 60.813        8.15   21.14
#3 37.874        8.17   19.93
#4 56.054        8.21   17.23
#5 29.500        8.43   22.12
#6 15.549        8.52   21.51
                            
psych::mediate(y=1, x=2, m=3, data=mediation.data)


