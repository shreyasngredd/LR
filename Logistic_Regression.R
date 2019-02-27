#####LOGISTIC REGRESSION#####

#Suppose we are interested in the factors that influence whether a political candidate wins an election. 
#The outcome (response) variable is binary (0/1); win or lose. 
#The predictor variables of interest are the amount of money spent on the campaign, 
#the amount of time spent campaigning negatively and whether or not the candidate is an incumbent.

#Load and Attach the data
elec<- read.csv(file.choose())
View(elec)
attach(elec)

#Summarize the data
summary(elec)

#Election.id: Mean=451.6, Median=362.5; As Mean>Median,it is skewed to the left

#Result: Mean=0.6, Median=1.0; As Mean>Median,it is skewed to the left

#Year: Mean=43.30; Median=43.00; As Mean>Median,it is skewed to the left

#Amount.Spent: Mean= 4.229, Median= 4.005; As Mean>Median,it is skewed to the left

#Popularity.Rank: Mean= 3.00, Median= 3.00; As Mean=Median, skewness is absent 
#& the distribution is even.
library(DataExplorer)
plot_str(elec)

plot_missing(elec)

plot_histogram(Election.id)
plot_histogram(Year)
plot_histogram(Amount.Spent)
plot_histogram(Popularity.Rank)

plot_density(Result)
plot_density(Amount.Spent)
plot_density(Popularity.Rank)
plot_density(Year)
plot_density(Election.id)

str(elec)

elec$Result<- factor(elec$Result)
str(elec)

#Correlation scatterplot
pairs(elec)

#Correlation Coefficient matrix
cor(elec)

#Linear Regression
linr<- lm(Result~ Amount.Spent+Year+Popularity.Rank)
summary(linr)
#Linear regression technique can not be employed as the output is continous

#Logistic Regression
logr<- glm(Result~Year+Amount.Spent+Popularity.Rank, family = binomial)
summary(logr)

# Checking for best fit model
library(MASS)
stepAIC(logr)

# Odds Ratio
exp(coef(logr))

# Confusion matrix table 
prob <- predict(logr,type=c("response"))
prob
conf<-table(prob>0.5,Result)
conf

# Model Accuracy 
Accu<-sum(diag(conf)/sum(conf))
Accu
## The model accuracy is 100%, so this model can be considered.

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,Result)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

###################

#Output variable -> y
#y -> Whether the client has subscribed a term deposit or not 
#Binomial ("yes" or "no")


#Load and Attach the data
bank<- read.csv(file.choose())
View(bank)
attach(bank)

#Summarize the data
summary(bank)
#Age: Mean= 40.94, Median= 39.00; As Mean>Median,it is skewed to the left
#default: Mean= 0.01803, Median= 0.00; As Mean>Median,it is skewed to the left
#balance: Mean= 1362, Median= 448; As Mean>Median,it is skewed to the left
#housing: Mean= 0.5558, Median= 1.000; As Mean<Median,it is skewed to the right
#loan: Mean= 0.1602, Median= 0.000; As Mean>Median,it is skewed to the left
#duration: Mean= 258.2, Median= 180.00; As Mean>Median,it is skewed to the left
#campaign: Mean= 2.764, Median= 2.000; As Mean>Median,it is skewed to the left
#pdays: Mean=40.2, Median=-1.0;As Mean>Median,it is skewed to the left
#previous: Mean=0.5803, Median= 0.000; As Mean>Median, it is skewed to the left
#poutfailure: Mean=0.1084, Median=0.000; As Mean>Median, it is skewed to the left
#poutother: Mean=0.407, Median=0.000; As Mean>Median, it is skewed to the left
#poutsuccess: Mean=0.03342, Median=0.000; As Mean>Median, it is skewed to the left
#poutunknown: Mean=0.8175, Median=1.000; As Mean>Median, it is skewed to the left
#con_cellular: Mean=0.6477, Median=1.000; As Mean>Median, it is skewed to the left
#con_telephone: Mean=0.06428, Median=0.000; As Mean>Median, it is skewed to the left
#con_unknown: Mean=0.288, Median=0.000; As Mean>Median, it is skewed to the left
#divorced: Mean=0.1152, Median=0.000; As Mean>Median, it is skewed to the left
#married: Mean=0.6019, Median=0.000; As Mean>Median, it is skewed to the left
#single: Mean=0.2829, Median=0.000; As Mean>Median, it is skewed to the left
#joadmin: Mean=0.1144, Median=0.000; As Mean>Median, it is skewed to the left
#joblue.collar: Mean=0.2153, Median=0.000; As Mean>Median, it is skewed to the left
#joentrepreneur: Mean=0.03289, Median=0.000; As Mean>Median, it is skewed to the left
#jomanagement: Mean=0.2092, Median=0.000; As Mean>Median, it is skewed to the left
#joretried: Mean=0.05008, Median=0.000; As Mean>Median, it is skewed to the left
#joself.employed: Mean=0.03493, Median=0.000; As Mean>Median, it is skewed to the left
#joservices: Mean=0.09188, Median=0.000; As Mean>Median, it is skewed to the left
#jostudent: Mean=0.02075, Median=0.000; As Mean>Median, it is skewed to the left
#jotechnician: Mean:0.168, Median=0.000; As Mean>Median, it is skewed to the left
#jounemployed: Mean=0.02882, Median=0.000; As Mean>Median, it is skewed to the left
#y: Mean=0.117, Median=0.000; As Mean>Median, it is skewed to the left

library(DataExplorer)
plot_str(bank)
str(bank)

plot_missing(bank)

plot_histogram(age)
plot_histogram(balance)
plot_histogram(duration)
plot_histogram(bank)

plot_density(age)
plot_density(balance)
plot_density(duration)

str(bank)

d<- factor(default)
d

h<- factor(housing)
h

l<- factor(loan)
l

p<- factor(poutfailure)
p 

ps<- factor(poutother)
ps

pts<- factor(poutsuccess)
pts

pu<- factor(poutunknown)
pu

s<- factor(single)
s

c<- factor(con_cellular)
c

ct<- factor(con_telephone)
ct

cu<- factor(con_unknown)
cu

di<- factor(divorced)
di

mr<- factor(married)
mr

si<- factor(single)
si

ja<- factor(joadmin.)
ja

jbc<- factor(joblue.collar)
jbc

joe<- factor(joentrepreneur)
joe

jhm<- factor(johousemaid)
jhm

jr<- factor(joretired)
jr

jse<- factor(joself.employed)
jse

js<- factor(joservices)
js

jos<- factor(jostudent)
jos

jot<- factor(jotechnician)
jot

jou<- factor(jounemployed)
jou

joun<- factor(jounknown)
joun

wy<- factor(y)
wy

#Logistic Regression1
logr2<-glm(y~age+balance+duration+pdays+campaign+factor(default)
           +factor(housing)+factor(loan)+factor(poutfailure)+
             factor(poutother)+factor(poutsuccess)+factor(poutunknown)+
             factor(single)+factor(con_cellular)+factor(con_telephone)+
             factor(con_unknown)+factor(divorced)+factor(married)+factor(single)+
             factor(joadmin.)+factor(joblue.collar)+factor(joentrepreneur)+
             factor(johousemaid)+factor(joretired)+factor(joself.employed)+
             factor(joservices)+factor(jostudent)+factor(jotechnician)+
             factor(jounemployed)+factor(jounknown),family=binomial)
summary(logr2)

# Checking for best fit model
library(MASS)
library(car)
stepAIC(logr2)

# Odds Ratio
exp(coef(logr2))

# Confusion matrix table 
prob2 <- predict(logr2,type=c("response"))
prob2
conf2<-table(prob2>0.5,y)
conf2

# Model Accuracy 
Accu2<-sum(diag(conf2)/sum(conf2))
Accu2

##The Accuracy of the model is 90%, so, this model can be considered. 

# ROC Curve 
library(ROCR)
rocrpred2<-prediction(prob2,y)
rocrperf2<-performance(rocrpred2,'tpr','fpr')
plot(rocrperf2,colorize=T,text.adj=c(-0.2,1.7))

#############

#I have a dataset containing family information of married couples, which have around 10 variables & 600+ observations. 
#Independent variables are ~ gender, age, years married, children, religion etc.
#I have one response variable which is number of extra marital affairs.
#Now, I want to know what all factor influence the chances of extra marital affair.
#Since extra marital affair is a binary variable (either a person will have or not), 
#so we can fit logistic regression model here to predict the probability of extra marital affair.

#Load and Attach the data
affairs<- read.csv(file.choose())
View(affairs)
attach(affairs)

#Summarize the data
summary(affairs)
#X: Mean= 301, Median= 301; As Mean=Median,skewness is absent & the 
#distribution is even.
#naffairs: Mean=1.456, Median=0.000; As Mean>Median,it is skewed to the left
#kids: Mean= 0.7155, Median= 1.000; As Mean<Median,it is skewed to the right
#vryunhap: Mean= 0.02662, Median= 0.000; As Mean>Median,it is skewed to the left
#unhap: Mean= 0.1098, Median= 0.000; As Mean>Median,it is skewed to the left
#avgmarr: Mean= 0.1547, Median= 0.000; As Mean>Median,it is skewed to the left
#hapavg: Mean= 0.3228, Median= 0.000; As Mean>Median,it is skewed to the left
#vryhap: Mean= 0.386, Median= 0.000; As Mean>Median,it is skewed to the left
#antirel: Mean= 0.07987, Median= 0.000; As Mean>Median,it is skewed to the left
#notrel: Mean= 0.2729, Median= 0.000; As Mean>Median,it is skewed to the left
#slghtrel: Mean= 0.2146, Median= 0.000; As Mean>Median,it is skewed to the left
#smerel: Mean= 0.3161, Median= 0.000; As Mean>Median,it is skewed to the left
#vryrel: Mean= 0.1165, Median= 0.000; As Mean>Median,it is skewed to the left
#yrsmarr1: Mean= 0.08652, Median= 0.000; As Mean>Median,it is skewed to the left
#yrsmarr3: Mean= 0.1747, Median= 0.000; As Mean>Median,it is skewed to the left
#yrsmarr4: Mean= 0.1364, Median= 0.000; As Mean>Median,it is skewed to the left
#yrsmarr5: Mean= 0.1165, Median= 0.000; As Mean>Median,it is skewed to the left
#yrsmarr6: Mean= 0.3394, Median= 0.000; As Mean>Median,it is skewed to the left

library(DataExplorer)
plot_str(affairs)
str(affairs)

plot_missing(affairs)

plot_histogram(naffairs)
plot_histogram(kids)
plot_histogram(affairs)

plot_density(naffairs)
plot_density(kids)
plot_density(affairs)


k<- factor(kids)
k
vu<- factor(vryunhap)
vu
uh<- factor(unhap)
uh
am<- factor(avgmarr)
am
ha<- factor(hapavg)
ha
vh<- factor(vryhap)
vh
ar<- factor(antirel)
ar
nr<- factor(notrel)
nr
sl<- factor(slghtrel)
sl
vl<- factor(vryrel)
vl
ys1<- factor(yrsmarr1)
ys1
ys2<- factor(yrsmarr2)
ys2
ys3<- factor(yrsmarr3)
ys3
ys4<- factor(yrsmarr4)
ys4
ys5<- factor(yrsmarr5)
ys5
ys6<- factor(yrsmarr6)
ys6

#Logistic Regression1
logreg3<- glm(factor(kids)~ naffairs+factor(vryunhap)+factor(unhap)+factor(avgmarr)+
                factor(hapavg)+factor(vryhap)+factor(antirel)+factor(notrel)+
                factor(slghtrel)+factor(vryrel)+factor(yrsmarr1)+factor(yrsmarr2)+
                factor(yrsmarr3)+factor(yrsmarr4)+factor(yrsmarr5)+factor(yrsmarr6)+X, family=binomial)
summary(logreg3)

# Checking for best fit model
library(MASS)
library(car)
stepAIC(logreg3)

#Odds Ratio
exp(coef(logreg3))

# Confusion matrix table 
prob3 <- predict(logreg3,type=c("response"))
prob3
conf3<-table(prob3>0.5,kids)
conf3

# Model Accuracy 
Accu<-sum(diag(conf3)/sum(conf3))
Accu

##The Accuracy of the model is 84.6%, so, this model can be considered.

# ROC Curve 
library(ROCR)
rocrpred3<-prediction(prob3,kids)
rocrperf3<-performance(rocrpred3,'tpr','fpr')
plot(rocrperf3,colorize=T,text.adj=c(-0.2,1.7))
