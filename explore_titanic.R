---
##  title: "Exploring Titanic dataset"
##  author: "Ashutosh Singh"
##  date: "17 December 2015"

---

setwd("C:/Users/my lapy/Desktop/titanic")
train = read.csv("train.csv")
str(train)
## Find the % of survival in each catagory using table
table(train$Survived)
342/(549+342)
## overall survival chance 38% approx
table(train$Survived,train$Pclass)
## Survival chance of 1st class passengers considerably higher than avg.
136/(80+136)
## For 2nd class passengers still higher than average
87/(87+97)
## For 3rd class passengers lower than average
119/(119+372)
## Also the difference between 1st and 2nd class 0.33 and 2nd and 3rd is 0.48
## also 0.61 very high
(0.6296-0.4728)/0.4728
(0.2423-0.4728)/0.4728
(0.6296-0.2423)/0.6296


## Now look at the effect of gender and class
table(train$Survived,train$Sex)
## chances of survival of male passengers
109/(109+468)
## chances of survival of female passengers
233/(81+233)


## Now look at survival dependency on age

survived = subset(train,train$Survived==1)
notsurvived = subset(train,train$Survived==0)
mean(notsurvived$Age,na.rm = TRUE)
mean(notsurvived$Age,na.rm = TRUE)
sd(survived$Age,na.rm = TRUE)
sd(notsurvived$Age,na.rm = TRUE)

## As the deviation within the group is much larger than across the group hence we discard the effect of age on chances of survival

## Chance of survival according to number of siblings

table(train$SibSp,train$Survived)
210/(398+210)
112/(97+112)
13/(13+15)
4/(4+12)
3/(3+15)
0
0

## Here we see the chances of survival decrease with increase in number of siblings  because they may less likely to leave each other

table(train$Parch,train$Survived)
233/(233+445)
## With 0 parents/children it is very close to overall average
(65+40+3+0+1+0)/(53+40+2+4+4+1+65+40+3+0+10+0)
## this is higher than the average, thus travelling with a parent children improvess the chances of survival


##  Survival chance with varying fare
mean(survived$Fare)
mean(notsurvived$Fare)

## The difference is very significant however due to correlation between fare and passenger class must be tackeld carefully however the dependence of fare on Pclass 

## Don't read complete just notice the trend. or look at the plot to understand

plot(train$Fare,train$Pclass)

##  Understanding relationship between survival and place of boarding
table(train$Survived,train$Embarked)
93/(93+75)
30/(47+30)
217/(217+427)

## This is interesting as Cherbourg has a very high survival rate compared to others (c>q>s)
## Let's investigate 

table(train$Pclass,train$Embarked)
85/(17+66+85)
0.197205

## Ok! So, C has a very high number of passengers travelling in 1st class.Maybe its a rich place
## However  Q and S looks very fishy! not much difference in Q and S and infact most of people travelling in Q are in 3rd class and still it has higher survival chance per passenger ! Let's look deeper...

table(train$Sex,train$Embarked)
36/(36+41)
203/(203+441)

## so, Q has a higher female percentage of population boarding compared to S but still look for something more

table(train$SibSp,train$Embarked)
## Looking at the % of people travelling alone
109/(109+53+6)
59/(59+11+3+4)
438/(438+145+19+16+14+5+7)

## So here we are. May be people who boarded at S are working class people,mostly male and travelling alone.
##  Let's confirm by looking at the number of parents or children travelling with them.

table(train$Parch,train$Embarked)
123/(123+30+14+1)
69/(69+6+1+1)
484/(82+484+65+4+4+4+1)

##  Not many conclusions here, except that Q which has a higher percentage of females also has a better parent/children relationship as females are less likely to leave their children alone 

new_train = subset(train,select = c(PassengerId,Survived,Pclass,Sex,SibSp,Parch,Fare,Embarked))
new_test = subset(test,select = c(PassengerId,Pclass,Sex,SibSp,Parch,Fare,Embarked))

write.csv(new_train,file="new_train.csv")
write.csv(new_test,file="new_test.csv")