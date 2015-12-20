---
  title: "Descriptive plots Titanic"
author: "Ashutosh Singh"
date: "17-18 December 2015"
output: html_document
---

setwd("C:/Users/my lapy/Desktop/titanic")
library(ggplot2)
new_train = read.csv("new_train.csv")
## overall chances of survival

qplot(Survived,data=new_train,geom="bar")

## Survival chance of male vs female
qplot(Survived,data=new_train,geom="bar",fill=Sex,position="fill")
## To check if conclusion biased
table(new_train$Survived,new_train$Sex)
(81+233)/(468+109)
## Hence likely better chances of survival for female passengers as number of female passengers almost half of male passengers still higher count in survival list


## Survival chance of different lodging places
qplot(Survived,data=new_train,geom="bar",fill=Embarked,position="fill")
## To check if description biased
table(new_train$Survived,new_train$Embarked)
217/(217+421)
30/(30+47)
93/(75+93)
## Hence, S has lowest chance of survival still looks biggest as it has strength of numbers
## Caution : If in any model factors affecting different for different places, s will heavily dominate. So, suggested that cluster according to requirement

## Effect of class on survival rate

new_train$Pclass=as.factor(new_train$Pclass)
qplot(Survived,data=new_train,geom="bar",fill=Pclass,position="fill")

## To check biasdness
table(new_train$Survived,new_train$Pclass)
(80+136)/(80+136+97+87+372+119)
(97+87)/(80+136+97+87+372+119)
(80+136)/(80+136+97+87+372+119)

## Thus 3rd class suffered the most in the tragedy.
new_train$SibSp = as.factor(new_train$SibSp)
qplot(Survived,data=new_train,geom="bar",fill=SibSp,position="fill")
## It appears that having one sibling improves your chances of survival; whereas you are better off alone than having more than one sibling

## let us verify our theory
table(new_train$Survived,new_train$SibSp)
210/398
112/97
(13+4+3+0)/(15+12+15+5+7)
## Thus suggested that creation of a new factor variable with 3 values : no siblings, one sibling and more than one sibling

## Effect of parent/children

new_train$Parch = as.factor(new_train$Parch)
qplot(Survived,data=new_train,geom="bar",fill=Parch,position="fill")
## So, we see having one or two parents or children improves the chances of survival. Or maybe because women are most likely to be with children.

table(new_train$Parch,new_train$Sex)

## Thus we see that the Parch is not really an independent variable, the chances that if the Parch is one or more than one  , then it is very likely that she is female passenger

## Now let us look at two variables at once to see how survival rates vary within these subgroups
## For example look at the Female and Male survival rates in different passenger classes to see if they were treated differentially

train = read.csv("new_train.csv")
## First let us created two subgroups called survived and not survived to be able to variables required for creating heatmaps 

## So have a look at the relationship first

survived=subset(train,train$Survived==1)
notsurvived=subset(train,train$Survived==0)
library(ggplot2)
x=table(survived$Pclass,survived$Sex)
y=table(notsurvived$Pclass,notsurvived$Sex)
z=x/(x+y)
z
## Surprising !!! Female passengers of 1st and 2nd class have the most chances of survival,whereas of 3rd class has 50% which is still higher than male survival rate in 1st class
## maybe the trend or preference was :  gender>>passengerclass

sex=c("male","male","male","female","female","female")
class = c(1,2,3,1,2,3)
Survival_Chance = c(0.3688525,0.1574074,0.1354467,0.9680851,0.9210526,0.5000000)
vis = data.frame(sex,class,Survival_Chance)
ggplot(data=vis,aes(x=sex,y=class))+geom_tile(aes(fill=Survival_Chance))+scale_fill_gradient(name="Survival chance of genders across Passenger class",low = "red",high="white")

## The red shows the danger zone and white means high survival rate

## Now, letus prove our point that the parch variable is not really an independent variable. For this purpose we will look the survival chances across genders when Parch variable is >=

qplot(Parch,data=train,geom="bar",fill=Sex,position="dodge")

## Thus we see that the likelyhood of a paasenger been female increases highly with increasing Parch variable

## Now check the variation of survival rate,Pclass and fare to see if they are independent or correlated

ggplot(train,aes(x=Pclass,y=Survived))+geom_tile(aes(fill=Fare))+scale_fill_gradient(name="Variation of fare,classes and survival",low="white",high="black")

## Intresting ! Thus we see that the people who survived had generally paid higher fares than who didnot survived. We donot know the reason of this but it must be something to do with privilages. The 3rd class is indifferent to this feature

## Now see the variation in places embarked

a=table(survived$Embarked,survived$Sex)
a=a[2:4,1:2]
b=table(notsurvived$Embarked,notsurvived$Sex)
b=b[2:4,1:2]
c=a/(a+b)
c
sex=c("male","male","male","female","female","female")
Embarked=c("C","Q","S","C","Q","S")
Survival_Chance=c(0.30526316,0.07317073,0.17460317,0.8767123,0.7500000,0.6896552)
vis4=data.frame(sex,Embarked,Survival_Chance)
ggplot(vis4,aes(x=Embarked,y=sex))+geom_tile(aes(fill=Survival_Chance))+scale_fill_gradient(name="Variation of fare,classes and survival",low="white",high="green")

## Thus we see that the general trend of survuval is C>Q>S and F>M...however male at Q looks like an exception. We may like to know more about it

## First have a quick look at the number of male and female passengers at each place

qplot(Embarked,data=train,geom="bar",fill=Sex,position="dodge")

## Now at the passenger class with places embarked

qplot(Pclass,data=train,geom="bar",fill=Embarked,position="dodge")

## Thus we see that most of people embarked at C are 1st class(best survival chances), at S are 3rd class(least survival chance). However the differentiating factor about Q is that though at places C and S both 1st and 3rd class are present, Q is overwhemingly 3rd class. Thus the difference

## Suggested that Embarked is also a combination of factors and not really an independent variable

