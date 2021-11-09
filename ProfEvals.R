---
  title: "Professor Evaluations"
author: "William Duquette"
date: "Due on Wednesday, April 22 at 11:59PM"
output:
  html_document:
  df_print: paged
pdf_document: default
word_document: default
---
require(car)
require(carData)
require(ggplot2)
require(tidyverse)
require(EnvStats)
require(MASS)
library(MuMIn)
require(gridExtra)
require(Hmisc)
library(expss)

download.file("http://www.openintro.org/stat/data/evals.RData", destfile = "evals.RData")
load("evals.RData")

drops = c("bty_f1lower", "bty_f1upper", "bty_f2upper", "bty_m1upper", "bty_m2upper", "bty_m1lower")
evals = evals[, !(names(evals) %in% drops)]

evals2=evals
evals2$cls_did_eval=log(evals$cls_did_eval)
evals2$cls_students=log(evals$cls_students)

evals=apply_labels(evals, cls_did_eval="Number of Students who Completed Evaluations", cls_students= "Total Number of Students in Class")
evals2=apply_labels(evals2, cls_did_eval="Number of Students who Completed Evaluations", cls_students= "Total Number of Students in Class")

ggplot(evals, aes(x=rank))+geom_bar()+labs(title = "Distribution of Rank")
summary(evals$rank)

ggplot(evals, aes(x=ethnicity))+geom_bar()+labs(title="Distribution of Ethnicity")
summary(evals$ethnicity)

ggplot(evals, aes(x=gender))+geom_bar()+labs(title="Distribution of Gender")
summary(evals$gender)

ggplot(evals, aes(x=language))+geom_bar()+labs(title="Distribution of Language")
summary(evals$language)

ggplot(evals2, aes(x=age))+geom_histogram(bins=10)+labs(title="Distribtuion of Age")

ggplot(evals, aes(x=cls_perc_eval))+geom_histogram(bins=10)+labs(title="Distribution of cls_perc_eval", x="Percent of Students that Completed an Evaluation")

ggplot(evals, aes(x=cls_did_eval))+geom_histogram(bins=10)+labs(title="Distribution of cls_did_eval", x="Amount of Students that Completed an Evaluation")

ggplot(evals, aes(x=cls_students))+geom_histogram(bins=10)+labs(title="Distribution of cls_students",x="Amount of Students in a Class")

ggplot(evals, aes(x=cls_level))+geom_bar()+labs(title="Distribution of cls_level",x="Class Level")
summary(evals$cls_level)

ggplot(evals, aes(x=cls_profs))+geom_bar()+labs(title="Distribution of cls_profs",x="Multiple or Single")
summary(evals$cls_profs)

ggplot(evals, aes(x=cls_credits))+geom_bar()+labs(title="Distribution of cls_credits",x="Whether the Course is One Credit or Multi-Credit")
summary(evals$cls_credits)

ggplot(evals2, aes(x=bty_avg))+geom_histogram(bins=10)+labs(title="Distribution of bty_avg",x="Professors Average Beauty Average")

ggplot(evals, aes(x=pic_outfit))+geom_bar()+labs(title="Distribution of pic_outfit", x="Whether the Professor was Dressed Formally or Informally")
summary(evals$pic_outfit)

ggplot(evals, aes(x=pic_color))+geom_bar()+labs(title="Distribution of pic_color", x="Whether the Professor's Pictures was in Color or Black & White")
summary(evals$pic_color)

# Analyzing all of variables to check if they need to be transformed
par(mfrow=c(2,2))
ggplot(evals2, aes(x=score))+geom_histogram(bins=15)+labs(title = "Distribution of response variable: score")
plot(score~.,data=evals,  main="Score vs. Explanatory Variable")

# Chekcing cls_did_eval
before1<-ggplot(evals, aes(x= cls_did_eval, y= score))+geom_point()+labs(title= "Before", x= "Number of Students who Completed Evaluations")
after1<-ggplot(evals2, aes(x=cls_did_eval, y= score))+geom_point()+labs(title="After", x="Number of Students who Completed Evaluations")
grid.arrange(before1,after1,ncol=2)

# Checking cls_students
before2<-ggplot(evals, aes(x=cls_students, y=score)) + geom_point()+labs(title= "Before Transformation for cls_students", x= "Total Number of Students in Class")
after2<-ggplot(evals2, aes(x=cls_students, y=score))+geom_point()+labs(title="After Transformation for cls_students", x="Total Number of Students in Class")
grid.arrange(before2,after2)

# Forming my first interaction term
ggplot(evals2, aes(x=bty_avg, y=score, color=age))+geom_point()
mod.int.1<-lm(score~age*bty_avg, data=evals2)
summary(mod.int.1)

newdata= data.frame(bty_avg=c(4,4,4,4,6,6,6,6,8,8,8,8,10,10,10,10), age=c(40,50,60,70,40,50,60,70,40,50,60,70,40,50,60,70))
predict(mod.int.1,newdata)

#Forming my second interaction term
ggplot(evals2, aes(x=rank, y= score, fill=ethnicity))+geom_boxplot()
mod.int.2<- lm(score~rank*ethnicity, data= evals2)
summary(mod.int.2)
table(evals2$ethnicity, evals2$rank)

#backwards stepwise model selection
mod.full<-lm(score~.+age*bty_avg+rank*ethnicity, data=evals2)
mod.back<-step(mod.full,direction="backward", trace=0)
summary(mod.back)

plot(mod.back)