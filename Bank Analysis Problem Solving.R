#import bank_additional_full.xlxs
DATA=bank_additional_full
head(bank,3)
View(DATA)
#helps you see attributes 
str(DATA)
options(scipen=999)
#How many na's do we have in y?
#data=na.omit(data)
dim(DATA)
sum(is.na(DATA))
unique(DATA$pdays)
install.packages("sqldf")
library(sqldf)
sqldf("select month,pdays,count(pdays) from DATA group by pdays order by month,pdays ASC ")

#only 3 defaulted
defaulters=subset(DATA,DATA$y=="yes")
nondefaulters=subset(DATA,DATA$y=="no")
dim(defaulters)
dim(nondefaulters)
xtabs(~y,DATA)
xtabs(~y+job,DATA)
4640/41188
######
######
######
######
#######
######
#REGRESSION


#Trying to find reference group for job
unique(DATA$job)
#for full model with unemployed as reference group
DATA$job=ifelse(DATA$job=="unemployed","aaunemployed",DATA$job)
#for full model bluecollar as reference group for jobs
DATA_b=DATA
DATA_b$job=ifelse(DATA$job=="blue-collar","aablue-collar",DATA$job)

#We can also determine percentiles of specific ages:
install.packages("sqldf")
library(sqldf)
sqldf("select count(job) from DATA group by job")
job=sqldf("select distinct job from DATA")



#FullModelunemployed <- glm(y ~ ., data=DATA[traindata,],family=binomial)
#FullModelblue <- glm(y ~ ., data=DATA_b[traindata,],family=binomial)


##Step() function reduces model for us


#Test for multicollinearity
install.packages("corrplot")
library(corrplot)
correlations <- cor(DATA[,c(1,11:14,16:21)])
corrplot(correlations, method="circle")


###LOGISTIC REGRESSION

#The y-variable in logistic needs to be an ordinal variable i.e. a 0/1 variable
DATA$y=ifelse(DATA$y=="yes",1,0)

Modelunemployed <- glm(y ~ ., data=DATA,family=binomial)
#job + default + contact + month + day_of_week + 
  #duration + campaign + pdays + poutcome + emp.var.rate
summary(Modelunemployed)

#PREDICTION IN LOGISTIC
LOG.ODDS <- predict(Modelunemployed) 
ODDS <- exp(LOG.ODDS)

PredictedProbability <- ODDS / (1 + ODDS)
head(PredictedProbability)

#This gives the probability of yes, which is between 0 and 1 , not a "yes" vs. "no" prediction
#To try and create a column of we like purple/green, we have to pick a cutoff probability above
#which we say we pick purple, if pr of purple is below the cutoff, we say we pick Green in model
#We use an ifelse statement on the probability, the output will be stored as a column


#Choosing the probability of "yes" threshhold based on which we should predict a "yes"
#We will need to compare numbers to build the ROC model:
install.packages("pROC")
library(pROC)
MyROC <- roc(DATA$y, PredictedProbability)
plot(MyROC)
#Finding the best cut off value
coords(MyROC, "best",transpose = TRUE)
#4640/41188= around 11%. compares well with coords results of 9%

##Predicting based on that threshhold
LogisticPrediction1 <- ifelse(PredictedProbability > 0.08593378, "yes", "no")
#We can combine that column and the color column to get a table summary of number of predict purple,
#and predict green against actual purple and greens
table(LogisticPrediction1, DATA$y)
#From the table
(30318+477)/41188
AccuracyLogistic=(29849+4303)/(41188)*100
SensitivityLogistic=(4303)/(337+4303)*100
SpecificityLogistic=29849/(29849+6699)*100
Logistic_1_cut_off_0.086=c(AccuracyLogistic,SensitivityLogistic,SpecificityLogistic)
ComparisonDF=round(cbind.data.frame(DecisionTree,Logistic_1_cut_off_0.086),0)
        #
      #   #
    #       #
  #           #
##Classification tree
View(DATA)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)


#install.packages("lattice") 
#install.packages("caret")
#library(caret)
#split <- createDataPartition(y=DATA$y, p = 0.5, list=FALSE)
#train <- DATA[split,]
#test <- DATA[-split,]

trees <- tree(y ~., DATA)
plot(trees)

##Reassigning 1 to yes and o to no default
DATA$y=ifelse(DATA$y==1,"yes","no")

tree.fit <-rpart(y~.,method="class",data=DATA)
plot(tree.fit,uniform=TRUE,main="Classification for y")
#text() labels classification tree we called tree.fit.cex= changes font size
#all=TRUE says we want a name for colour also?
text(tree.fit,use.n=TRUE, all=TRUE,cex=.4)

#Make tree pretty. The tree.fit is the name of the classification tree we defined in the 
#previous step.type denotes how we would like the plot to be labeled.
#tweak changes font size.  1.4 means 40% larger font.

prp(tree.fit, type = 4, extra = 101,leaf.round = 0, fallen.leaves = TRUE, varlen = 0, tweak = 1.4)
#The percentages shown are total of numbers in that box as the proportion of total
#population, not just the colour we want. That's why top has G vs PURPLE but is 100%

#Creating predicted values
Predictions <- rpart.predict(tree.fit, type="class")
table(Predictions, DATA$y)
#Optional: to see the list of all 500 predicted values, type the command
Prediction<-rpart.predict(tree.fit,type="class")
#QSTN. This table(Predictions) doesnt give totals why?
##Overall Accuracy of classification=tOTAL CORRECT/tOTAL SAMPLES

nnyy=35191+2407
nnyynyyn=35191+2407+1357+2233
accuracy=(35191+2407)/(35191+1357+2407+2233)*100
accuracy
#SENSITIVITY OF CLASSIFICATION= Total correct of the variable we're interested in
#E.G If purple, total purple identified as purple/all PURPLE
sensitivity=2407/(2407+2233)*100
sensitivity

#SPECIFICITY = correctly identified other variable i.e.green/total actual other variable
#specificity=ie correctly classifies as green/total green
specificity=35191/(35191+1357)*100
DecisionTree=rbind(accuracy,sensitivity,specificity)
`colnames<-`(DecisionTree,c("DecisionTree"))
`rownames<-`(DecisionTree,c("Accuracy","Sensitivity","Specificity"))
DecisionTree=as.data.frame(DecisionTree,row.names = rownames)









#To get the 2 best variables, We need to install a package called leaps.  
#The regsubsets() function will give us the "best" models for a preferred 
#number of variables!


#
library(leaps)
ListBestSubsets1 <- regsubsets(y ~ ., data=DATA, nbest=1)

summary(ListBestSubsets1)

#checking first for interractions

library(ggplot2)

ggplot(data=biomass, aes(x = Ph, y = Biomass, color=as.factor(Loc))) + geom_point() + geom_smooth(method="lm")
#.*. means all factors and all interactions
OverlyFullModel <- lm(Biomass ~ .*., data=biomassNoID)

summary(OverlyFullModel)

#Reducing the model
ReducedModel2 <- step(OverlyFullModel)

summary(ReducedModel2)

#We can create a model with fewer interaction terms
#This model includes interactions between the quantitative independent variables 
#(Na, D, Zn, Ph, and Sal) with the categorical variables (Location and Type).
MediumFullModel <- lm(Biomass ~ . + (Na + K + Zn + Ph + Sal)*(Loc + Type), 
                      data=biomassNoID)

summary(MediumFullModel)

ReducedModel3 <- step(MediumFullModel)

summary(ReducedModel3)
#still complicaed. step dont help. lest use regsubets
ListBestSubsets2 <- regsubsets(Biomass ~ . + (Na + K + Zn + Ph + Sal)*(Loc + Type),
                               data=biomassNoID)

summary(ListBestSubsets2)
Best4 = lm(Biomass ~ Loc*Ph +Sal*Type, data=biomassNoID)
summary(Best4)


a= 2491.654+8.116-53.561-2815.171+410.036+100.104*(Sal*Type3)
