################ Clearing everything ################

rm(list = ls())

################ Loading Packages ################

library(tidyverse) 
library(ggplot2)
library(dplyr)
library(forcats)
library(forecast)
library(lubridate)
library(stringr)
library(dlookr)
library(corrplot)
library(rpart)
library(rpart.plot)
library(mlbench)
library(randomForest)
library(caret)
library(pROC)
library(gbm)
library(parallel)
library(ROCR)
library(pdp)
library(doParallel)
library(corrplot)
library(dygraphs)
library(xts)
library(glmnet)
library(magrittr)
library(ROSE)

################ Reading dataset ################

dset <- read.csv(file = "AT2_credit_train.csv")

################ Summarizing Dataset ################

head(dset)
summary(dset)
str(dset)
colnames(dset)
sum(is.na(dset))

################ Cleaning dataset ################

## ID column dropped 

dset <- dset[-1] 

## Labeled sex column as a Gender

colnames(dset)[2] <- "Gender" 

## Checking gender column 

unique(dset$Gender)

## Set Gender as factors 

dset$Gender <- as.factor(ifelse(dset$Gender == 1 , "Male" ,
                                ifelse(dset$Gender == 2, "Female", "NULL")))

## Checking gender column again

unique(dset$Gender)

## Set Education as factors

dset$EDUCATION <- as.factor(ifelse(dset$EDUCATION == 1 , "Graduate_School" , 
                                   ifelse(dset$EDUCATION == 2 , "University" ,
                                          ifelse(dset$EDUCATION == 3 , "High_school" , 
                                                 ifelse(dset$EDUCATION == 4 , "Other" , "Unknown")))))   

## Set Marriage as factors

dset$MARRIAGE <- as.factor(ifelse(dset$MARRIAGE == 1 , "Married" , 
                                  ifelse(dset$MARRIAGE == 2 , "Single" , "Other")))        


## Set default as factors 

dset$default <- as.factor(ifelse(dset$default == "Y" , "Yes" , "No")) 

################ Visualizing EDA ################

## Initial exploratory data analysis. Plotting charts of demographic breakouts

gender <- ggplot(dset, aes(Gender, fill = Gender)) + 
  geom_bar(size = 0.5) + scale_fill_brewer(palette = "Paired")
gender


edu <- ggplot(dset, aes(EDUCATION, fill = EDUCATION)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
edu

marital <- ggplot(dset, aes(MARRIAGE, fill = MARRIAGE)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
marital


p0 <- ggplot(dset, aes(PAY_0, fill = PAY_0)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p0

p2 <- ggplot(dset, aes(PAY_2, fill = PAY_2)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p2

p3 <- ggplot(dset, aes(PAY_3, fill = PAY_3)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p3

p4 <- ggplot(dset, aes(PAY_4, fill = PAY_4)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p4

p5 <- ggplot(dset, aes(PAY_5 , fill = PAY_5)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p5

p6 <- ggplot(dset, aes(PAY_6, fill = PAY_6)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p6


## Plotting age vs gender

age <- ggplot(dset, aes(AGE, fill = Gender)) + 
  geom_histogram(binwidth = 0.75)
age

## Replacing invalid age

range(dset$AGE)
median(dset$AGE)
dset[c(which(dset$AGE>120)),5] = 34 

## Plotting education vs gender

edu2 <- ggplot(dset, aes(EDUCATION, Gender, color = Gender)) + 
  geom_count() 
edu2

## Visualize credit limits. We can identify 1,000,000 as an outlier

limit <- ggplot(dset) + 
  geom_qq(aes(sample=LIMIT_BAL))
limit


limit6 <- ggplot(dset, aes(LIMIT_BAL, fill = EDUCATION)) + 
  geom_histogram() + scale_fill_brewer(palette = "Paired")
limit6


## Visualize credit limits compared to default. Overall, people who default have lower credit limits.

limit2 <- ggplot(dset, aes(default, LIMIT_BAL, fill = default)) + 
  geom_boxplot()
limit2

## Visualize credit limits compared to education

limit3 <- ggplot(dset, aes(EDUCATION, LIMIT_BAL, fill = EDUCATION)) + 
  geom_boxplot()
limit3

## List all limit balance values

unique(dset$LIMIT_BAL)

## Proportion of cards that default

prop.table(table(dset$default))

## Credit default by Gender

ggplot(dset, aes(x = Gender, fill = default)) +
  geom_bar() +
  labs(x = 'Gender',title="Credit Default by Gender") +
  theme_classic() +
  stat_count(aes(label = ..count..), geom = "label")


################ New Features for Payment columns ################
## Assign -1 to all customer with on-time payment or in front payment
for(i in 1:nrow(dset)){
  if(dset$PAY_0[i] == -2 | dset$PAY_0[i] == -1 | dset$PAY_0[i] == 0){
    dset$PAY_0[i] = 0
  }
  if(dset$PAY_2[i] == -2 | dset$PAY_2[i] == -1 | dset$PAY_2[i] == 0){
    dset$PAY_2[i] = 0
  }
  if(dset$PAY_3[i] == -2 | dset$PAY_3[i] == -1 | dset$PAY_3[i] == 0){
    dset$PAY_3[i] = 0
  }
  if(dset$PAY_4[i] == -2 | dset$PAY_4[i] == -1 | dset$PAY_4[i] == 0){
    dset$PAY_4[i] = 0
  }
  if(dset$PAY_5[i] == -2 | dset$PAY_5[i] == -1 | dset$PAY_5[i] == 0){
    dset$PAY_5[i] = 0
  }
  if(dset$PAY_6[i] == -2 | dset$PAY_6[i] == -1 | dset$PAY_6[i] == 0){
    dset$PAY_6[i] = 0
  }
}


## Assign 3 to all customer with more than 3 months delay payment
for(i in 1:nrow(dset)){
  if(dset$PAY_0[i] == 3 | dset$PAY_0[i] == 4 | dset$PAY_0[i] == 5 | dset$PAY_0[i] == 6 ){
    dset$PAY_0[i] = 3
  }
  if(dset$PAY_2[i] == 3 | dset$PAY_2[i] == 4 | dset$PAY_2[i] == 5 | dset$PAY_2[i] == 6 ){
    dset$PAY_2[i] = 3
  }
  if(dset$PAY_3[i] == 3 | dset$PAY_3[i] == 4 | dset$PAY_3[i] == 5 | dset$PAY_3[i] == 6 ){
    dset$PAY_3[i] = 3
  }
  if(dset$PAY_4[i] == 3 | dset$PAY_4[i] == 4 | dset$PAY_4[i] == 5 | dset$PAY_4[i] == 6 ){
    dset$PAY_4[i] = 3
  }
  if(dset$PAY_5[i] == 3 | dset$PAY_5[i] == 4 | dset$PAY_5[i] == 5 | dset$PAY_5[i] == 6 ){
    dset$PAY_5[i] = 3
  }
  if(dset$PAY_6[i] == 3 | dset$PAY_6[i] == 4 | dset$PAY_6[i] == 5 | dset$PAY_6[i] == 6 ){
    dset$PAY_6[i] = 3
  }
}

## Set payment columns as factors

dset[6:11] <- lapply(dset[6:11], factor) 

## Ploting the new payment factors

p00 <- ggplot(dset, aes(PAY_0, fill = PAY_0)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p00

p22 <- ggplot(dset, aes(PAY_2, fill = PAY_2)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p22

p33 <- ggplot(dset, aes(PAY_3, fill = PAY_3)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p33

p44 <- ggplot(dset, aes(PAY_4, fill = PAY_4)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p44

p55 <- ggplot(dset, aes(PAY_5 , fill = PAY_5)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p55

p66 <- ggplot(dset, aes(PAY_6, fill = PAY_6)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p66

################ Resolving Unbalanced Dataset ################

## Default proportions

prop.table(table(dset$default))

## Graphing default proportions 

ggplot(dset, aes(x=default)) +
  geom_bar(stat = "count") +
  labs(title = "Distribution of Customers who Default")

## Synthetic minority sample generation 
dset_balance <- ovun.sample(default ~., data = dset, method = "both", p = 0.5,
                            N = 23101, seed = 1)$data

## Graphing new default proportions 
ggplot(dset_balance, aes(x=default)) +
  geom_bar(stat = "count") +
  labs(title = "Distribution of Customers who Default")

################ Creating train & test partitions ################

set.seed(40)
traintest_split_bal <- createDataPartition(dset_balance$default,p=0.7,list=FALSE)
train_bal <- dset_balance[traintest_split_bal,] 
test_bal <- dset_balance[-traintest_split_bal,]

## Graphing default proportions in training dataset
ggplot(train_bal, aes(x=default)) +
  geom_bar(stat = "count") +
  labs(title = "Distribution of Customers who default")

## Graphing default proportions in testing dataset
ggplot(test_bal, aes(x=default)) +
  geom_bar(stat = "count") +
  labs(title = "Distribution of Customers who default")

################ Train with GBM ################

## Control factors
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     classProbs = TRUE , 
                     summaryFunction = twoClassSummary , 
                     allowParallel =  TRUE)


## Set training space

gbm <- expand.grid(interaction.depth = c(1,2,5), 
                   n.trees = c(50, 100), 
                   shrinkage = c(0.01,0.05), 
                   n.minobsinnode = c(5,10))

## Training the gbm model

gbm_train <- train(default~., 
                   data = train_bal, 
                   method = "gbm",
                   trControl = ctrl, 
                   tuneGrid = gbm, 
                   metric = "ROC")

## Get results

gbm_train$results
print(gbm_train)
plot(gbm_train)

## Predictions

train_bal$prediction <- predict(gbm_train, train_bal, type = "raw")
train_bal$probability <- predict(gbm_train, train_bal, type = "prob")[,2]

test_bal$prediction <- predict(gbm_train,test_bal, type = "raw")
test_bal$probability <- predict(gbm_train, test_bal, type = "prob")[,2]

## Confusion matrix

confusionMatrix(train_bal$prediction, train_bal$default, mode = "everything")
confusionMatrix(test_bal$prediction, test_bal$default, mode = "everything")

## Variable Importance in GBM 

impo <- gbm_train %>% 
  varImp() %>%
  extract2("importance") %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(Overall))

impo %>% head(20) %>%
  ggplot(aes(Overall, reorder(variable, Overall))) +
  geom_point() +
  geom_segment(aes(y=variable, yend = variable, x = 0 , xend = Overall)) +
  labs(title = "Variable Importance",
       y= "Variable",
       x= "Importance")

## AUC on training dataset

ROC_obj_traininggbm <- roc(train_bal$default, train_bal$probability)
train_roc_gbm <-auc(ROC_obj_traininggbm)
train_roc_gbm

## AUC on testing dataset 

ROC_obj_testgbm <- roc(test_bal$default, test_bal$probability)
test_roc_gbm <-auc(ROC_obj_testgbm)
test_roc_gbm


################ Validation Dataset ################
################ Reading Validation Dataset ################

dtest <- read.csv(file = "AT2_credit_test.csv")

################ Summarising Dataset ################

head(dtest)
summary(dtest)
str(dtest)
colnames(dtest)
sum(is.na(dtest))

################ Cleaning Validation Dataset ################

## ID column dropped

dtest <- dtest[-1] 

## Labeled sex column as a Gender

colnames(dtest)[2] <- "Gender" 

## Checking gender column 

unique(dtest$Gender)

## Set Gender as factors 
dtest$Gender <- as.factor(ifelse(dtest$Gender == 1 , "Male" ,
                                 ifelse(dtest$Gender == 2, "Female", "NULL"))) 

## Set Education as factors
dtest$EDUCATION <- as.factor(ifelse(dtest$EDUCATION == 1 , "Graduate_School" , 
                                    ifelse(dtest$EDUCATION == 2 , "University" ,
                                           ifelse(dtest$EDUCATION == 3 , "High_school" , 
                                                  ifelse(dtest$EDUCATION == 4 , "Other" , "Unknown")))))   

## Set Marriage as factors
dtest$MARRIAGE <- as.factor(ifelse(dtest$MARRIAGE == 1 , "Married" , 
                                   ifelse(dtest$MARRIAGE == 2 , "Single" , "Other")))        

## Checking age column 

range(dtest$AGE)

################ New Features for Payment columns ################
## Assign -1 to all customer with on-time payment or in front payment
for(i in 1:nrow(dtest)){
  if(dtest$PAY_0[i] == -2 | dtest$PAY_0[i] == -1 | dtest$PAY_0[i] == 0){
    dtest$PAY_0[i] = 0
  }
  if(dtest$PAY_2[i] == -2 | dtest$PAY_2[i] == -1 | dtest$PAY_2[i] == 0){
    dtest$PAY_2[i] = 0
  }
  if(dtest$PAY_3[i] == -2 | dtest$PAY_3[i] == -1 | dtest$PAY_3[i] == 0){
    dtest$PAY_3[i] = 0
  }
  if(dtest$PAY_4[i] == -2 | dtest$PAY_4[i] == -1 | dtest$PAY_4[i] == 0){
    dtest$PAY_4[i] = 0
  }
  if(dtest$PAY_5[i] == -2 | dtest$PAY_5[i] == -1 | dtest$PAY_5[i] == 0){
    dtest$PAY_5[i] = 0
  }
  if(dtest$PAY_6[i] == -2 | dtest$PAY_6[i] == -1 | dtest$PAY_6[i] == 0){
    dtest$PAY_6[i] = 0
  }
}


## Assign 3 to all customer with more than 3 months delay payment
for(i in 1:nrow(dtest)){
  if(dtest$PAY_0[i] == 3 | dtest$PAY_0[i] == 4 | dtest$PAY_0[i] == 5 | dtest$PAY_0[i] == 6 ){
    dtest$PAY_0[i] = 3
  }
  if(dtest$PAY_2[i] == 3 | dtest$PAY_2[i] == 4 | dtest$PAY_2[i] == 5 | dtest$PAY_2[i] == 6 ){
    dtest$PAY_2[i] = 3
  }
  if(dtest$PAY_3[i] == 3 | dtest$PAY_3[i] == 4 | dtest$PAY_3[i] == 5 | dtest$PAY_3[i] == 6 ){
    dtest$PAY_3[i] = 3
  }
  if(dtest$PAY_4[i] == 3 | dtest$PAY_4[i] == 4 | dtest$PAY_4[i] == 5 | dtest$PAY_4[i] == 6 ){
    dtest$PAY_4[i] = 3
  }
  if(dtest$PAY_5[i] == 3 | dtest$PAY_5[i] == 4 | dtest$PAY_5[i] == 5 | dtest$PAY_5[i] == 6 ){
    dtest$PAY_5[i] = 3
  }
  if(dtest$PAY_6[i] == 3 | dtest$PAY_6[i] == 4 | dtest$PAY_6[i] == 5 | dtest$PAY_6[i] == 6 ){
    dtest$PAY_6[i] = 3
  }
}

## Set payment columns as factors
dtest[6:11] <- lapply(dtest[6:11], factor) 

## Ploting the new payment factors

p00 <- ggplot(dtest, aes(PAY_0, fill = PAY_0)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p00

p22 <- ggplot(dtest, aes(PAY_2, fill = PAY_2)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p22

p33 <- ggplot(dtest, aes(PAY_3, fill = PAY_3)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p33

p44 <- ggplot(dtest, aes(PAY_4, fill = PAY_4)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p44

p55 <- ggplot(dtest, aes(PAY_5 , fill = PAY_5)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p55

p66 <- ggplot(dtest, aes(PAY_6, fill = PAY_6)) + 
  geom_bar() + scale_fill_brewer(palette = "Paired")
p66

################ Cleaning Validation Dataset ################

validation_prediction <- predict(gbm_train, dtest, type = "prob")[,2]

################ Binding ID Column to Prediction ################

id_dtest <- read.csv(file = "AT2_credit_test.csv")
id <- id_dtest$ID
gbm_kaggle <- as.data.frame(cbind(id, validation_prediction))

## Renaming Columns 

gbm_kaggle$ID <- gbm_kaggle$id
gbm_kaggle$default <- gbm_kaggle$validation_prediction

################ Export into CSV ################

gbm_kaggle <- gbm_kaggle %>%
  select(ID, default)
write.csv(gbm_kaggle, file = "THE GRADIENT ASCENDERS PREDICTIONS.csv", row.names = FALSE)
