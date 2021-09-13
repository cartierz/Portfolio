install.packages("dplyr")
install.packages("tidyverse")
install.packages("ISLR")
install.packages("ggplot2")
install.packages("corrplot")
install.packages('lubridate')
install.packages("data.table")
install.packages("gbm")
install.packages("parallel")
install.packages("doParallel")
install.packages("glmnet")
install.packages(ROCR)
install.packages(caret)
install.packages("pdp")
devtools::install_github("cran/ggplot2")
library("tidyverse")
library("plyr")
library("dplyr")
library("ggplot2")
library("ISLR")
library("corrplot")
library("lubridate")
library("data.table")
library("magrittr")
library(gbm)
library(parallel)
library(doParallel)
library(ROCR)
library(glmnet)
library(caret)
library (rpart)
library(rpart.plot)
library(mlbench)
library(randomForest)
library(pdp)

#Read csv into a data frame and summarise
repurchase_training <- read.csv(file = "repurchase_training.csv")

########################1. EXPLORATORY DATA ANALYSIS######################################################
#####################################CLEANING
#duplicate values
retraining <- unique(repurchase_training)

#summary, structure and examination for invalid values
summary(retraining)
str(retraining)
unique(retraining$Target)
unique(retraining$age_band)
unique(retraining$car_model)
unique(retraining$car_segment)

#invalid values
filter(retraining,car_model=="model_19") 
which(retraining$ID == 30920 | retraining$ID == 100546)
retraining <- retraining[-c(26319,85668),]

# missing values test
sum(is.na(retraining))

###############################VISUALISING AND DESCRIBING
#scatter plot - distribution for numerical variables
par(mfrow = c(3,4))
counter = 0
for (variable in colnames(retraining[, 7:17])) {
  plot(retraining[, variable], retraining$Target, main = variable, ylab = "Target", xlab = variable)
  counter = counter + 1
  if (counter %% 12 == 0) {
    readline(prompt = "Hit ENTER to show more plots")
  }
}   

#bar graph distribution for categorical variables
ggplot(data = retraining) + 
  geom_bar(mapping = aes(x = age_band)) + 
  labs(x = "Age Band", title = "Proportion of Customers in Each Age Band") 

ggplot(data = retraining) + 
  geom_bar(mapping = aes(x = gender)) + 
  labs(x = "Gender", title = "Proportion of Customers in Each Gender") 

ggplot(data = retraining) + 
  geom_bar(mapping = aes(x = car_model))+ 
  labs(x = "Car Model", title = "Proportion of Each Car Model") 

ggplot(data = retraining) + 
  geom_bar(mapping = aes(x = car_segment)) + 
  labs(x = "Car Segment", title = "Proportion of Each Car Segment") 

#boxplotting for outlier detection
par(mfrow = c(3,4))
counter = 0
for (variable in colnames(retraining[, 7:17])) {
  boxplot(retraining[, variable], main = variable, ylab = variable)
  counter = counter + 1
  if (counter %% 12 == 0) {
    readline(prompt = "Hit ENTER to show more plots")
  }
}

#correlation matrix
mcor <- cor(retraining[7:17])
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", addCoef.col = "black")

#Proportion of purchases 
retraining_table <- table(retraining$Target)
prop.table(retraining_table)
retraining$ID <- NULL
#######################################2. CREATING MODEL GLM##################################################
#2. A)
#partitioning
trainset_size <- floor(0.75 * nrow(retraining))
set.seed(42) 
trainset_indices <- sample(seq_len(nrow(retraining)), size = trainset_size)
trainset <- retraining[trainset_indices, ]
testset <- retraining[-trainset_indices,]

#checking number of observations in trainset & testset
nrow(trainset)
nrow(testset)

#logistic regression model
retraining.glm = glm(formula = Target ~ .,
                     data = trainset,
                     family = "binomial")
summary(retraining.glm)
#What's the AIC value?
#15340
#removing variables to drop the AIC
retraining1.glm = glm(formula = Target ~ age_band + gender + car_model + car_segment
                      + age_of_vehicle_years + sched_serv_warr + sched_serv_paid 
                      + non_sched_serv_paid + total_services + total_paid_services +
                        mth_since_last_serv + annualised_mileage + 
                        num_dealers_visited + num_serv_dealer_purchased,
                      data = trainset, family = "binomial")
summary(retraining1.glm) #AIC 15340 (less non_sched_serv_warr)

retraining2.glm = glm(formula = Target ~ age_band + gender + car_model + 
                        age_of_vehicle_years + sched_serv_warr + sched_serv_paid 
                      + non_sched_serv_paid + total_services + total_paid_services +
                        mth_since_last_serv + annualised_mileage + 
                        num_dealers_visited + num_serv_dealer_purchased, 
                      data = trainset, family = "binomial")
summary(retraining2.glm) #AIC 15340 (less car_segment and non_sched_serv_warr)

#prediction and probabilities
trainset$probability <- predict(retraining2.glm, newdata = trainset, type = "response")
lapply(X = trainset[trainset$Target == 1, ], 
       FUN = summary, basic = TRUE)
lapply(X = trainset[trainset$Target == 0, ], 
       FUN = summary, basic = TRUE)
trainset$prediction = "0"
trainset[trainset$probability >= 0.0446841, "prediction"] = "1" #number from 1st quartile target=1testset$probability = predict(retraining2.glm, newdata = testset, type = "response")
testset$probability <- predict(retraining2.glm, newdata = testset, type = "response")
lapply(X = testset[testset$Target == 1, ], 
       FUN = summary, basic = TRUE)
lapply(X = testset[testset$Target == 0, ], 
       FUN = summary, basic = TRUE)
testset$prediction = "0"
testset[testset$probability >= 0.0446841, "prediction"] = "1" #number from 1st quartile target=1

#Confusion Matrix
cfm <- table(predicted=testset$prediction,true=testset$Target)
cfm

#Precision
Precision <- 666/(666+4264)

#Recall
Recall <- 666/(666+229)

#F1
F1 <- 2*((Precision*Recall)/(Precision+Recall))

#accuracy
glm_accuracy <- (27675+666)/(27675+666+229+4264)

####################################AUC
#prediction objects for AUC
train_pred = prediction(trainset$probability, trainset$Target)
test_pred = prediction(testset$probability, testset$Target)

#training TPR and FPR
train_tpr_fpr = performance(train_pred, "tpr","fpr")
train_auc = performance(train_pred, "auc")

#testing TPR and FPR
test_tpr_fpr = performance(test_pred, "tpr","fpr")
test_auc = performance(test_pred, "auc")

#TPR and FPR gains chart ROC for both testing and training data
plot(test_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

#AUC figures
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

test_auc = unlist(slot(test_auc, "y.values"))
test_auc

##################### 3. tree based classification model to predict which customers repurchase###########
#partitioning
retraining$Target <- as.factor(retraining$Target)
retraining$age_band <- as.factor(retraining$age_band)
retraining$gender <- as.factor(retraining$gender)
retraining$car_model <- as.factor(retraining$car_model)
retraining$car_segment <- as.factor(retraining$car_segment)

#cross validation
train_test_split <- createDataPartition(retraining$Target,p=0.75,list=FALSE)
training_data <- retraining[train_test_split,] 
test_data <- retraining[-train_test_split,]
cvSplits <- createFolds(training_data$Target,k=10,)

#initialise accuracy vector
accuracies <- rep(NA,length(cvSplits))
i <- 0

#loop 
for (testset_indices in cvSplits){
  i <- i+1
  trainsetcv <- retraining[-testset_indices, ]
  testsetcv <- retraining[testset_indices, ]
  retrainingrf_cv <- randomForest(Target ~.,data = trainsetcv, ntree=100, importance=TRUE,testsetcv[,-1])
  
  # Accuracy on test data
  accuracies[i] <- mean(retrainingrf_cv$test$predicted==testsetcv$Target)
  
}

#model
retrainingrf = randomForest(Target ~.,data = training_data, ntree=100,importance=TRUE, test_data[,-1], keep.forest = TRUE)

#prediction
predictions_rf <- data.frame(test_data,retrainingrf$test$predicted)

#Accuracy for test set
mean(retrainingrf$test$predicted==test_data$Target)

#Confusion matrix
table(predicted=predictions_rf$retrainingrf.test.predicted,true=test_data$Target)

#Precision
Precision_rf <- 710/(710+29)

#Recall
Recall_rf <- 710/(710+170)

#F1
F1_rf <- 2*((Precision_rf*Recall_rf)/(Precision_rf+Recall_rf))

############################Partial dependency plot
# Quantitative measure of variable importance
imp = importance(retrainingrf)
varImpPlot(retrainingrf)

#Plots
grid.arrange(
  partial(retrainingrf, pred.var = "mth_since_last_serv", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE, parallel=F, which.class = "1", train=training_data),
  partial(retrainingrf, pred.var = "annualised_mileage", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE,parallel=F, which.class="1",train=training_data),
  partial(retrainingrf, pred.var = "num_serv_dealer_purchased", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE, parallel=F, which.class = "1", train=training_data),
  partial(retrainingrf, pred.var = "age_of_vehicle_years", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE,parallel=F, which.class="1",train=training_data),
  partial(retrainingrf, pred.var = "total_services", plot = TRUE, rug = TRUE, 
          type="classification", prob=TRUE, parallel=F, which.class = "1", train=training_data),
  ncol = 3, nrow = 2
)
partialPlot(retrainingrf, training_data,"gender", which.class = "1", plot = TRUE, add= FALSE, rug = TRUE)

########################### 4. validation#############################
repurchase_validation <- read.csv(file = "repurchase_validation.csv")
#duplicate values
validation <- unique(repurchase_validation)
validation_data <- validation %>%
  select(-ID)
#summary, structure and examination for invalid values
summary(validation_data)
str(validation_data)
validation_data$age_band <- as.factor(validation_data$age_band)
validation_data$gender <- as.factor(validation_data$gender)
validation_data$car_model <- as.factor(validation_data$car_model)
validation_data$car_segment <- as.factor(validation_data$car_segment)
levels(validation_data$age_band)
levels(validation_data$car_model)
levels(validation_data$car_segment)

# missing values test
sum(is.na(validation_data))

#optimal model with validation data
validation_rf <- randomForest(Target ~.,data = training_data, ntree=100, 
                              importance=TRUE, validation_data, keep.forest = TRUE)
#prediction and probability
validation_prediction <- data.frame(validation_data,validation_rf$test$predicted)
validation_prediction$target_0_probability <- predict(validation_rf,validation_data, type = "prob")
validation_prediction <- validation_prediction %>%
  mutate(target_1_probability = abs(target_0_probability - 1))

#final output 
validation_prediction$target_0_probability <- NULL 
validation_prediction <- cbind(validation$ID,validation_prediction)
repurchase_validation_14171767 <- validation_prediction %>%
  select(`validation$ID`, "target_1_probability", "validation_rf.test.predicted") %>%
  `colnames<-`(c("ID", "target_probability", "target_class"))

#results
write.csv(repurchase_validation_14171767, file = "repurchase_validation_14171767.csv")
