library("ggplot2")
library("dplyr")
install.packages("xlsx")
library("xlsx")
library(ROCR)
library("corrplot")
devtools::install_github("cran/ggplot2")
library(tidyverse)
install.packages("neuralnet")
library(neuralnet)
library(caret)
library(randomForest)
library(ROCR)
library(pROC)
library(ROSE)
##data preprocessing ####
#read file into R
training_data <- read.csv(file = "Assignment3-TrainingData.csv")

#unique data and deletion of duplicates
training_data <- unique(training_data)

# summaries
str(training_data)
summary(training_data)

#removal of quote_id
training_data$Quote_Id <- NULL
training_data$Property_info2 <- NULL

#deleting rows with missing data 
unique(training_data$Geographic_info4)
training_data <- training_data[-c(which(training_data$Geographic_info4=="")),]

#recoding chr, missing values and NA
unique(training_data$Personal_info1) #missing
training_data$Personal_info1 <- ifelse(training_data$Personal_info1 == "", 2, ifelse(training_data$Personal_info1 == "N", 1, 0))

unique(training_data$Property_info1) #missing
training_data$Property_info1 <- ifelse(training_data$Property_info1 == "", 2, ifelse(training_data$Property_info1 == "N", 1, 0))

unique(training_data$Personal_info5) #NA
training_data[c(which(is.na(training_data$Personal_info5))),19] <- 0

alphabet <- as.data.frame(LETTERS)
alphabet$numeric <- seq_len(26)
training_data$Field_info1 <- match(training_data$Field_info1, alphabet$LETTERS)

#field_info3 to numeric
unique(training_data$Field_info3)
training_data[c(which(training_data$Field_info3 == "1,165")),5] <- "1165"
training_data[c(which(training_data$Field_info3 == "1,113")),5] <- "1113"
training_data[c(which(training_data$Field_info3 == "1,487")),5] <- "1487"
training_data[c(which(training_data$Field_info3 == "1,480")),5] <- "1480"
training_data$Field_info3 <- as.numeric(training_data$Field_info3)

#field_info4 to numeric
unique(training_data$Field_info4)
training_data$Field_info4 <- ifelse(training_data$Field_info4 == "N", 1, 0)

#coverage_info3 to numeric
sort(unique(training_data$Coverage_info3))
training_data$Coverage_info3 <- match(training_data$Coverage_info3, alphabet$LETTERS)

#sales_info4 to numeric
sort(unique(training_data$Sales_info4))
training_data$Sales_info4 <- match(training_data$Sales_info4, alphabet$LETTERS)

#personal_info3 to numeric
personal3_index <- as.data.frame(sort(unique(training_data$Personal_info3)))
personal3_index$numeric_og <- seq_len(38)

personal3_alpha <- as.data.frame(training_data$Personal_info3)
personal3_alpha <- personal3_alpha %>%
  mutate(first = substring(training_data$Personal_info3,1,1)) %>% 
  mutate(second = substring(training_data$Personal_info3,2))
personal3_alpha <- personal3_alpha %>%
  mutate(first = match(first, alphabet$LETTERS)) %>%
  mutate(second = match(second, alphabet$LETTERS)) 
  
training_data <- cbind(training_data, personal3_alpha$first, personal3_alpha$second)
training_data$personal3_num <- match(training_data$Personal_info3, personal3_index$`sort(unique(training_data$Personal_info3))`)
training_data$Personal_info3 <- NULL

#property_info3 to numeric
sort(unique(training_data$Property_info3))
training_data$Property_info3 <- match(training_data$Property_info3, alphabet$LETTERS)

#geographic_info4 to numeric
sort(unique(training_data$Geographic_info4))
training_data$Geographic_info4 <- ifelse(training_data$Geographic_info4 == "N", 1, 0)

#geographic_info5 to numeric
sort(unique(training_data$Geographic_info5))
training_data$Geographic_info5 <- recode(training_data$Geographic_info5,"CA" = 1,
                                         "IL" = 2, "NJ" = 3, "TX" = 4)

# coding character as numeric
library(lubridate)
training_data <- training_data %>%
  mutate(Quote_Month = month(Quote_Date)) %>%
  mutate(Quote_Day = day(dmy(Quote_Date))) %>%
  mutate(Quote_Year = year(dmy(Quote_Date))) %>%
  mutate(Sequential_Month = ((12*((Quote_Year)-2012))+(Quote_Month))) %>%
  mutate(day_of_year = yday(Quote_Date))

as.numeric(as.Date(min(training_data$Quote_Date), format = "%d/%m/%y", origin = " 01/01/2013"))
training_data$Quote_Date <- (as.numeric(as.Date(training_data$Quote_Date, format = "%d/%m/%y", origin = " 01/01/2013")) - 15706)

#correlation matrix
mcor <- cor(training_data[1:35])
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black", addCoef.col = "black")
corrplot(mcor, method = "shade", shade.col = NA, tl.col = "black")

cor(training_data$Quote_Date, training_data$Sequential_Month)
training_data$Quote_Date <- NULL #redundant

cor(training_data$Field_info1, training_data$Geographic_info5)
training_data$Geographic_info5 <- NULL # redundant 

mcor_data <- as.data.frame(mcor)
mcor_data_flag <- as.data.frame(abs(mcor_data$Quote_Flag))
training_data$Sales_info5 <- NULL # sales5 low correlation to quote_flag
training_data$alpha_Quote_Flag <- ifelse(training_data$Quote_Flag== 0, "No", "Yes")
training_data$Quote_Flag <- NULL
write.csv(training_data, file = "training_data_r_pp.csv")
#########################################################################################
##UNKNOWN##
##data preprocessing ####
#read file into R
unknown_data <- read.csv(file = "Assignment3-UnknownData.csv")

#unique data and deletion of duplicates
unknown_data <- unique(unknown_data)

#removal of quote_id and property 2 
unknown_data_pp <- subset(unknown_data, select = -Quote_Id)
unknown_data_pp$Property_info2 <- NULL

# summaries
str(unknown_data_pp)
summary(unknown_data_pp)

#deleting rows with missing data 
unique(unknown_data_pp$Geographic_info4)

#recoding chr, missing values and NA
unique(unknown_data_pp$Personal_info1)
unknown_data_pp$Personal_info1 <- ifelse(unknown_data_pp$Personal_info1 == "", 2, ifelse(unknown_data_pp$Personal_info1 == "N", 1, 0))

unique(unknown_data_pp$Property_info1)
unknown_data_pp$Property_info1 <- ifelse(unknown_data_pp$Property_info1 == "", 2, ifelse(unknown_data_pp$Property_info1 == "N", 1, 0))

unique(unknown_data_pp$Personal_info5)
unknown_data_pp[c(which(is.na(unknown_data_pp$Personal_info5))),18] <- "0"
unknown_data_pp$Personal_info5 <- as.numeric(unknown_data_pp$Personal_info5)

unknown_data_pp$Field_info1 <- match(unknown_data_pp$Field_info1, alphabet$LETTERS)

#field_info3 to numeric
unique(unknown_data_pp$Field_info3)
unknown_data_pp[c(which(unknown_data_pp$Field_info3 == "1,165")),4] <- "1165"
unknown_data_pp[c(which(unknown_data_pp$Field_info3 == "1,113")),4] <- "1113"
unknown_data_pp[c(which(unknown_data_pp$Field_info3 == "1,487")),4] <- "1487"
unknown_data_pp[c(which(unknown_data_pp$Field_info3 == "1,480")),4] <- "1480"
unknown_data_pp$Field_info3 <- as.numeric(unknown_data_pp$Field_info3)

#field_info4 to numeric
unique(unknown_data_pp$Field_info4)
unknown_data_pp$Field_info4 <- ifelse(unknown_data_pp$Field_info4 == "N", 1, 0)

#coverage_info3 to numeric
sort(unique(unknown_data_pp$Coverage_info3))
unknown_data_pp$Coverage_info3 <- match(unknown_data_pp$Coverage_info3, alphabet$LETTERS)

#sales_info4 to numeric
sort(unique(unknown_data_pp$Sales_info4))
unknown_data_pp$Sales_info4 <- match(unknown_data_pp$Sales_info4, alphabet$LETTERS)

#personal_info3 to numeric
unique_unknown_personal3 <- as.data.frame(sort(unique(unknown_data_pp$Personal_info3))) #index of unique class in unknown data
unique_unknown_personal3$class <- unique_unknown_personal3$`sort(unique(unknown_data_pp$Personal_info3))`
unique_unknown_personal3$`sort(unique(unknown_data_pp$Personal_info3))` <- NULL

personal3_index$class <- personal3_index$`sort(unique(training_data$Personal_info3))`
personal3_index$`sort(unique(training_data$Personal_info3))` <- NULL

combined_personal3 <- as.data.frame(append(personal3_index$class, #appending index from training to unknown index 
                                           unique_unknown_personal3$class))
combined_personal3$class <- combined_personal3$`append(personal3_index$class, unique_unknown_personal3$class)`
combined_personal3$`append(personal3_index$class, unique_unknown_personal3$class)` <- NULL

master_list_personal3 <- as.data.frame(sort(unique(combined_personal3$class))) #unique sorted classes for personal 3
master_list_personal3$class <- master_list_personal3$`sort(unique(combined_personal3$class))`
master_list_personal3$`sort(unique(combined_personal3$class))` <- NULL

master_list_personal3$numeric <- seq_len(42) # new numeric code for personal_3


## revising personal 3 variables in training data
training_data$personal3_alpha1 <-training_data$`personal3_alpha$first`
training_data$`personal3_alpha$first` <-NULL
training_data$personal3_alpha2 <-training_data$`personal3_alpha$second`
training_data$`personal3_alpha$second` <-NULL
training_data$personal3_num<- personal3_index[c(match(training_data$personal3_num,
                                                      personal3_index$numeric_og)),]$class # regenerate original personal3 column
training_data$personal3_num <- match(training_data$personal3_num, 
                                     master_list_personal3$class) #coded numerically with new classes sorted 


## unknown personal 3 coded to numeric
unknown_data_pp$personal3_num <- match(unknown_data_pp$Personal_info3, 
                                     master_list_personal3$class) 
unknown_data_pp <- unknown_data_pp %>%
  mutate(personal3_alpha1 = substring(unknown_data_pp$Personal_info3,1,1)) %>%
  mutate(personal3_alpha2 = substring(unknown_data_pp$Personal_info3,2))
unknown_data_pp$personal3_alpha1 <- match(unknown_data_pp$personal3_alpha1, alphabet$LETTERS)
unknown_data_pp$personal3_alpha2 <- match(unknown_data_pp$personal3_alpha2, alphabet$LETTERS)
unknown_data_pp$Personal_info3 <- NULL
#property_info3 to numeric
sort(unique(unknown_data_pp$Property_info3))
unknown_data_pp$Property_info3 <- match(unknown_data_pp$Property_info3, alphabet$LETTERS)

#geographic_info4 to numeric
sort(unique(unknown_data_pp$Geographic_info4))
unknown_data_pp$Geographic_info4 <- ifelse(unknown_data_pp$Geographic_info4 == "N", 1, 0)

#geographic_info4 to numeric
sort(unique(unknown_data_pp$Geographic_info5))
unknown_data_pp$Geographic_info5 <- recode(unknown_data_pp$Geographic_info5,"CA" = 1,
                                        "IL" = 2, "NJ" = 3, "TX" = 4)
# coding character as numeric
library(lubridate)
unknown_data_pp <- unknown_data_pp %>%
  mutate(Quote_Month = month(Quote_Date)) %>% #quote_month var
  mutate(Quote_Day = day(dmy(Quote_Date))) %>% #quote_day var
  mutate(Quote_Year = year(dmy(Quote_Date))) %>% #quote_year var
  mutate(Sequential_Month = ((12*((Quote_Year)-2012))+(Quote_Month))) %>% #month in sequence of months from initial date var
  mutate(day_of_year = yday(Quote_Date)) # day of the year

as.numeric(as.Date(min(unknown_data_pp$Quote_Date), format = "%d/%m/%y", origin = " 01/01/2013")) #finding the numeric format of the first date
unknown_data_pp$Quote_Date <- (as.numeric(as.Date(unknown_data_pp$Quote_Date, format = "%d/%m/%y", origin = " 01/01/2013")) - 15706)

#correlation matrix
mcor_unk <- cor(unknown_data_pp[1:34])
corrplot(mcor_unk, method = "shade", shade.col = NA, tl.col = "black", addCoef.col = "black")
corrplot(mcor_unk, method = "shade", shade.col = NA, tl.col = "black")
cor(unknown_data_pp$Quote_Date, unknown_data_pp$Sequential_Month)
unknown_data_pp$Quote_Date <- NULL #redundant 0.999375

cor(unknown_data_pp$Field_info1, unknown_data_pp$Geographic_info5)
unknown_data_pp$Geographic_info5 <- NULL #redundant 0.975

mcor_unkdata <- as.data.frame(mcor_unk)

unknown_data_pp$Sales_info5 <- NULL # sales5 not relevant to quote_flag (training_data)

#appending quote id back to data
unknown_data_pp <- cbind(unknown_data_pp, unknown_data$Quote_Id)

#writing unknown and training data out 
write.csv(unknown_data_pp, file = "unknown_data_r_pp.csv")
write.csv(training_data, file= "training_data_r_pp.csv")

#########################################################
#second stage of pre-processing 
#proportions of quote flag
prop.table(table(training_data$alpha_Quote_Flag))

#process for oversample
ovun_data <- ovun.sample(alpha_Quote_Flag ~., data = training_data, 
                         method = "both", 
                         p = 0.5,
                         N = 62579, 
                         seed = 1)$data

write.csv(ovun_data, file = "ovun_data.csv")
#####################################################
