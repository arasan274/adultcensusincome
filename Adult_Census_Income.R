
#Adult_Census_Income
#author: Arantxa Sanchis
#date: 21/06/2020

#We are using the publicly available "Adult Census Income" dataset from the UCI Machine 
#Learning Repository on "kaggle" as below:
  
#https://www.kaggle.com/uciml/adult-census-income  

#We have uploaded this dataset to "Google Drive" and made it "publicly available to all".
#We will download it from "Google Drive" into a local folder on our system using the code below.
#We can load the dataset into R as below:
  
#install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(googledrive)) install.packages("googledrive", repos = "http://cran.us.r-project.org")
if(!require(httpuv)) install.packages("httpuv", repos = "http://cran.us.r-project.org")

#Deauthorize i.e do not request for any login credentials
drive_deauth()
drive_user()

#Download dataset from Google drive
downloaded_file_aci <- drive_download(as_id("1ic_BT3GVKn_pQy0f81ITQKPiEAV7QMVW"), overwrite = TRUE)
google_file_aci <- downloaded_file_aci$local_path

#Read dataset into RStudio
adult_census_income_dataset<-read.csv(google_file_aci)

#We can see the number of rows and columns in the dataset as below:
dim(adult_census_income_dataset)


#We will first inspect the dataset for missing values. 
#We notice these have been represented by "?" in the observations.
missing_check<- filter(adult_census_income_dataset, 
                       workclass == "?"|occupation == "?"|native.country == "?")

nrow(missing_check)
head(missing_check)

#A total of 2,399 rows have missing values in the dataset. We will exclude them as below:
adult_census_income_dataset <- filter(adult_census_income_dataset,
                                      !workclass == "?",!occupation == "?",!native.country == "?")
adult_census_income_dataset <- droplevels(adult_census_income_dataset)

#We can get a glance of the dimensions and the first six rows of the dataset as below:
dim(adult_census_income_dataset)
head(adult_census_income_dataset)

#The data now contains 30,162 rows and 15 columns. We would need to carry out a check that 
#the dataset is complete in all aspects using the "summary" function as below.
summary(adult_census_income_dataset)

#We will also use the "str" function to view the class of the objects as below. 
str(adult_census_income_dataset)


#Studying the variables

#1) Age
#In order to have a more rational view of the population, we can segregate the age into 5 main groups - 
  
#a) (0-14yrs) Children
#b) (15-24yrs) Early Working Age
#c) (25-54yrs) Prime Working Age
#d) (55-64yrs) Mature Working Age
#e) (65yrs and above) Elderly

#We can group the age as below:
adult_census_income_dataset_age_group <- adult_census_income_dataset %>%
  mutate(age_group=case_when(
    age >=0 & age <= 14 ~ "(0-14yrs) \n Children",
    age >=15 & age <= 24 ~ "(15-24yrs) \n Early Working Age",
    age >=25 & age <= 54 ~ "(25-54yrs) \n Prime Working Age",
    age >=55 & age <= 64 ~ "(55-64yrs) \n Mature Working Age",
    age >=65 ~ "(65yrs and above) \n Elderly"
  ))

adult_census_income_dataset_age_group %>% 
  ggplot(aes(x=age_group,color=income,fill=income)) + 
  geom_bar(color="black", position = "dodge",size=0.6) + 
  ggtitle("A Distribution of Age Groups") + scale_fill_manual(values = c("purple","green")) +
  theme_gray()

#The percentage breakdown is as below:
adult_census_income_dataset_age_group %>% 
group_by(age_group) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_age=n/sum(n)*100)

#We can see the age boxplot
adult_census_income_dataset %>% 
ggplot(aes(income,age)) + geom_boxplot(color="black",fill="cyan") + 
ggtitle("A Boxplot of Age & Income Distribution")


#2) Working Class

#working_class_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=workclass,color=income,fill =income)) + 
  geom_bar(color="black", position = "dodge",size=0.6) + ggtitle("A Distribution of Work Class") + 
  scale_fill_manual(values = c("purple","green")) + theme_gray() + 
  theme(axis.text.x = element_text(angle = 25))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(workclass) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_workclass=n/sum(n)*100)

#3) fnlwgt

#fnlwgt distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=fnlwgt,color=income,fill =income)) + geom_histogram(color="black",size=0.6, bins=30) + 
  ggtitle("Fnlwgt") + scale_fill_manual(values = c("purple","green")) +  theme_gray() +  
  theme(axis.text.x = element_text(angle = 25)) + scale_x_continuous(trans = 'log10')

#4) Education

#education_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=education,color=income,fill =income)) + 
  geom_bar(color="black", position = "stack",size=0.6) + ggtitle("A Distribution of Education Level") + scale_fill_manual(values = c("purple","green")) +  
  theme_gray()  + theme(axis.text.x = element_text(angle = 90))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(education) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_education=n/sum(n)*100)

#5) Education num

#The education.num is simply a numerical representation of the education attribute. 
#It ranges from 1 to 16 with 1 being the lowest (Preschool) and 16 being the highest (Doctorate).

#6) Marital Status

#marital_status_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=marital.status,color=income,fill =income)) + 
  geom_bar(color="black", position = "dodge",size=0.6) + ggtitle("A Distribution of Marital Status") + 
  scale_fill_manual(values = c("purple","green")) +  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(marital.status) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_marital_status=n/sum(n)*100)

#7) Occupation

#occupation_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=occupation,color=income,fill =income)) + 
  geom_bar(color="black", position = "stack",size=0.6) + ggtitle("A Distribution of Occupation") + 
  scale_fill_manual(values = c("purple","green")) +  theme_gray() +
  theme(axis.text.x = element_text(angle = 90))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(occupation) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_occupation=n/sum(n)*100)

#8) Relationship

#relationship_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=relationship,color=income,fill =income)) + 
  geom_bar(color="black", position = "dodge",size=0.6) + ggtitle("A Distribution of Relationship") + 
  scale_fill_manual(values = c("purple","green")) +  theme_gray() +
  theme(axis.text.x = element_text(angle = 90))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(relationship) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_relationship=n/sum(n)*100)

#9) Race

#race_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=race,color=income,fill =income)) + geom_bar(color="black", position = "dodge",size=0.6) + 
  ggtitle("A Distribution of Race") + scale_fill_manual(values = c("purple","green")) +  theme_gray() +
  theme(axis.text.x = element_text(angle = 90))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(race) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_race=n/sum(n)*100)

#10) Sex

#sex_distribution, echo=FALSE
adult_census_income_dataset %>% 
  ggplot(aes(x=sex,color=income,fill =income)) + geom_bar(color="black", position = "dodge",size=0.6) + 
  ggtitle("A Distribution of Sex") + scale_fill_manual(values = c("purple","green")) +  theme_gray()

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(sex) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_sex=n/sum(n)*100)

#11) Capital gain

#capital_gain_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=capital.gain,color=income,fill =income)) + geom_histogram(color="black",size=0.6, bins=20) + 
  ggtitle("A Distribution of Capital Gains") + scale_fill_manual(values = c("purple","green")) +  theme_gray()

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(capital.gain) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_capital_gain=n/sum(n)*100)

#12) Capital loss

#capital_loss_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=capital.loss,color=income,fill =income)) + geom_histogram(color="black",size=0.6, bins=20) + 
  ggtitle("A Distribution of Capital Losses") + scale_fill_manual(values = c("purple","green")) +  theme_gray()

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(capital.loss) %>% summarise(n=n()) %>% arrange(desc(n)) %>%
  mutate(percent_capital_loss=n/sum(n)*100)

#13) Hours per week

#hours_per_week_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=hours.per.week,color=income,fill =income)) + 
  geom_histogram(color="black",size=0.6, bins=20) + ggtitle("A Distribution of Hours Per Week") +
  scale_fill_manual(values = c("purple","green")) +  theme_gray()

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(hours.per.week) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_hours_per_week=n/sum(n)*100)

#14) Native Country

#native_country_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=native.country,color=income,fill =income)) + 
  geom_bar(color="black", position = "stack",size=0.6) + ggtitle("A Distribution of Native Country") + 
  scale_fill_manual(values = c("purple","green")) +  theme_gray() + 
  theme(axis.text.x = element_text(angle = 90))

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(native.country) %>% summarise(n=n()) %>% arrange(desc(n)) %>% mutate(percent_native_country=n/sum(n)*100)

#15) Income

#income_distribution
adult_census_income_dataset %>% 
  ggplot(aes(x=income,color=income,fill =income)) + geom_bar(color="black",size=0.6) + 
  ggtitle("A Distribution of Income") + scale_fill_manual(values = c("purple","green")) +  theme_gray() 

#The percentage breakdown is as below:
adult_census_income_dataset %>% 
  group_by(income) %>% summarise(n=n()) %>% arrange(desc(n))%>% mutate(percent_income=n/sum(n)*100)

#We will convert the predictor "income" to a factor with 2 levels- less than or equal to $50K and greater than $50K.
adult_census_income_dataset$income <- as.factor(adult_census_income_dataset$income)
class(adult_census_income_dataset$income)

str(adult_census_income_dataset)

#Split the "Adult Census Income" dataset into train and test (validation) sets. 

#NOTE: The validation data will NOT be used for training the algorithm and will ONLY be used for 
#evaluating the accuracy of the final algorithm.

set.seed(1)
test_index <- createDataPartition(adult_census_income_dataset$income, times = 1, p = 0.2, list = FALSE)
adult_census_income_training<- adult_census_income_dataset[-test_index, ]
adult_census_income_validation <- adult_census_income_dataset[test_index, ]

#Split the "Adult Census Income" train dataset into train and test sets to train our algorithms
set.seed(10)
test_index1 <- createDataPartition(adult_census_income_training$income, times = 1, p = 0.2, list = FALSE)
train_set <- adult_census_income_training[-test_index1, ]
test_set <- adult_census_income_training[test_index1, ]


#Models

#I. K Nearest Neighbours (KNN) Model

#Train the knn model on the training data set
set.seed(9)
#Use a 10 fold cross-validation method
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(income ~ ., 
                   method = "knn", 
                   data = train_set, 
                   tuneGrid = data.frame(k = seq(5,41,2)), 
                   trControl = control)

#Plot the k values
ggplot(train_knn, highlight = TRUE)

#Choose the optimal k value
train_knn$bestTune

#Compute the accuracy of the knn model on the test data set
knn_accuracy <- confusionMatrix(predict(train_knn, test_set, type = "raw"), 
                                test_set$income)$overall["Accuracy"]

#Create a results table to store the results for each model
accuracy_results <- bind_rows(data.frame(method = "KNN Model", Accuracy = knn_accuracy))

#View the knn accuracy results in the table
accuracy_results %>% knitr::kable()

#II. Classification and Regression Trees (CART) Model

#Train the CART model using rpart on training set
set.seed(300)
train_rpart <- train(income ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.01, len=100)),
                     data = train_set)

#Highlight the optimized complexity parameter
ggplot(train_rpart, highlight=TRUE)

#Obtain optimal cp
train_rpart$bestTune

#Compute the accuracy of the CART model on the test data set
rpart_accuracy <- confusionMatrix(predict(train_rpart, test_set),
                                  test_set$income)$overall["Accuracy"]

#Store the results of the model
accuracy_results <- bind_rows(accuracy_results,data.frame(method="CART Model", Accuracy = rpart_accuracy))

#View the CART accuracy results in the table
accuracy_results %>% knitr::kable()

#Classification tree figure
plot(train_rpart$finalModel, margin = 0.1)  
text(train_rpart$finalModel, cex = 0.7)

#III. Gradient Boosting Machines (GBM) Model

#Train the gbm model on the training data set

set.seed(2000)
#Use a 10 fold cross-validation method
trCtrl <- trainControl (method = "cv", number = 10)

#Train the gbm model
train_gbm <- train (income~ .,
                    trControl = trCtrl,
                    method = "gbm",
                    preProc="zv",
                    data = train_set,
                    verbose = FALSE)

#Compute the accuracy of the gbm model on the test data set
gbm_accuracy <- confusionMatrix(predict(train_gbm, test_set, type = "raw"),
                                test_set$income) $overall["Accuracy"]
gbm_accuracy

#Store the results of the model
accuracy_results <- bind_rows(accuracy_results,data.frame(method="GBM Model",Accuracy = gbm_accuracy))

#View the gbm accuracy results in the table
accuracy_results %>% knitr:: kable()

#IV. Random Forest (RF) Model

#Train the rf model on the training data set
set.seed(9)
train_rf <- randomForest(income ~ ., data = train_set)

#Compute the accuracy of the rf model on the test dataset
rf_accuracy <- confusionMatrix(predict(train_rf, test_set),
                               test_set$income)$overall["Accuracy"]

#Store the results of the model
accuracy_results <- bind_rows(accuracy_results,data.frame(method="RF Model", Accuracy = rf_accuracy))

#View the rf accuracy results in the table
accuracy_results %>% knitr::kable()

#View the rf importance
importance(train_rf)



#Validation


set.seed(3)
final_train_rf <- randomForest(income ~ ., data = adult_census_income_training)

#Compute the accuracy of the rf model on the validation data set
final_rf_accuracy <- confusionMatrix(predict(final_train_rf, adult_census_income_validation),
                                     adult_census_income_validation$income)$overall["Accuracy"]

#Store the results of the model
accuracy_results_val <- (data.frame(method="Random Forest Model", Accuracy = final_rf_accuracy))

##View the rf accuracy results in the table
accuracy_results_val %>% knitr::kable()

#Results
accuracy_results_val %>% knitr::kable()

#View the rf importance
importance(final_train_rf)