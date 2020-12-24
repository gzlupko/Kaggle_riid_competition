#  riid EDA and prediction generations 


library(dplyr) 
library(readr) 
library(ggplot2) 
library(caret)
library(RColorBrewer)

example_submission <- read_csv("example_sample_submission.csv") 
View(example_submission)

riid_train <- read.csv("train.csv", nrows = 10000) 
train <- na.omit(riid_train) 


# data splits 


set.seed(401) 


index_training <- createDataPartition(train$row_id, p = 0.5, list = FALSE) 
training_set <- train[index_training, ]
testing_set <- train[-index_training, ]


# EDA 
training_set %>%
  group_by(answered_correctly) %>%
  count() %>%
  ggplot(aes(x = answered_correctly, y = n, fill = answered_correctly)) + 
  geom_bar(stat = "identity") 

training_set %>%
  group_by(task_container_id) %>%
  count() %>%
  arrange(desc(n)) 

training_set %>%
  group_by(content_id) %>%
  count() %>%
  arrange(desc(n)) 

training_set %>%
  group_by(user_id) %>%
  count() %>%
  arrange(desc(n))

training_set %>%
  group_by(user_id) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(avg_user_count = sum(n)/4872)


# logistic regression 

multi_log <- glm(answered_correctly ~ content_id + task_container_id
                 + prior_question_elapsed_time,
                 data = train, family = binomial)


training_prediction <- predict(multi_log, 
                               newdata = training_set, type = "response") 

hist(training_prediction)

testing_prediction <- predict(multi_log, 
                              newdata = testing_set, type = "response") 



prediction_cutoff <- ifelse(testing_prediction > 0.5, 1, 0)
table(prediction_cutoff, testing_set$answered_correctly)

testing_prediction <- as.numeric(testing_prediction) 

confusionMatrix(table(testing_set$answered_correctly, 
                      prediction_cutoff))




