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



View(train) 

glm(answered_correctly ~ content_id, data = train, family = binomial) %>%
  summary()






