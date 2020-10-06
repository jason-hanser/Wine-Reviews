## Loading Libraries

library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(textstem)
library(e1071)


##############################
######## LOADING DATA ########
##############################

## Loading the cleaned, pre-processed wine reviews

wine_reviews <- read.csv("data//clean_wine_reviews.csv",
                         stringsAsFactors = FALSE)


###########################
######## DATA PREP ########
###########################

## spliting the wine_reviews into a training and testing data set

set.seed(6170)

wine_reviews_train <- wine_reviews[sample(nrow(wine_reviews)*0.8), ]
wine_reviews_test  <- wine_reviews[-wine_reviews_train$wine_review, ]

rm(wine_reviews)


####################################
######## VARIABLE SELECTION ########
####################################

## Un-nesting tokens and removing stop words or place holders (i.e. capitalized words)

wine_reviews_train %>%
  unnest_tokens(input    = clean_desc,
                output   = "token",
                to_lower = FALSE) %>%
  anti_join(y  = stop_words,
            by = c("token" = "word")) %>%
  filter(str_detect(token, "[:upper:]") == FALSE) -> wine_tokens


## Calculating the frequency of each token for each variety

wine_tokens %>%
  group_by(variety) %>%
  mutate(VARIETY_CT = n_distinct(wine_review)) %>%
  group_by(variety,
           token,
           VARIETY_CT) %>%
  summarise(TOKEN_CT   = n()) %>%
  ungroup() %>%
  mutate(FREQ = TOKEN_CT/VARIETY_CT) -> wine_tokens


## Filtering out tokens that appear in less than 5% of reviews

wine_tokens %>%
  filter(FREQ >= 0.05) -> wine_tokens



#####################################################
######## MATRIX CONSTRUCTION - TRAINING DATA ########
#####################################################

## Adding variables to training data

wine_reviews_train[paste0("token_", unique(wine_tokens$token))] <- NA

## Searching wine description for variables (i.e. tokens)

wine_reviews_train %>%
  mutate_at(.vars = vars(matches("^token_")),
            .funs = funs(str_extract(deparse(substitute(.)), "(?<=^token_).*"))) -> wine_reviews_train

wine_reviews_train %>%
  mutate_at(.vars = vars(matches("^token_")),
            .funs = funs(ifelse(str_detect(clean_desc, paste0("\\b", ., "\\b")) , 1, 0))) -> wine_reviews_train

## input matrix construction

wine_reviews_train %>%
  select_at(.vars = vars(matches("^token_"))) %>%
  as.matrix() -> x_train



#################################################
######## MATRIX CONSTRUCTION - TEST DATA ########
#################################################

## Adding variables to test data

wine_reviews_test[paste0("token_", unique(wine_tokens$token))] <- NA

## Searching wine description for variables (i.e. tokens)

wine_reviews_test %>%
  mutate_at(.vars = vars(matches("^token_")),
            .funs = funs(str_extract(deparse(substitute(.)), "(?<=^token_).*"))) -> wine_reviews_test

wine_reviews_test %>%
  mutate_at(.vars = vars(matches("^token_")),
            .funs = funs(ifelse(str_detect(clean_desc, paste0("\\b", ., "\\b")) , 1, 0))) -> wine_reviews_test

## input matrix construction

wine_reviews_test %>%
  select_at(.vars = vars(matches("^token_"))) %>%
  as.matrix() -> x_test



###################################
######## MODELING THE DATA ########
###################################

## Fitting the model to the training data

svm_fit <- tune.svm(x = x_train,
                    y = as.factor(wine_reviews_train$variety),
                    type   = "C-classification",
                    scale  = FALSE,
                    cost   = c(0.01, 0.1, 1, 10, 100),
                    gamma  = c(0.01, 0.05, 0.1, 0.5, 1),
                    tunecontrol = tune.control(cross = 5))


## Getting predictions for training data

wine_reviews_train %>%
  select(wine_review,
         variety) %>%
  mutate(pred = predict(object  = svm_fit$best.model,
                        newdata = x_train)) -> y_train


## Getting predictions for test data

wine_reviews_test %>%
  select(wine_review,
         variety) %>%
  mutate(pred = predict(object  = svm_fit$best.model,
                        newdata = x_test)) -> y_test




######################################
######## WRITING DATA TO FILE ########
######################################

## saving data files

wine_reviews_train %>%
  select(wine_review,
         variety) %>%
  cbind(x_train) %>%
  write.csv("data//output//svm//5 percent//x_train.csv", row.names = FALSE)

wine_reviews_test %>%
  select(wine_review,
         variety) %>%
  cbind(x_test) %>%
  write.csv("data//output//svm//5 percent//x_test.csv", row.names = FALSE)

write.csv(y_train, "data//output//svm//5 percent//pred_train.csv", row.names = FALSE)
write.csv(y_test, "data//output//svm//5 percent//pred_test.csv", row.names = FALSE)

## Saving model to file

saveRDS(svm_fit, "data//output//svm//5 percent//svm_model.rds")



