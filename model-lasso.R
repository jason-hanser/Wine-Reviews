## Loading Libraries

library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(textstem)
library(glmnet)
library(doParallel)


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

## Unnesting tokens and removing stop words or place holders (i.e. capitilized words)

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


## Filtering out tokens that appear in less than 2.5% (or 5%) of reviews

wine_tokens %>%
  filter(FREQ >= 0.05) -> wine_tokens


  
#####################################################
######## MATRIX CONSTRUCTION - TRAINING DATA ########
#####################################################

## Adding variables to training data

wine_reviews_train[paste0("token_", unique(wine_tokens$token))] <- NA

## Searching wine discription for variables (i.e. tokens)

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



###################################################
######## MODELING THE DATA - TRAINING DATA ########
###################################################

registerDoParallel(4)

temp_variety <- unique(wine_reviews_train$variety)

wine_reviews_train %>%
  select(wine_review,
         variety) -> y_train
  
for (i in temp_variety) {
  
  ## Getting the response variable
  
  y_train %>%
    mutate(variety_ind = ifelse(variety == i, 1, 0)) %>%
    select(variety_ind) %>%
    as.matrix() -> temp_y_train
  
  ## Fitting the model to the training data
  
  cv_fit <- cv.glmnet(x = x_train,
                      y = temp_y_train,
                      family = "binomial",
                      alpha  = 1,
                      nfolds = 10,
                      parallel = TRUE)
  
  ## Getting predictions for training data (for i of temp_variety)
  
  y_train %>%
    mutate(pred = predict(object = cv_fit,
                          newx   = x_train,
                          type   = "link",
                          s      = "lambda.min")) %>%
    mutate(pred = exp(pred)/(1+exp(pred))) %>%
    rename(!!i := pred) -> y_train
  
  ## Cleaning up loop
  
  rm(cv_fit, temp_y_train)
  print(i)
  
}
rm(i, temp_variety)


## Getting predictions for training data (overall predictions)

y_train %>%
  gather("pred", "prob", 3:12) %>%
  group_by(wine_review) %>%
  top_n(n = 1, wt = prob) %>%
  select(wine_review,
         pred) %>%
  left_join(x  = y_train,
            by = "wine_review") -> pred_train




#################################################
######## MATRIX CONSTRUCTION - TEST DATA ########
#################################################

## Adding variables to test data

wine_reviews_test[paste0("token_", unique(wine_tokens$token))] <- NA

## Searching wine discription for variables (i.e. tokens)

wine_reviews_test %>%
  mutate_at(.vars = vars(matches("^token_")),
            .funs = funs(str_extract(deparse(substitute(.)), "(?<=^token_).*"))) -> wine_reviews_test

wine_reviews_test %>%
  mutate_at(.vars = vars(matches("^token_")),
            .funs = funs(ifelse(str_detect(clean_desc, paste0("\\b", ., "\\b")) , 1, 0))) -> wine_reviews_test

## Input matrix construction

wine_reviews_test %>%
  select_at(.vars = vars(matches("^token_"))) %>%
  as.matrix() -> x_test



###############################################
######## MODELING THE DATA - TEST DATA ########
###############################################

temp_variety <- unique(wine_reviews_train$variety)

wine_reviews_train %>%
  select(wine_review,
         variety) -> y_train

wine_reviews_test %>%
  select(wine_review,
         variety) -> y_test


for (i in temp_variety) {
  
  ## Getting the response variable
  
  y_train %>%
    mutate(variety_ind = ifelse(variety == i, 1, 0)) %>%
    select(variety_ind) %>%
    as.matrix() -> temp_y_train
  
  ## Fitting the model to the training data
  
  cv_fit <- cv.glmnet(x = x_train,
                      y = temp_y_train,
                      family = "binomial",
                      alpha  = 1,
                      nfolds = 10,
                      parallel = TRUE)
  
  ## Getting predictions for training data (for i of temp_variety)
  
  y_test %>%
    mutate(pred = predict(object = cv_fit,
                          newx   = x_test,
                          type   = "link",
                          s      = "lambda.min")) %>%
    mutate(pred = exp(pred)/(1+exp(pred))) %>%
    rename(!!i := pred) -> y_test
  
  ## Cleaning up loop
  
  rm(cv_fit, temp_y_train)
  print(i)
  
}
rm(i, temp_variety)


## Getting predictions for training data (overall predictions)

y_test %>%
  gather("pred", "prob", 3:12) %>%
  group_by(wine_review) %>%
  top_n(n = 1, wt = prob) %>%
  select(wine_review,
         pred) %>%
  left_join(x  = y_test,
            by = "wine_review") -> pred_test



######################################
######## WRITING DATA TO FILE ########
######################################

wine_reviews_train %>%
  select(wine_review,
         variety) %>%
  cbind(x_train) %>%
  write.csv("data//output//lasso//5 percent//x_train.csv", row.names = FALSE)

wine_reviews_test %>%
  select(wine_review,
         variety) %>%
  cbind(x_test) %>%
  write.csv("data//output//lasso//5 percent//x_test.csv", row.names = FALSE)


write.csv(pred_train, "data//output//lasso//5 percent//pred_train.csv", row.names = FALSE)
write.csv(pred_test, "data//output//lasso//5 percent//pred_test.csv", row.names = FALSE)


