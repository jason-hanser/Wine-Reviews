
## Loading Libraries

library(dplyr)
library(tidyr)
library(stringr)


##############################
######## LOADING DATA ########
##############################

wine_reviews <- read.csv("data//winemag-data-130k-v2.csv",
                         stringsAsFactors = FALSE,
                         encoding = "UTF-8")


################################
######## FILTERING DATA ########
################################

## Getting the number of reviews by varierty

wine_reviews %>%
  group_by(variety) %>%
  summarise(CT= n()) %>%
  arrange(desc(CT)) -> wine_varieties


## Getting wine reviews for the ten most common varieties of wine

wine_varieties %>%
  filter(str_detect(tolower(variety), "blend") == FALSE) %>%
  filter(row_number() <= 10) %>%
  select(variety) %>%
  inner_join(x  = wine_reviews,
             by = "variety") -> wine_reviews


## getting only the columns I need

wine_reviews %>%
  transmute(wine_review = row_number(),
            name        = title,
            winery      = winery,
            country     = country,
            province    = province,
            variety     = variety,
            orig_desc   = description,
            clean_desc  = tolower(description)) -> wine_reviews


##########################################
######## PRE-PROCESSING TEXT DATA ########
##########################################

## Removing the variety and/or variety abbreviations from the wine descriptions

temp_abbr <- c("pinot", "cab", "sb", "bordeaux", "blend", "chard", "sauv blanc", "cabernet")

wine_reviews %>%
  mutate(clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = paste0("\\b", tolower(variety), "\\b"),
                                      replacement = "VARIETY"),
         clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = paste0("\\b", temp_abbr,"s?\\b", collapse = "|"),
                                      replacement = "VARIETY")) -> wine_reviews


## Removing the names the winery, country, and province from the wine descriptions

wine_reviews %>%
  mutate(clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = paste0("\\b", tolower(winery), "\\b"),
                                      replacement = "WINERY"),
         clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = paste0("\\b", tolower(country), "\\b"),
                                      replacement = "COUNTRY"),
         clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = paste0("\\b", tolower(province), "\\b"),
                                      replacement = "PROVINCE")) -> wine_reviews
         
           
## Removing certain colors from the wine descriptions

temp_colors <- c("red", "white", "pink", "blush")

wine_reviews %>%
  mutate(clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = paste0("\\b", temp_colors,"\\b", collapse = "|"),
                                      replacement = "COLOR")) -> wine_reviews


## Removing numbers/years

wine_reviews %>%
  mutate(clean_desc = str_replace_all(string      = clean_desc,
                                      pattern     = "\\b\\d+\\b",
                                      replacement = "NUMBER")) -> wine_reviews


## lemmatizing words within the wine descriptions

wine_reviews %>%
  mutate(clean_desc = lemmatize_strings(clean_desc)) -> wine_reviews

rm(wine_varieties, temp_colors, temp_abbr)



######################################
######## WRITING DATA TO FILE ########
######################################

write.csv(wine_reviews, "data//clean_wine_reviews.csv", row.names = FALSE)

