##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Begin Project

# This is used to calculate the rmse of the predictions
RMSE <- function(x,y){
  SE <- (x - y) ^ 2
  result <- mean(SE) ^ (1/2)
  return(result)
}
# This is used to restrict the predictions to within the minimum and maximum ratings that are possible
# in the data set and slightly reduce errors, since the linear model developed can yield ratings of greater
# than 5 and less than 0.5.
within_bounds <- function(x) {
  if(x > 5){
    result <- 5
  } else if (x < 0.5) {
    result <- 0.5
  } else {
    result <- x
  }
  return(result)
}

# calculate the average rating of the training dataset
mu <- mean(edx$rating)

# calculate the movie bias (b_m)
b_m <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))

# calculate the user bias (b_u)
b_u <- edx %>%
  left_join(b_m, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))

#calculate the genre bias (b_g)
b_g <- edx %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - b_m - b_u - mu))

# use the bias effects calculated above to generate predicted ratings on the training set.  Note the use of the 
# within_bounds function to restrict values to the range 0.5 to 5.0
train_prediction <- edx %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = sapply(mu + b_m + b_u + b_g, within_bounds)) %>%
  .$pred

#FUN WITH LAMBDA
lambdas <- seq(0,10,1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_m <- edx %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu) / (n() + l))
  b_u <- edx %>%
    group_by(userId) %>%
    left_join(b_m, by = "movieId") %>%
    summarize(b_u = sum(rating - mu - b_m) / (n() + l))
  b_g <- edx %>%
    group_by(genres) %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    summarize(b_g = sum(rating - mu - b_m - b_u) / (n() + l))
  prediction <- edx %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_m + b_u + b_g) %>%
    .$pred
  return(RMSE(edx$rating, prediction))
})

plot(lambdas, rmses)
# evaluate the RMSE for the training set predictions
RMSE(edx$rating, train_prediction)

# The following three steps examine the prevalence of specific movies, users, and genres in the dataset to estimate
# whether or not reularization of the bias effects will help improve the model
edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  arrange(count)

edx %>%
  group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(count)

edx %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(count) %>%
  head()

# determine the final RMSE on the validation set using the model generated above
validation_prediction <- validation %>%
  left_join(b_m, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = sapply(mu + b_m + b_u + b_g, within_bounds)) %>%
  .$pred

RMSE(validation$rating, validation_prediction)











