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
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

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

#for convenience and to save time in the future, save edx and validation as an rda
save(edx, file="rdas/edx.rda")
save(validation, file="rdas/validation.rda")

#################
# Answers to Quiz
#################

load("rdas/edx.rda")
load("rdas/validation.rda")

# See size of edx data frame
dim(edx)

# handy to see structure and column names
head(edx)

# Number of entries with a 0 rating
sum(edx$rating==0)

# Number of entries with a 3 rating
sum(edx$rating==3)

# Count of distinct (unique) movies
length(unique(edx$movieId))

# Count of distinct users
length(unique(edx$userId))

# create genres and run sapply with th str_detect function to calculate the number of entries containing each genre
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# Top 10 movies (titles) considering the number of ratings
edx%>%group_by(title)%>%summarize(count=n())%>%arrange(desc(count))%>%head(10)

# Sort ratings from most popular to least. It also clearly shows that integer ratings are more frequent.
edx%>%group_by(rating)%>%summarize(count=n())%>%arrange(desc(count))

##############
#Data Analysis
##############

options(digits=5)

## Predict based on training set mean
m<-mean(edx$rating)
m

# useful to create a Root Mean Square Error funtion
RMSE<-function(true,predicted){
  sqrt(mean((true-predicted)^2))
}


# RMSE if we use the mean value to predict
rmse_mean<-RMSE(validation$rating,m)
rmse_mean

#create a dataframe with the results
results <- data_frame(method = "Mean", RMSE = as.numeric(rmse_mean))

## Modeling effects per movie

movie<-edx%>%
  group_by(movieId)%>%
  summarise(b_mov=mean(rating-m))

predicted_ratings <- validation %>%
  left_join(movie, by='movieId') %>%
  mutate(pred=m+b_mov)%>%
  pull(pred)

# RMSE considering the effect of movies
rmse_movie<-RMSE(validation$rating,predicted_ratings)
rmse_movie

# adding the RMSE results, we see an improvement in the RSME
results<-results%>%
  add_row(method="Movie effect",RMSE=as.numeric(rmse_movie))


## Modeling effects per user

user<- edx %>%
  left_join(movie, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_user = mean(rating - m - b_mov))

predicted_ratings <- validation %>%
  left_join(movie, by='movieId') %>%
  left_join(user, by="userId")%>%
  mutate(pred=m+b_mov+b_user)%>%
  pull(pred)

# RMSE considering the effect of users
rmse_user<-RMSE(validation$rating,predicted_ratings)
rmse_user

# adding the RMSE results, we see an improvement in the RSME
results<-results%>%
  add_row(method="User effect",RMSE=as.numeric(rmse_user))

