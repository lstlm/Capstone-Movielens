##############################################################################
# TERRY MCLAUGHLIN - CAPSTONE MOVIELENS PROJECT
##############################################################################


#Script below provided by course materials
#R Script used for Capstone Movielens
################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,] #train
temp <- movielens[test_index,] #test


# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
############################# END OF SCRIPT PROVIDED BY COURSE MATERIALS #######################

##########################################
# Load libraries needed for project
##########################################
library(dslabs)
library(tidyverse)
library(lubridate)
library(caret)
library(dplyr)
library(kableExtra)
library(formattable)
library(ggrepel)


############################################################################
# Prepare the edx and validation datasets to add columns needed for project
############################################################################

#Add columns for year rated, release year, and number of years between for both EDX and Validations sets
edxdata <-mutate(edx,  year_rated = year(as_datetime(timestamp)), year_released = as.numeric(str_sub(title,-5,-2)), year_between = year_rated - year_released) 
validationdata <- mutate(validation, year_rated = year(as_datetime(timestamp)), year_released = as.numeric(str_sub(title,-5,-2)), year_between = year_rated - year_released)

#--------------------DATASET ANALYSIS-------------------------------------------------

############################################################################
# General Statistics
############################################################################

#Dataset dimensions
dim(movielens)
dim(edxdata)
dim(validationdata)

#MEAN AND MEDIAN AVERAGE FOR EDX DATA SET
mean(edxdata$rating)
median(edxdata$rating)

#MEAN AND MEDIAN AVERAGE FOR VALIDATION DATA SET
mean(validationdata$rating)
median(validationdata$rating)

### EDX dataset header provided by initial code course provided
formattable(head(edx))
formattable(head(edxdata))

#A BIG UGH - FORMATTABLE DOESN'T WORK WELL WITH RMARKDOWN LATEX - HAD TO SWITCH TO KABLE

############################################################################
# Summarize the edx dataset
############################################################################

#edx dataset
edx_summary <- edxdata %>%
  summarize(No_Users = n_distinct(userId),
            No_Movies = n_distinct(movieId), 
            No_Genres = n_distinct(genres),
            Distinct_Between_Years = n_distinct(year_between),            
            Median_Rating = median(rating),
            Average_Rating = mean(rating),
            earliestYearReleased = min(year_released), 
            earliestYearRated = min(year_rated),            
            latestYearReleased = max(year_released), 
            latestYearRated = max(year_rated))

############################################################################
# Summarize the validation dataset
############################################################################

val_summary <- validationdata %>%
  summarize(No_Users = n_distinct(userId),
            No_Movies = n_distinct(movieId), 
            No_Genres = n_distinct(genres),
            Distinct_Between_Years = n_distinct(year_between), 
            Median_Rating = median(rating),
            Average_Rating = mean(rating),
            earliestYearReleased = min(year_released), 
            earliestYearRated = min(year_rated),            
            latestYearReleased = max(year_released), 
            latestYearRated = max(year_rated))
############################################################################
# Put it together
############################################################################

summarystats <- bind_rows(edx_summary, val_summary)
#add as.character to control decimal points displayed
charsummary <- data.frame(No_Users = as.character(summarystats$No_Users), 
                          No_Movies = as.character(summarystats$No_Movies), 
                          No_Genres= as.character(summarystats$No_Genres), 
                          Earliest_Movie_Release_Year = as.character(summarystats$earliestYearReleased),
                          Earliest_Movie_Rated_Year= as.character(summarystats$earliestYearRated),
                          Latest_Movie_Release_Year = as.character(summarystats$latestYearReleased),
                          Latest_Movie_Rated_Year= as.character(summarystats$latestYearRated),
                          Year_Between = as.character(summarystats$Distinct_Between_Years),
                          Median_Rating = as.character(summarystats$Median_Rating), 
                          Average_Rating = summarystats$Average_Rating)

#transpose data for better readablilty
transposeSummary <- data.frame(t(charsummary))

############################################################################
# Top and bottom 10 with over 1000 ratings
############################################################################
#Top 10 by rating (over 1000 ratings)
top_10 <- edxdata %>% group_by(title, movieId) %>%
  summarize(count = n(), average_rating = mean(rating)) %>% 
  filter(count > 1000) %>%
  ungroup(title, movieId) %>%
  top_n(10, average_rating) %>%
  arrange(desc(average_rating))

#Bottom 10 by rating (over 1000 rating)
bottom_10 <- edxdata %>% group_by(title,movieId) %>%
  summarize(count = n(), average_rating = mean(rating)) %>% 
  filter(count > 1000) %>%
  ungroup(title, movieId) %>%
  top_n(-10, average_rating) %>%
  arrange((average_rating))  

############################################################################
# Ratings by Genre
############################################################################

separate_genres <- edxdata %>% separate_rows(genres, sep ="\\|")

# filter out no genre (#ratings = 7)
number_of_genres <- separate_genres %>% group_by(genres) %>% 
  summarize(TotalRatings = n(), AverageRating = mean(rating)) %>%
  filter (TotalRatings >10) %>%
  arrange(desc(AverageRating))

number_of_genres

# filter out no genre (#ratings = 7)
totalnumber_of_genres <- separate_genres %>% group_by(genres) %>% 
  summarize(TotalRatings = n(), AverageRating = mean(rating)) %>%
  filter (TotalRatings >10) %>%
  arrange(desc(TotalRatings))

totalnumber_of_genres

############################################################################
# Display Statistic Outputs
############################################################################
#formattable(transposeSummary, align = c("c","c"),col.names = c("EDX Data", "Validation Data"))
#transposeSummary %>% knitr::kable()
#formattable(top_10, align=(c("l","c", "r", "r")))
#formattable(bottom_10, align=(c("l","c", "r", "r")))

#Display Summary
kable(transposeSummary, align = c("c","c"), 
      col.names = c("EDX Data", "Validation Data")) %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      kable_styling(latex_options = "striped")

#Display Top 10
kable(top_10, align=(c("l","c", "r", "r")), col.names = c("Title", "Movie ID", "Count", "Average Rating")) %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  kable_styling(latex_options = "striped")

#Display Bottom 10
kable(bottom_10, align=(c("l","c", "r", "r")), col.names = c("Title", "Movie ID", "Count", "Average Rating")) %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  kable_styling(latex_options = "striped")

#Display Genre by Average Rating
kable(number_of_genres, align=(c("l","r", "r")), col.names = c("Genre", "# Ratings", "Average Rating")) %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  kable_styling(latex_options = "striped")

#Display Genre by # Ratings
kable(totalnumber_of_genres, align=(c("l","r", "r")), col.names = c("Genre", "# Ratings", "Average Rating")) %>%
  row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
  kable_styling(latex_options = "striped")

#--------------------VISUALIZING DATA -------------------------------------------------
############################################################################
# Display Charts in Report
############################################################################
# Simple Historgram Average Rating
edx %>% 
  group_by(movieId) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 10, color = "gray77", fill = "lightsteelblue4") +
  geom_vline(xintercept=3.51, linetype="dashed", color = "coral",size=1) +
  ggtitle("Average Movie Rating") +
  xlab("Average Rating") +
  ylab("Number of Ratings") 

#Number of user ratings by release per year 
edxdata %>%
  group_by(year_released)  %>%
  summarize(Total=n())  %>%
  ggplot(aes(x=year_released, y=Total/1000)) +
  geom_line(color = "blue4", size=1) +
  ggtitle("Total Number of Ratings by Movie Releases by Year (in thousands)") + 
  xlab("Release Year")+
  ylab("Number of Ratings")  

#Average rating by year released 
edxdata %>%
  group_by(year_released) %>%
  summarize(AverageRating = mean(rating)) %>%
  ggplot(aes(year_released, AverageRating)) +
  geom_point(color = "brown4") +
  geom_smooth() + 
  ggtitle("Average Rating by Release Year") +
  xlab("Year Released") +
  ylab("Average Rating")

#Average rating by year rated 
edxdata %>%
  group_by(year_rated) %>%
  summarize(AverageRating = mean(rating)) %>%
  ggplot(aes(year_rated, AverageRating)) +
  geom_point(color = "brown4") +
  geom_smooth() + 
  ggtitle("Average Rating by Year Rated") +
  xlab("Year Rated") +
  ylab("Average Rating")

#Both Together
ggplot() + 
  geom_point(data = edxdata %>% 
               group_by(year_released) %>% 
               summarize(Average_Rating = mean(rating), number=n()),
             aes(year_released, Average_Rating, col="year_released", size=number)) + 
  geom_point(data = edxdata %>% group_by(year_rated) %>% 
               summarize(Average_Rating = mean(rating), number=n()),
             aes(year_rated, Average_Rating, col="year_rated", size=number)) + 
  ggtitle("Mean Rating per Release Year and Year Rated")  +
  xlab("Year") +
  ylab("Rating") + 
  guides(size = FALSE)


############################################################################
# Display Genre Charts in Report
############################################################################
#Average Rating Genre with rating axis adjusted
number_of_genres %>% 
  ggplot(aes(x= reorder(genres, AverageRating), y = AverageRating)) + 
  geom_bar(stat = "identity", color = "#aeb6bf", fill = "#34495e") +
  ggtitle("Average Rating per Genre - rating limits adjusted") + 
  xlab("Genre")+
  ylab("Rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip(ylim = c(3,4))

#Total Ratings per Genre Bar Chart
number_of_genres %>% 
  ggplot(aes(x= reorder(genres, TotalRatings), y = TotalRatings/1000)) + 
  geom_bar(stat = "identity", color = "#aeb6bf", fill = "#34495e") +
  ggtitle("Total Rating per Genre (in thousands)") + 
  xlab("Genre")+
  ylab("Rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

#Mean Ratings by Genre 
number_of_genres %>% 
  ggplot(aes(x= genres, y = AverageRating )) + 
  geom_point(aes(size=TotalRatings/1000), color = "turquoise4") +
  ggtitle("Average Rating per Genre") + 
  xlab("Genre")+
  ylab("Rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(size=FALSE)


#--------------------end of VISUALIZING DATA -------------------------------------------------


#-----------------------------------RESULTS  SECTION  ----------------------------------------------
############################################################################
# MODEL 1 - AVERAGE ONLY
############################################################################

#-------------------------AVERAGE ONLY ----------------------------------------------
# First lets just get the average of the ratings, not taking anything else into account
#     Most basic of all the analysis
rmse_results <- ''
options(digits=7)
mu_hat <- mean(edxdata$rating)
RMSE <- RMSE(validationdata$rating, mu_hat)

rmse_results <- tibble(method = "Model 1 - Average only", RMSE)

#display results
kable(rmse_results, 
      align = c("l","c"), 
      format="latex",
      col.names = c("Method", "RMSE")) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 1, color = "#fbfcfc", background = "#7b241C")

#---------------------end of AVERAGE ONLY -------------------------------------------


############################################################################
# MODEL 2 - MOVIE EFFECT
############################################################################

mu <- mean(edxdata$rating) 
movie_avgs <- edxdata %>%   
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validationdata   %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

#-------------------------MOVIE EFFECT HISTOGRAM----------------------------------------
# Movie Effect Rating
movie_avgs%>% 
  ggplot(aes(x=b_i, y = ..density..)) + 
  geom_histogram(bins = 10, color = "gray", fill = "tomato4") +
  geom_density(method = "loess", size = 2, color = "darkblue") +
  geom_vline(xintercept=0, linetype="dashed", color = "goldenrod1",size=1) +
  ggtitle("Movie Effect Rating") +
  xlab("Average Rating") 


#-------------------------MOVIE EFFECT RMSE ---------------------------------------------
model_m_rmse <- RMSE(validationdata$rating, predicted_ratings)

#add rows to a running table to summarize all model results
rmse_results <- bind_rows(rmse_results,
                tibble(method="Model 2 - Movie Effect Model",RMSE = model_m_rmse))
                
#display results
kable(rmse_results, 
      align = c("l","c"), 
      format="latex",
      col.names = c("Method", "RMSE")) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 2, color = "#fbfcfc", background = "#7b241C")
#---------------------end of MOVIE EFFECT -------------------------------------------



############################################################################
# MODEL 3 - MOVIE + TIME EFFECT
############################################################################
#-------------------------MOVIE + USER + TIME EFFECT --------------------------------------

year_between_avgs <- edxdata %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(year_between) %>%
  summarize(b_t = mean(rating - mu - b_i))

predicted_ratings <- validationdata %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(year_between_avgs, by='year_between') %>%
  mutate(pred = mu + b_i + b_t) %>%
  pull(pred)

#-----------------------MOVIE + TIME EFFECT HISTOGRAM--------------------------------
# Movie Effect Rating
year_between_avgs %>% 
  ggplot(aes(x=b_t, y = ..density..)) + 
  geom_histogram(bins = 10, color = "gray", fill = "tomato4") +
  geom_vline(xintercept=0, linetype="dashed", color = "red",size=1) +
  geom_density(method = "loess", size = 2, color = "darkblue") +
  ggtitle("Movie + Time Rating") +
  xlab("Average Rating") 


#-----------------------MOVIE + TIME EFFECT RMSE--------------------------------
model_my_rmse <- RMSE(predicted_ratings, validationdata$rating)

#add rows to a running table to summarize all model results
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Model 3 - Movie + Time Effect Model",
                                 RMSE = model_my_rmse ))

#display results
kable(rmse_results, 
      align = c("l","c"), 
      format="latex",
      col.names = c("Method", "RMSE")) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 3, color = "#fbfcfc", background = "#7b241C")

#------------------------END MOVIE + TIME EFFECT --------------------------------------



############################################################################
# MODEL 4 - MOVIE + USER EFFECT
############################################################################
#-------------------------MOVIE + USER EFFECT --------------------------------------
#Now the Movie + user effect
user_avgs <- edxdata %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validationdata %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#-------------------------MOVIE + USER EFFECT HISTOGRAM----------------------------------------
# Movie Effect Rating
user_avgs%>% 
  ggplot(aes(x=b_u, y = ..density..)) + 
  geom_histogram(bins = 10, color = "gray", fill = "tomato4") +
  geom_vline(xintercept=0, linetype="dashed", color = "red",size=1) +
  geom_density(method = "loess", size = 2, color = "darkblue") +
  ggtitle("Movie + User Effect Rating") +
  xlab("Average Rating") 

#-------------------------MOVIE + USER EFFECT RMSE ---------------------------------------------

model_mu_rmse <- RMSE(validationdata$rating, predicted_ratings)

#add rows to a running table to summarize all model results
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Model 4 - Movie + User Effect Model",
                                 RMSE = model_mu_rmse ))

#display results
kable(rmse_results, 
      align = c("l","c"), 
      format="latex",
      col.names = c("Method", "RMSE")) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 4, color = "#fbfcfc", background = "#7b241C")

#---------------------end of MOVIE + USER EFFECT -------------------------------------------



############################################################################
# MODEL 5 - REGULARIZATION MOVIE EFFECT
############################################################################
#-------------------------REGULARIZATION MOVIE EFFECT --------------------------------------
#Choose lambda with cross-validation
lambdas <- seq(0, 10, 0.25)
class(lambdas)
head(lambdas)
head(rmses)
mu <- mean(edxdata$rating)

just_the_sum <- edxdata %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validationdata %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validationdata$rating))
})

#---------------------------Plot the lambdas-------------------------------------------------------
best_Lambda <- lambdas[which.min(rmses)]
chartData <- as.data.frame(cbind(rmses, lambdas))

chartData %>% ggplot(aes(lambdas, rmses)) + 
  geom_point() +
  geom_vline(xintercept=best_Lambda, linetype="dashed", color = "red",size=1)+
  ggtitle("Regularization Movie Effect") +
  xlab("Lambda") + 
  ylab("RMSE")  
#-------------------------REGULARIZATION MOVIE EFFECT RMSE -----------------------------------------
best_lambda
RMSE_RM = min(rmses)
RMSE_RM

#add rows to a running table to summarize all model results
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Model 5 - Regularized Movie Effect Model",
                                 RMSE= RMSE_RM, Lambda = best_Lambda))

#-------------------------REGULARIZATION MOVIE EFFECT RESULTS -----------------------------------------
#display results
kable(rmse_results, 
      align = c("l","c", "r"), 
      format="latex",
      col.names = c("Method", "RMSE", "Lambda" )) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 5, color = "#fbfcfc", background = "#7b241C")

#---------------------end of REGULARIZATION MOVIE EFFECT  -------------------------------------------



############################################################################
# MODEL 6 - REGULARIZATION MOVIE + USER EFFECT
############################################################################
#-------------------------REGULARIZATION MOVIE + USER EFFECT --------------------------------------
#Choose lambda with cross-validation
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edxdata %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edxdata %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validationdata %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE( predicted_ratings, validationdata$rating))
})

#---------------------------Plot the lambdas-------------------------------------------------------
best_Lambda <- lambdas[which.min(rmses)]
chartData <- as.data.frame(cbind(rmses, lambdas))

chartData %>% ggplot(aes(lambdas, rmses)) + 
  geom_point() +
  geom_vline(xintercept=best_Lambda, linetype="dashed", color = "red",size=1)+
  ggtitle("Regularization Movie + User Effect") +
  xlab("Lambda") + 
  ylab("RMSE")  
#-------------------------REGULARIZATION MOVIE + USER EFFECT RMSE -----------------------------------------
best_Lambda
RMSE_RMU = min(rmses)
RMSE_RMU

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Model 6 - Regularized Movie + User Effect Model",
                                 RMSE= RMSE_RMU, Lambda = best_Lambda))


#-------------------------REGULARIZATION MOVIE + User EFFECT RESULTS -----------------------------------------
#display results
kable(rmse_results, 
      align = c("l","c", "r"), 
      format="latex",
      col.names = c("Method", "RMSE", "Lambda" )) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 6, color = "#fbfcfc", background = "#7b241C")

#---------------------end of REGULARIZATION + USER MOVIE EFFECT  -------------------------------------------



############################################################################
# MODEL 7 - REGULARIZATION MOVIE + USER + TIME EFFFECT
############################################################################
#-------------------------REGULARIZATION MOVIE + + TIME EFFFECT ------------------------------------
#Choose lambda with cross-validation
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edxdata$rating)
  
  b_i <- edxdata %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edxdata %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_yb <- edxdata %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by='userId') %>%
    group_by(year_between) %>%
    summarize(b_yb = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    validationdata %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_yb, by = 'year_between') %>%
    mutate(pred = mu + b_i + b_u + b_yb) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validationdata$rating))
})

#---------------------------Plot the lambdas-------------------------------------------------------
best_Lambda <- lambdas[which.min(rmses)]
chartData <- as.data.frame(cbind(rmses, lambdas))

chartData %>% ggplot(aes(lambdas, rmses)) + 
  geom_point() +
  geom_vline(xintercept=best_Lambda, linetype="dashed", color = "red",size=1)+
  ggtitle("Regularization Movie + User + Time Effect") +
  xlab("Lambda") + 
  ylab("RMSE")  



#-------------------------REGULARIZATION MOVIE + + USER + TIME EFFFECT RMSE -----------------------------------------
best_lambda
RMSE_RMUT = min(rmses)
RMSE_RMUT

#add rows to a running table to summarize all model results
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Model 7 - Regularized Movie + User + Time Effect Model",
                                 RMSE= RMSE_RMUT, Lambda = best_Lambda))
#display results
kable(rmse_results, 
      align = c("l","c", "r"), 
      format="latex",
      col.names = c("Method", "RMSE", "Lambda" )) %>%
  kable_styling(latex_options = "striped", position = "left")

kable(rmse_results, 
      align = c("l","c", "r"), 
      format="latex",
      col.names = c("Method", "RMSE", "Lambda" )) %>%
      column_spec(column = 2, color = "#fbfcfc", background = "#21618c") %>%
      row_spec(row = 0, bold = T, color = "#212f3d", background = "#fcf3cf") %>%
      row_spec(row = 7, color = "#fbfcfc", background = "#7b241C")

#-------------------------END REGULARIZATION MOVIE + + USER + TIME EFFFECT  -----------------------------------------

