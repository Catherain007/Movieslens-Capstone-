################################################################################
#title: "MovieLens Recommendation System Project"
#subtitle: "HarvardX - PH125.9x: Data Science: Capstone"
#author: "YIN THU WIN"
################################################################################

##[My Github Repo](https://github.com/Catherain007/Movieslens-Capstone-)

#Install the pacman package
if(!require(pacman)) install.packages("pacman", repos = "http://cran.us.r-project.org")
#Load the required libraries
#If a package below is missing, p_load will automatically download it from CRAN
pacman::p_load(tidyverse, ggplot2, ggthemes, data.table, lubridate, caret, 
               knitr, scales, treemapify)
#All Data for the MovieLens Dataset Will be obtained from the following sources:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
#Data Preparation
#Download File

library(lubridate)
library(data.table)
library(corrplot)
library(corrr)
library(tinytex)





dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")



### if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
title = as.character(title),
genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")


### Validation set will be 10% of MovieLens data


set.seed(1, sample.kind ="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


### Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

### Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#### remove unnecessary file for better processing time
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# 2.1.1 Exploratory Data Analysis

#### Check missing values in any column.
sapply(edx, {function(x) any(is.na(x))}) %>% knitr::kable()



dim(edx)


edx %>% as_tibble()


## Explore the features and classes of edx while also confirming its observations


glimpse(edx)


# Deployment for the unique number of userIds, movieIds, and genres

edx %>% summarize(unique_users = length(unique(userId)),
                  unique_movies = length(unique(movieId)),
                  unique_genres = length(unique(genres)))




# 2.2 Ratings:


length(unique(edx$rating))



rp <-edx %>% filter(edx$rating >=3)
nrow(rp)/length(edx$rating)

##### Data Visualizations and Analysis
 
avge_rating <- mean(edx$rating) # calculate overall average rating of whole edx data set
medi_rating <- median(edx$rating) # calculate median rating of whole edx data set




######RATING

  edx_ratings <- edx %>% # take data from edx and. assign new data set.. 
  group_by(rating) %>% # ...group data by rating and... 
  summarize(num_ratings = n()) %>% # ...summarize frequency of each rating and... 
  arrange(desc(num_ratings)) # ...arrange data in descending order

edx_ratings # display rating frequencies
edx_ratings %>% # take data and..... 
  ggplot(aes(rating, num_ratings)) + # scatter plot rating vs frequency ........and
  geom_point(aes(size = num_ratings)) + #  display rating point size with number of ratings.......and.
    scale_size_continuous(limits = c(0, 7e+06)) + # set the scale in Y........and 
  xlim(0,5) + # set the scale in x....and
  labs(x = " Ratings", y = "Frequency of Ratings", title = "Frequency of Ratings by each Rating")
            # puting the lables 





## 2.2.1 User



edx %>%  # finding unique userId in data set 
  
  summarize(num_users = n_distinct(userId))

edx_users <- edx %>% # take data from edx and. assign new data set.. 
  group_by(userId) %>% # ...group by user and... 
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and... 
  arrange(desc(num_ratings)) # ...arrange data in descending order



edx_users %>%
  ggplot(aes(x = userId, y = num_ratings, color = avg_rating)) +  # Scatter plot with userId and number of ratings, colored by average rating
  geom_point(size = 3, alpha = 0.6) +  # Adjust size and transparency for visibility
  scale_color_gradientn(colours = rainbow(6)) +  # Apply a color gradient to represent average rating
  labs(
    title = "Number of Ratings by UserId",  # Main title
    x = "UserId",  # Label for the x-axis
    y = "Number of Ratings",  # Label for the y-axis
    color = "Average Rating"  # Color legend title
  ) +
  theme_minimal() +  # Apply minimal theme for a clean look
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "darkblue", hjust = 0.5),  # Title styling
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "darkred"),  # Rotate x-axis labels and style them
    axis.text.y = element_text(size = 12, color = "darkgreen"),  # Style y-axis labels
    panel.grid.major = element_line(color = "gray", size = 0.3),  # Grid lines for the plot
    panel.grid.minor = element_blank()  # Remove minor grid lines for a cleaner appearance
  )


top_10users<-head(edx_users,10) # assign data to new data set

lowest_10users<-tail( edx_users,10) # assign data to new data set

plot(top_10users) # correlation plot for top 10 user

 plot(lowest_10users) # correlation plot for bottom 10 user



## 2.2.2 Movie

edx %>% # finding unique movieId in data set 
  summarize( n_movies = n_distinct(movieId))

edx_films <- edx %>% # take data and assign new data set and... 
  group_by(movieId) %>% # ...group by movie and... 
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and... 
  arrange(desc(num_ratings)) # ...arrange data in descending order


edx_films %>% 
  ggplot(aes(x = movieId, y = num_ratings, color = avg_rating)) +  # Plot movieId vs number of ratings with color by average rating
  geom_point(size = 3, alpha = 0.6) +  # Add points with adjusted size and transparency for better visualization
  scale_color_gradientn(colours = terrain.colors(55)) +  # Apply a 5-color terrain gradient for better color differentiation
  labs(
    x = "Movie ID",  # Label for the x-axis
    y = "Number of Ratings",  # Label for the y-axis
    title = "Ratings by Movie ID",  # Title of the plot
    color = "Average Rating"  # Label for the color legend
  ) +
  theme_light() +  # Apply a light theme for a clean and modern look
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "darkblue", hjust = 0.5),  # Title styling with bold and centered alignment
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "darkred"),  # Rotate x-axis labels and adjust their style
    axis.text.y = element_text(size = 12, color = "darkgreen"),  # Style y-axis labels
    panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines with gray color
    panel.grid.minor = element_blank()  # Remove minor grid lines for a cleaner look
  )




####plot with unique movie 
edx_films %>%
  mutate(row_number = 1:n()) %>%  # Create a unique identifier for each movie using row numbers
  ggplot(aes(x = row_number, y = num_ratings, color = avg_rating)) +  # Scatter plot: row number (movie ID) vs. number of ratings
  geom_point(size = 3, alpha = 0.7) +  # Points with size adjustment and slight transparency
  scale_color_gradientn(colours = terrain.colors(7)) +  # Apply a color gradient based on average ratings
  labs(
    x = "Unique Movie ID",  # X-axis label
    y = "Total Ratings",  # Y-axis label
    title = "Distribution of Ratings Across Movies",  # Main plot title
    color = "Average Movie Rating"  # Color legend label
  ) +
  theme_light() +  # Light theme for a clean, modern look
  theme(
    plot.title = element_text(size = 18, face = "bold", color = "navy", hjust = 0.5),  # Bold title with centered alignment
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "darkorange"),  # Rotated X-axis labels with adjusted size
    axis.text.y = element_text(size = 12, color = "darkgreen"),  # Styled Y-axis labels with custom size and color
    panel.grid.major = element_line(color = "gray80", size = 0.3),  # Lighter gray for major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines for a cleaner appearance
    plot.margin = margin(20, 20, 20, 20)  # Increase plot margins for better readability
  )
                       # putting the labels

# the data frame top_title contains the top 10 movies which count the major number of ratings
top_title <- edx %>% # take the data .....and
  group_by(title) %>% # group by movie title .....and
  summarize(count=n()) %>% # ...summarize ratings counts ...and 
  top_n(10,count) %>% # take top 10 rating numbers..and
  arrange(desc(count)) # arrange data in decent order

#Horizontal bar chart of top_10 title of  movies 



top_title %>%  # Take the data for movie titles and rating counts
  ggplot(aes(x = reorder(title, count), y = count)) +  # Reorder the titles based on the count of ratings
  geom_bar(stat = "identity", fill = "#FF6347", color = "black", width = 0.7) +  # Create a bar plot with a tomato color and black borders
  coord_flip(ylim = c(0, 40000)) +  # Flip the coordinates and set the y-axis range
  geom_text(aes(label = count), hjust = -0.1, size = 3.5, color = "black") +  # Add rating counts with better spacing and color
  labs(
    title = "Top 10 Movies Based on Number of Ratings",  # Add a modernized title
    x = NULL,  # Remove the x-axis label for cleaner presentation
    y = "Number of Ratings"  # Add y-axis label
  ) +
  theme_minimal() +  # Use a minimalistic theme
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "darkblue"),  # Customize the title
    axis.text.x = element_text(angle = 45, hjust = 1, color = "darkgreen"),  # Rotate x-axis labels and customize color
    axis.text.y = element_text(color = "darkred"),  # Customize y-axis label color
    panel.grid.major = element_line(color = "gray", size = 0.5, linetype = "dashed"),  # Add a dashed grid for a modern touch
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )


## 2.2.3 Genres

edxgenres<-edx%>% # take data from edx and assign new data set
  group_by(genres) %>% # ...group data by genre and... 
  summarize(num_ratings = n(), avg_rating = mean(rating)) %>% # ...summarize ratings counts and average rating and... 
  arrange(desc(num_ratings)) # ...arrange data in descending order

head(edxgenres,10) # show top 10 rating genere

top_10g<-head(edxgenres,10)  # assign top genere data to new data set


tail(edxgenres,10) # show lowest 10 rating genere


top_10g %>%  # Take the data for top 10 genres
  ggplot(aes(x = genres, y = num_ratings, color = avg_rating)) +  # Scatter plot between genres and number of ratings, with color based on average rating
  geom_point(size = 7) +  # Plot the points with a size of 4 for visibility
  scale_colour_gradientn(colours = rainbow(7)) +  # Set the color gradient to a rainbow color palette
  labs(x = "Genre", y = "Number of Ratings", title = "Ratings by Genre") +  # Add labels for x, y, and title
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "darkred", size = 10),  # Rotate x-axis labels to 45 degrees, change color to darkred, and adjust size
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold"),  # Center the title and customize its appearance
    plot.subtitle = element_text(hjust = 0.5, color = "green", size = 12),  # Center subtitle and change its color
    panel.background = element_rect(fill = "darkgray"),  # Set the panel background to light gray
    panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "gray"),  # Major grid lines in solid gray
    panel.grid.minor = element_line(size = 0.25, linetype = "dotted", color = "lightgray")  # Minor grid lines in dotted light gray
  )



#An error bar plots for genres with more than 100000 ratings

edx %>% 
  group_by(genres) %>%  # Group the data by genres
  summarize(n = n(), 
            avg_rating = mean(rating), 
            se = sd(rating) / sqrt(n())) %>%  # Calculate the number of ratings, average rating, and standard error
  filter(n >= 100000) %>%  # Filter genres with ratings greater than or equal to 100,000
  mutate(genres = reorder(genres, avg_rating)) %>%  # Reorder genres by average rating
  ggplot(aes(x = genres, y = avg_rating, ymin = avg_rating - 2 * se, ymax = avg_rating + 2 * se)) + 
  geom_point(color = "darkblue") +  # Plot the genres vs. average rating with points
  geom_errorbar(width = 0.7, color = "red") +  # Add error bars to represent the standard error
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "darkgreen", size = 10)) +  # Rotate x-axis labels and customize appearance
  labs(title = "Error Bar Plots by Genres", subtitle = "Average Ratings with Standard Error") +  # Set the title and subtitle
  theme(  # Set a custom background and grid style
    panel.background = element_rect(fill = "skyblue", color = "darkred", size = 1),
    panel.grid.major = element_line(size = 0.9, linetype = "dotted", color = "green"),
    panel.grid.minor = element_line(size = 0.9, linetype = "dotted", color = "yellow"),
    plot.title = element_text(hjust = 0.5, color = "darkblue", size = 14, face = "bold"),  # Center and customize title
    plot.subtitle = element_text(hjust = 0.5, color = "purple", size = 12)  # Center and customize subtitle
  )


## 2.2.4 Time

##### TIME 

#ggplot showing timestamp per date(week unit)
  edx %>% 
    mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%  # Add a new 'date' column with rounded dates
    group_by(date) %>%  # Group by the 'date' column
    summarize(avg_rating = mean(rating)) %>%  # Calculate the average rating for each week
    ggplot(aes(x = date, y = avg_rating)) + 
    geom_line(color = "blue") +  # Line plot for the average rating over time
    geom_point(color = "red") +  # Add red points on the line for each data point
    ggtitle("Timestamp, Time Unit: Week") +  # Add a main title
    labs(subtitle = "Average Ratings") +  # Add a subtitle
    theme_minimal()  # Apply a minimal theme for better aesthetics


# 2.3 Developing the Linear Regression Models

## 2.3.1 Simple linear regression model (Average)


mue <- mean(edx$rating)
mue


##  Considering individual effect of predictors to the first model

### 2.3.2 Movie Effect Model


####movie effect (e_i) determine bias for each movie (mean rating of the movie compare to overall mean)

movie_avgs <- edx %>% # take data from edx....and assign new data set
  group_by(movieId) %>% # group by movieId..and 
  summarise(e_i = mean(rating) - mue) # summarize result of movie effect...

#### Movie bias/effect plots

EM_hist <- movie_avgs %>% #assign data to new data set....and
  ggplot(aes(e_i)) +  #plot movie bias histrogram vs movie count......
  geom_histogram(color = "blue", fill="gray",bins=40) + #select bin size and fill color
  ggtitle("Movie Effect Distribution Histogram") +
  xlab("Movie Bias") +
  ylab("Movie Count") # put all labels 
  
plot(EM_hist) # plot the histogram data of bias

### 2.3.3 User Effect Model


#####User effect (e_u) determine bias for each user (mean rating of the user compare to overall mean)

user_avgs <- edx %>% # take data from the edx. and assign as new data set.....
  group_by(userId) %>%  # make group by userID.....and 
  summarise(e_u = mean(rating) - mue) # summarize result of user effect...



### 2.3.4 Genres Effect Model



#### Time effect (e_g) determine bias for each genres group (mean rating of the genre compare to overall mean)
gene_avgs <- edx %>% # Take  data from edx....and assign new data set
  group_by(genres) %>% # Group by (genres)
  summarise(e_g = mean(rating) - mue) # summarize result of genres effect...





### 2.3.5 Time Effect Model


##### Time effect (e_t) determine bias for each rate time by week (mean rating of the time compare to overall mean)

## put additional column name date in edx data set 

edx <- edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "week")) 

time_avgs <- edx %>% #assign data to new data set
  group_by(date) %>%  # group by date 
  summarise (e_t = mean(rating) - mue ) # summarize result of time effect...





##  Combining multiple predictors effect to the simple model

### 2.3.6  Combine Movie and User Effects Model

### 2.3.7 Combine Movie + User + Genres Effects Model

##The visualize data shows some evidence of a genre effect. If we define $g_{u,i}$ as the genre for user's $u$ rating of movie $i$, the new model will be like following,

##$Y_{u,i} = \mu + e_i + e_u + \sum_{k=1}^K x_{u,i} \beta_k + \varepsilon_{u,i}$
  
 ##3 Where \$ g_u,i\$ is defined as the genre for user’s u and $x^k_{u,i} = 1$ if $g_{u,i}$ is genre $k$. $\mu$ is defined as in (1) We try to present the model here for explanation, and we will select predictors correlation matrix in following section for model improvement.

## 2.4 Correlation between dependent and independent variables




##### Add user bias column to edx and assign new data set for additional coulumns

edx_bias <- edx  %>% left_join(movie_avgs, by='movieId')%>% #left join movie bias data to edx and assign new data set 
  left_join(user_avgs, by='userId')%>% #left join user bias data to edx and assign new data set 
  left_join(gene_avgs,'genres')%>% #left join gene bias data to edx and assign new data set 
  left_join(time_avgs,'date') #left join time bias data to edx and assign new data set 

##### select bias/effect columns for more exploration

edxbias<-edx_bias  %>% select(userId,rating,e_i,e_u,e_g,e_t) # assign new data set and select bias values for new analysis

head(edxbias) # print top six row of data set to check no required value missing

###### Add additional column for correlation analysis

edxbiasall<- edxbias%>% mutate(moviebias=mue+e_i) %>% #assign new data set and put new moviebias column
  mutate(userbias=mue+e_u) %>% #assign new data set and put new userbias column
  mutate(genresbias=mue+e_g) %>% #assign new data set and put new genres bias colum
  mutate(timebias=mue+e_t) %>% #assign new data set and put new timebias column
  mutate (use_mov_bs=mue+e_u+e_i) %>% #assign new data set and put new user moviebias column
  mutate (use_mov_ge_bs= mue+e_u+e_i+e_g) #assign new data set and put new user movie genres bias column
### Correlation check for dependent variable and combine independent variables

corr <- edxbiasall %>% select(rating, moviebias,userbias,genresbias,timebias,use_mov_bs,use_mov_ge_bs)
index <- sample(1:nrow(corr), 1000000)
corr <- corr[index, ]

corrplot(cor(corr), method = "number", type="upper")

###Check correlation value between dependent and independent variables and themself

y<- (edxbiasall$rating)
x<- (edxbiasall$use_mov_bs)
x1<- (edxbiasall$use_mov_ge_bs)

cor(y,x)
cor(y,x1)
cor(x,x1)


## 2.5 Evaluation Method of the models


######RMSE (residual mean square error) 

RMSE <- function(true_ratings, predted_RATs){
  sqrt(mean((true_ratings - predted_RATs)^2))
}




## 2.6 Regularization


#### Made data partation for test and train  from edx set for regularization

set.seed(1, sample.kind ="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

### Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

### Add rows removed from test set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)
rm(temp,removed,test_index)

#### Regularization of movie + user effect model ##



lmdas <- seq(0, 10, 0.2)


rmses <- sapply(lmdas, function(l){
  muu <- mean(train$rating)
  b_i <- train %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - muu)/(n()+l))
  b_u <- train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - muu)/(n()+l))
  predted_RATs <- test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = muu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predted_RATs,test$rating))
})

### plot Rmses vs lambdas


L<-qplot(lmdas, rmses,geom=c("point", "line"))  # assign qplot for lambda and rmses
L+labs(title = "Rmses vs lambdas ") # add the main title for plot

### find optimal lambda (optimun parameter)

lmda <- lmdas[which.min(rmses)] # assign lambda value which give minimun rmse value
lmda # print miniumun lambda value for rsme



# 3. Results and Discussions

#####TEST all MODELS####
####Put new column of date in validation set

validationSET <- validation %>% mutate(date = round_date(as_datetime(timestamp), unit = "week"))

### predict all unknown ratings mu with validation rating

mu<-mean(edx$rating)

naive_rmse <- RMSE(validationSET$rating, mu) # assign rmse value 


### create a table to store results of prediction approaches

rmseVAL_results <- tibble(Method = "The Average", RMSE = naive_rmse) # put result to rmse table and put the name of result

#######  RMSE results with time effect


predted_RATs <- validationSET %>% # assign predicted rating  and...
  left_join(time_avgs, by="date") %>% # left join time avgs by date to validation set....
  mutate(pred = mu + e_t) %>% # put new column of predition 
  .$pred # take predit value

mdl_2_rmse <- RMSE(predted_RATs, validationSET$rating) # assign rmse value 
rmseVAL_results <- bind_rows(rmseVAL_results,                    # put  result to the rmse result table
                             tibble(Method="Time Effect Model", # put the name of rmse results
                                    RMSE = mdl_2_rmse))

########
## RMSE results with gene effect

predted_RATs <- validationSET %>%  # assign predicted rating  and...
  left_join(gene_avgs, by="genres") %>%   # left join genre avgs by date to validation set....
  mutate(pred = mu + e_g) %>%  # put new column of predition 
  .$pred  # take predit value
mdl_3_rmse <- RMSE(predted_RATs, validationSET$rating) # assign rmse value 
rmseVAL_results <- bind_rows(rmseVAL_results,                     # put  result to the rmse result table
                             tibble(Method="Genres Effect Model",  # put the name of rmse result
                                    RMSE = mdl_3_rmse))    

#######  RMSE results with user effect

predted_RATs <- validationSET %>% #assign predicted rating  and..
  left_join(user_avgs, by="userId") %>% # left join user avgs by date to validationSET set....
  mutate(pred = mu + e_u) %>%  # put new column of predition 
  .$pred # take predit value
mdl_4_rmse <- RMSE(predted_RATs, validationSET$rating) # assign rmse value 
rmseVAL_results <- bind_rows(rmseVAL_results, # put  result to the rmse result table
                             tibble(Method="User Effect Model",
                                    RMSE = mdl_4_rmse)) # put the name of rmse result




###  RMSE results with movie effect

predted_RATs <- validationSET %>% # assign predicted rating  and..
  left_join(movie_avgs, by='movieId') %>% # left join movie avgs by date to validationSET set....
  mutate(pred = mu + e_i ) %>% # put new column of predition 
  .$pred # take predit value
mdl_5_rmse <- RMSE(predted_RATs, validationSET$rating) # assign rmse value 
rmseVAL_results <- bind_rows(rmseVAL_results,    # put  result to the rmse result table
                             tibble(Method="Movie Effect Model",
                                    RMSE = mdl_5_rmse)) # put the name of rmse result




###  new RMSE results with user&movie effect


predted_RATs <- validationSET %>% #assign predicted rating  and..
  left_join(movie_avgs, by='movieId') %>% # left join movie avgs by date to validationSET set....
  left_join(user_avgs, by='userId') %>% # left join user avgs by date to validationSET set....
  mutate(pred = mu + e_i + e_u) %>% # put new column of predition 
  .$pred # take predit value
mdl_6_rmse <- RMSE(predted_RATs, validationSET$rating) # assign rmse value 
rmseVAL_results <- bind_rows(rmseVAL_results,   # put  result to the rmse result table
                             tibble(Method = "Combine Movie and User Effects Model",
                                    RMSE = mdl_6_rmse)) # put the name of rmse result


### new RMSE results with regularize user&movie effect with tuning parameter (lambda)

lmda <- 5
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(e_i = sum(rating - mu)/(n()+lmda)) 

user_reg_avgs <- edx %>% 
  left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(e_u = sum(rating - mu-e_i)/(n()+lmda))

predted_RATs <- validationSET %>% #assign predicted rating  and..
  left_join(movie_reg_avgs, by='movieId') %>% # left join movie avgs by date to validationSET set....
  left_join(user_reg_avgs, by='userId') %>% # left join user avgs by date to validationSET set....
  mutate(pred = mu + e_i + e_u) %>% # put new column of predition 
  .$pred # take predit value

mdl_7_rmse  <- RMSE(predted_RATs, validationSET$rating)

rmseVAL_results <- bind_rows(rmseVAL_results, # assign rmse value and put  result to the rmse result table
                             tibble(Method = "Regularized combine Movie and User Effects Model",
                                    RMSE = mdl_7_rmse)) # put the name of rmse result

### table showing all model final test results

rmseVAL_results %>% knitr::kable()







