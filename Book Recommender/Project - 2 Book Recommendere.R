setwd("C:/Users/enerc/OneDrive/Desktop/data science/sessions/r_training")
getwd()

#install.packages("data.table","tidyr","stringr","DT","knitr","grid","gridExtra","corrplot","qgraph","methods")

library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(corrplot)
library(qgraph)
library(methods)
library(Matrix)

books <- read.csv("books.csv")
ratings <- read.csv("book_ratings.csv")
book_tags <- read.csv("book_tags.csv")
tags <- read.csv("tags.csv")


# 1) In the first phase, we'd do a bit of data cleaning.
# a. So, we'll start off by removing the duplicate ratings. i.e., there are cases where a user has 
# rated the same book more than one time. So, we'll go ahead & remove all these instances. 
# b. After which, we'll go ahead & remove those users who have rated fewer than 3 books

ratings %>% group_by(user_id, book_id) %>% mutate(N=n()) -> ratings
table(ratings$N)
ratings %>% filter(N >1) -> duplicate_rating

ratings %>% filter(N==1) -> ratings    #Remove duplicate ratings

# Removing the user who has rated less than 3

ratings %>% group_by(user_id) %>% mutate(Ratings_given=n()) -> ratings
ratings %>% filter(Ratings_given > 2) -> ratings

# 2) In the second phase we'll do some data exploration
# a. We'll start off by extracting a sample set of 2% records from the entire dataset. 
# b. Then, we will make a bar-plot for the distribution of ratings. i.e we'd want to analyze the 
# count of different ratings. 
# c. After which, we'll make a plot to understand how many times each book has been rated. 
# d. Then, we'll make a plot for the percentage distribution of different 'genres'. 
# e. Going ahead, we'll find the top 10 books with highest ratings. 
# f. And finally, we'll find out the 10 most popular books

set.seed(1)
user_fraction <- 0.02
users <- unique(ratings$user_id)
sample_users <- sample(users, round(user_fraction * length(users)))  
ratings %>% filter(user_id %in% sample_users) -> ratings


ggplot(data = ratings,aes(x = rating, fill = factor(rating))) + geom_bar(color = "grey20") + 
  scale_fill_brewer(palette = "BuGn") + guides(fill = FALSE)

ratings %>% 
  group_by(book_id) %>% 
  summarize(number_of_ratings_per_book = n()) %>% 
  ggplot(aes(number_of_ratings_per_book)) + 
  geom_bar(fill = "blue", color = "grey20", width = 1) + coord_cartesian(c(0,40))


genres <- str_to_lower(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Cookbooks", "Crime", "Fantasy", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))

available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres, tags$tag_name)]


book_info <- book_tags %>% 
  filter(tag_id %in% available_tags) %>% 
  group_by(tag_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(sumN = sum(n), percentage = n / sumN) %>%
  arrange(-percentage) %>%
  left_join(tags, by = "tag_id")  

book_info %>% 
  ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage)) + geom_bar(stat = "identity") + coord_flip() + scale_fill_distiller(palette = 'YlOrRd') + labs(y = 'Percentage', x = 'Genre')



books %>% 
  arrange(-average_rating) %>% 
  top_n(10,wt = average_rating) %>% 
  select(title, ratings_count, average_rating)  -> top10_ratings


books  %>% 
  arrange(-ratings_count) %>% 
  top_n(10,wt = ratings_count) %>% 
  select(title, ratings_count, average_rating) -> top10_popular

# 3) In the 3rd phase, we'll finally do some recommending!!!!
#   a. So, we'll start off by building the 'user-based collaborative filtering' model.
# b. Then, we'll recommend 6 new books for two different readers

dimension_names <- list(user_id = sort(unique(ratings$user_id)), book_id = sort(unique(ratings$book_id)))
ratingmat <- spread(select(ratings, book_id, user_id, rating), book_id, rating) %>% select(-user_id)

ratingmat <- as.matrix(ratingmat)
ratingmat[,-1] -> ratingmat    # removing user_id
dimnames(ratingmat) <- dimension_names
ratingmat[1:5, 1:5]
dim(ratingmat)


#converting the rating matrix into a real rating matrix
ratingmat0 <- ratingmat  
dim(ratingmat0)
ratingmat0[is.na(ratingmat0)] <- 0
sparse_ratings <- as(ratingmat0, "sparseMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

#Splitting the data into train & test
sample(x=c(T,F),size=nrow(real_ratings),replace = T, prob = c(0.9,0.2)) -> split_book
real_ratings[split_book,] -> book_train
real_ratings[!split_book,] ->book_test


#Building the ubcf model
Recommender(data = book_train,method="IBCF") -> book_model_ibcf
n_recommended_ibcf <- 6

#Recommending books
predict(object = book_model_ibcf, newdata = book_test, n = n_recommended_ibcf) -> book_predicted_ibcf


#Recommeding books for user number-1
book_predicted_ibcf@items[[1]]->user1_book_numbers
book_predicted_ibcf@itemLabels[user1_book_numbers]

books %>% filter(id==6343) %>% select(original_title,authors)
books %>% filter(id==7482) %>% select(original_title,authors)
books %>% filter(id==2750) %>% select(original_title,authors)


#Recommeding books for user number-5
book_predicted_ibcf@items[[5]]->user5_book_numbers
book_predicted_ibcf@itemLabels[user5_book_numbers]

books %>% filter(id==4624) %>% select(original_title,authors)
books %>% filter(id==6867) %>% select(original_title,authors)
books %>% filter(id==7326) %>% select(original_title,authors)



