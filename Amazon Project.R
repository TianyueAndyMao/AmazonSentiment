library(jsonlite)
library(readr)
library(magrittr)
library(stringr)
library(plyr)
library(tm)
library(tidytext)

#Read in data from http://jmcauley.ucsd.edu/data/amazon/ 

#We are only using the Movie/TV data since using the entirety of the data is not computationally possible on my desktop
if(FALSE){
  movie_data <- stream_in(gzcon(url("http://snap.stanford.edu/data/amazon/productGraph/categoryFiles/reviews_Movies_and_TV_5.json.gz")))
  all.equal(movie_data, as.data.frame(movie_data2))
  
  #Make subset of the data
  
  idx = sample(nrow(movie_data), 50000)
  movie_sample = movie_data[idx,]
  
  movie_sample$helpful1 = sapply(movie_sample$helpful, function(x) x[1])
  movie_sample$helpful2 = sapply(movie_sample$helpful, function(x) x[2])
  
  movie_sample$helpful = NULL
  
  #writing row number to null
  movie_sample$X = NULL
  
  #Save data to CSV file
  write.csv(movie_sample, file = "movie_sample.csv", row.names = FALSE)
}



############################################################################################################
# cleaning ----

#Now we can simply read in the csv file next time
setwd("C:/Users/tiany/Desktop/Amazon Project")

#WHY DOESN'T READ.CSV WORK????????
movie_sample = read_csv("movie_sample.csv")
movie_sample = movie_sample[1:100,]

#
movie_sample$reviewText = tolower(movie_sample$reviewText) # lowercase
reviewText2 =gsub('\\w\'\\w', '', movie_sample$reviewText) # remove apostrophes (might not work)
reviewText3 =  gsub('[[:punct:][:digit:]]', ' ', reviewText2) # remove punctuations and digits"

# check if certain words were correctly removed
word_check = function(word = "movie", text) {
  sapply(text, function(x) grepl(word, sprintf('\\b%s\\b', x))) -> is_word_in_review
  names(is_word_in_review) = NULL
  return(is_word_in_review)
}


# Stop words ----
#Now we eliminated stop words because they do not contribute semantically
# Maybe we should just use tidytext::stop_words...
stop_words = scan("stopwords.txt", what = "character")
# stop_words = tidytext::stop_words


#WHY NO WORK?
# find.string = paste(unlist(stop_words), collapse = " | ")
# reviewText4 = gsub(find.string, replacement = " ", x = reviewText3)


# remove stop_words
reviewText4 = lapply(as.vector(reviewText3), function(x) {
  t <- unlist(strsplit(x, " "))
  t[!(t %in% stop_words)]
})

# remove blank characters
reviewText5 = lapply(reviewText4, function(x) {
  x[x != ""]
})


# stop_words_regex = paste0("\\b", stop_words, "\\b")
# reviewText4 = str_replace_all(reviewText3, stop_words_regex, " ")
# reviewTextList = strsplit(reviewText5, "\\s+")


#Analyze Emojis?

# see word frequencies ----
ALL_WORDS = unlist(reviewText5)
table(ALL_WORDS) %>% sort(., decreasing = TRUE) %>% head(100)
"awful" %in% ALL_WORDS

#tidy text ----
# convert list of words into vector of strings
reviewText6 = sapply(reviewText5, paste, collapse = " ")

# add vector as a new column
movie_sample$reviewText_clean = reviewText6

# remove unnecessary columns
movie_sample_clean = subset(movie_sample, select = c(-reviewText, -summary, -reviewerName))

# tidy it up, make tokens: one word per row
# See austen_ex.R for more info
tidy_reviews = movie_sample_clean %>%
  group_by(asin) %>%
  ungroup() %>%
  unnest_tokens(word, reviewText_clean)
View(tidy_reviews)


















