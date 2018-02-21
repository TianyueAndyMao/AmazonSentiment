library(readr)
movie_sample = read_csv("movie_sample.csv")
stop_words = tidytext::stop_words
num_reviews = seq(5000, 10000, by = 500)
num_unique_words = numeric(length(num_reviews))
for (i in 1:length(num_reviews)) {
  nrows = num_reviews[i]
  m_s = movie_sample[1:nrows,]
  m_s$reviewText = tolower(m_s$reviewText) # lowercase
  reviewText2 =gsub('\\w\'\\w', '', m_s$reviewText) # remove apostrophes (might not work)
  reviewText3 =  gsub('[[:punct:][:digit:]]', ' ', reviewText2) # remove punctuations and digits"
  # remove stop_words
  reviewText4 = lapply(as.vector(reviewText3), function(x) {
    t <- unlist(strsplit(x, " "))
    t[!(t %in% stop_words)]
  })
  
  # remove blank characters
  reviewText5 = lapply(reviewText4, function(x) {
    x[x != ""]
  })
  uniqWords = unique(unlist(reviewText5))
  num_unique_words[i] = length(uniqWords)
  print(num_unique_words[i])
}

plot(x = num_reviews, y = num_unique_words, type = "l")
