library(profvis)
source("../R/reader.R")
source("../R/ngram.R")
source("../R/tokenizer.R")

lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 1700)


profvis({
  N <- 3
  colnms <- c()
  for (k in 1:N) {
    colnms <- c(colnms, paste('w', k, sep = ""))
  }
  df <- getTokenFrame(lines)
  excludes <- c(",", "!", "?", ".", "", " ", "-", "\"", "'")
  for (k in 1:nrow(df)) {
    now <- getNGram(unlist(df[k,]$tokens), colnms, excludes)
  }
})
