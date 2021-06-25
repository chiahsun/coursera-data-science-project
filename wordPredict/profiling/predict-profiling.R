library(profvis)
source("../R/tokenizer.R")
source("../R/model.R")
source("../R/predict.R")
lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 300)

profvis({
  predict <- getPredictFunc(lines)
})

profvis({
  predictFuzzy <- getPredictFuzzyFunc(lines)
})