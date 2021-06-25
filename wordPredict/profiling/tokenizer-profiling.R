library(profvis)
source("../R/tokenizer.R")
lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 4000)

# https://adv-r.hadley.nz/perf-measure.html
# https://rstudio.github.io/profvis/
profvis({
  getTokenFrame(lines)
})
