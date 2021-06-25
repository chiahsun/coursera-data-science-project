source("../R/tokenizer.R", chdir = TRUE)

# testthat::test_dir(".")

test_that("totokens works", {
  expect_equal(totokens("I'm fine. How're you?"),  
               c("I", "'m", "fine", ".", "How", "'re", "you", "?"))
  expect_equal(totokens("It's good"),  
               c("It", "'s", "good"))
  expect_equal(totokens(",1"),  
               c(",", "1"))
})

test_that("getTokenFrame", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  expect_true(is.data.frame(df))
  expect_equal(colnames(df)[1], "line")
  expect_equal(colnames(df)[2], "tokens")
  
  expect_equal(df$line[1], "How are you? Btw thanks for the RT. You gonna be in DC anytime soon? Love to see you. Been way, way too long.")
  expect_equal(unlist(df$tokens[1]), c("How", "are", "you", "?", "Btw" , "thanks", 
          "for", "the", "RT", ".", "You", "gonna", "be", "in", "DC", "anytime", 
          "soon", "?", "Love", "to", "see", "you", ".", "Been", "way", ",", "way", "too", "long", "."))
})

test_that("getTokenFrame performance", {
  numRow <- 4000
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), numRow)
  
  start_time <- Sys.time()
  df <- getTokenFrame(lines)
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 0.4)
  # expect_equal(nrow(df), 2360148)
  expect_equal(nrow(df), numRow)
})  