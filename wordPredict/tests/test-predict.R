source("../R/predict.R", chdir = TRUE)
source("../R/model.R", chdir = TRUE)
source("../R/tokenizer.R", chdir = TRUE)
source("../R/reader.R", chdir = TRUE)
source("../R/ngram.R", chdir = TRUE)

test_that("predict1GramNext", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table2 <- getNGramTable(df, 2)
  res <- predict1GramNext(table2, c("the"))
  expect_equal( res, 
                tibble(
                  w1 = c("the", "the"),
                  w2 = c("RT", "wonderful"),
                  count = c(1, 1),
                  prob = c(0.008130081, 0.008130081),
                  dprob = c(0.5, 0.5),
                  nxt = c("RT", "wonderful")), 
                tolerance=1e-7
              )
  expect_identical(sum(res$dprob), 1)
})  

test_that("generate1GramNext", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table1 <- getNGramTable(df, 1)
  table2 <- getNGramTable(df, 2)
  g1 <- generate1GramNext(table1, table2, skipThreshold = 0)
  expect_equal(g1(), list(word = "'ll", `1gram:next you` = 0.6666667), tolerance=1e-6)
  expect_equal(g1(), list(word = "meet", `1gram:next you` = 0.3333333), tolerance=1e-6)
  expect_equal(g1(), list(word = "reconnect", `1gram:next to` = 0.3333333), tolerance=1e-6)
  
  count <- 1
  loop(for (x in generate1GramNext(table1, table2, skipThreshold = 0, coverage = 0.5)) {
    count <- count + 1
  })
  expect_equal(count, 17)
})  

test_that("predict1GramNext performance", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 500)
  df <- getTokenFrame(lines)
  table1 <- getNGramTable(df, 1)
  table2 <- getNGramTable(df, 2)
  
  start_time <- Sys.time()
  count <- 1
  loop(for (x in generate1GramNext(table1, table2)) {
    count <- count + 1
  })
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 0.12) # TODO: too slow?
  expect_equal(count, 378)
})  

test_that("predict2GramNext", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table3 <- getNGramTable(df, 3)
  res <- predict2GramNext(table3, c("'ll", "smile"))
  expect_equal( res, 
                tibble(
                  w1 = c("'ll"),
                  w2 = c("smile"),
                  w3 = c("for"),
                  count = c(1),
                  prob = c(0.01020408),
                  dprob = c(1),
                  nxt = c("for")), 
                tolerance=1e-6
  )
  expect_identical(sum(res$dprob), 1)
})  

test_that("generate2GramNext", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table2 <- getNGramTable(df, 2)
  table3 <- getNGramTable(df, 3)
  g2 <- generate2GramNext(table2, table3, skipThreshold = 0)
  expect_equal(g2(), list(word = "know", `2gram:next you.'ll` = 0.5))
  expect_equal(g2(), list(word = "smile", `2gram:next you.'ll` = 0.5))

  count <- 1
  loop(for (x in generate2GramNext(table2, table3, skipThreshold = 0)) {
    count <- count + 1
  })
  expect_equal(count, 13)
})  

test_that("predict3GramNext", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table4 <- getNGramTable(df, 4)
  res <- predict3GramNext(table4, c("get", "another", "day"))
  expect_equal( res, 
                tibble(
                  w1 = c("get"),
                  w2 = c("another"),
                  w3 = c("day"),
                  w4 = c("off"),
                  count = c(1),
                  prob = c(0.01315789),
                  dprob = c(1),
                  nxt = c("off")), 
                tolerance=1e-6
  )
  expect_identical(sum(res$dprob), 1)
})  

test_that("generate3GramNext", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table3 <- getNGramTable(df, 3)
  table4 <- getNGramTable(df, 4)
  g3 <- generate3GramNext(table3, table4, skipThreshold = 0)
  expect_equal(g3(), list(word = "no", `3gram:next 'll.smile.for` = 1))
  expect_equal(g3(), list(word = "wakes", `3gram:next (:.and.THIS` = 1))
  
  count <- 1
  loop(for (x in generate3GramNext(table3, table4, skipThreshold = 0)) {
    count <- count + 1
  })
  expect_equal(count, 13)
})

test_that("getPredictFunc", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 100)
  predict <- getPredictFunc(lines)
  # TODO: add test cases
  predict(c("in"))
  predict(c("the"))
}) 

test_that("getPredictFunc performance", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 300)
  
  start_time <- Sys.time()
  predict <- getPredictFunc(lines)
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 0.6)
}) 

test_that("getPredictFuzzyFunc", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 250)
  predictFuzzy <- getPredictFuzzyFunc(lines)
  
  res <- predictFuzzy(c("first"))
  expect_identical(sum(res$dprob), 1)
  
  expect_equal(as_tibble(res[1, ]),
                   tibble(
                     w1 = c("first"),
                     w2 = c("discovered"),
                     count = c(1),
                     prob = c(0.0003825555),
                     dprob = c(0.1428571),
                     nxt = c("discovered"),
                     cor1 = c(1),
                     sprob = c(0.0003825555)
                   ), tolerance=1e-6)
  
  expect_equal(as_tibble(res[3, ]),
               tibble(
                 w1 = c("all"),
                 w2 = c("day"),
                 count = c(2),
                 prob = c(0.0007651109),
                 dprob = c(0.1428571),
                 nxt = c("day"),
                 cor1 = c(0.5),
                 sprob = c(0.0003825555)
               ), tolerance=1e-6)
})  

# bug for cor be 0
#  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 550)
# predictFuzzy <- getPredictFuzzyFunc(lines)
# res <- predictFuzzy(c("and", "you"))

test_that("getPredictFuzzyFunc 2", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 600)
  predictFuzzy <- getPredictFuzzyFunc(lines)
  
  res <- predictFuzzy(c("have", "a"))
  expect_identical(sum(res$dprob), 1)
  
  expect_equal(as_tibble(res[1, ]),
               tibble(
                 w1 = c("have"),
                 w2 = c("a"),
                 w3 = c("bf"),
                 count = c(1),
                 prob = c(0.0001853568),
                 dprob = c(0.1492579),
                 nxt = c("bf"),
                 cor1 = c(1),
                 cor2 = c(1),
                 sprob = c(0.0001853568)
               ), tolerance=1e-6)
  
  expect_equal(as_tibble(res[7, ]),
               tibble(
                 w1 = c("have"),
                 w2 = c("no"),
                 w3 = c("iPhone"),
                 count = c(1),
                 prob = c(0.0001853568),
                 dprob = c(0.07643914),
                 nxt = c("iPhone"),
                 cor1 = c(1),
                 cor2 = c(0.512128),
                 sprob = c(9.492641e-05)
               ), tolerance=1e-6)
})  

test_that("getPredictFuzzyFunc 3", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 600)
  predictFuzzy <- getPredictFuzzyFunc(lines)
  
  res <- predictFuzzy(c("I", "have", "a"))
  expect_identical(sum(res$dprob), 1)
  
  expect_equal(as_tibble(res[2, ]),
               tibble(
                 w1 = c("I"),
                 w2 = c("have"),
                 w3 = c("no"),
                 w4 = c("iPhone"),
                 count = c(1),
                 prob = c(0.0002258866),
                 dprob = c(0.2604349),
                 nxt = c("iPhone"),
                 cor1 = c(1),
                 cor2 = c(1),
                 cor3 = c(0.512128),
                 sprob = c(0.0001156828)
               ), tolerance=1e-6)
})    

# 1000: 3s
# 10000: 45s
test_that("getPredictFuzzyFunc performance", {
  library(testthat)
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 300)
  
  start_time <- Sys.time()
  predictFuzzy <- getPredictFuzzyFunc(lines)
  end_time <- Sys.time()
  end_time - start_time
  expect_lt(end_time - start_time, 1.3)
}) 


# 3 files, 3000, 2.925455 mins
# 3 files, 6000, 8.909532 mins
# 3 files, 9000, 16.70789 mins
test_that("write model", {
  skip("for write model")
  library(testthat)
  numLine <- 9000
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt",
                       "../data/raw/final/en_US/en_US.news.txt",
                       "../data/raw/final/en_US/en_US.blogs.txt"), numLine)
  
  start_time <- Sys.time()
  predictFuzzy <- getPredictFuzzyFunc(lines, skipThreshold = 4, 
                                      writePath = paste("../model/", numLine, "/", sep = ""))
  end_time <- Sys.time()
  end_time - start_time
  expect_lt(end_time - start_time, 1.3)
}) 

test_that("getPredictFuzzyFunc quiz4", {
  skip("Cost too much time")
  
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10000)
  predictFuzzy <- getPredictFuzzyFunc(lines)
  
  predictFuzzy(c("a", "case", "of"))
  predictFuzzy(c("mean", "the"))
  predictFuzzy(c("make", "me", "the"))
  predictFuzzy(c("but", "the"))
  predictFuzzy(c("at", "the"))
  predictFuzzy(c("date", "at", "the"))
  predictFuzzy(c("be", "on", "my"))
  predictFuzzy(c("quite", "some"))
  predictFuzzy(c("in", "quite", "some"))
  predictFuzzy(c("with", "his", "little"))
  predictFuzzy(c("his", "little"))
  predictFuzzy(c("during", "the"))
  predictFuzzy(c("You", "must", "be"))
  
  predictFuzzy(c("live", "and", "I'd"))
  predictFuzzy(c("me", "about", "his"))
  
  predictFuzzy(c("monkeys", "this"))
  predictFuzzy(c("this"))
  predictFuzzy(c("reduce", "your"))
  predictFuzzy(c("take", "a"))
  predictFuzzy(c("settle", "the"))
  predictFuzzy(c("in", "each"))
  predictFuzzy(c("bottom", "to", "the"))
  predictFuzzy(c("from", "playing"))
  predictFuzzy(c("Adam", "Sandler", "'s"))
})

test_that("getPredictFuzzyFuncInner from file", {
  skip("for test read from model")
  predictFuzzy <- getPredictFuzzyFuncInner(
    read_csv("../model/300/table2.bz2"),
    read_csv("../model/300/table3.bz2"),
    read_csv("../model/300/table4.bz2"),
    read_csv("../model/300/ct.bz2")
    )
  predictFuzzy(c("my"))
})
#test_that("getPredictFuzzyFunc quiz2", {
#  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt", 
#                       "../data/raw/final/en_US/en_US.blogs.txt"), 1000)
#  predictFuzzy <- getPredictFuzzyFunc(lines, skipThreshold = 5)
#  
#  res <- predictFuzzy(c("a", "case", "of"))
#})