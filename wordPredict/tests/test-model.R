source("../R/reader.R", chdir = TRUE)
source("../R/tokenizer.R", chdir = TRUE)
source("../R/model.R", chdir = TRUE)

test_that("getFrequencyMatrix", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table1 <- getNGramTable(df, 1)
  table2 <- getNGramTable(df, 2)
  g1 <- generate1GramNext(table1, table2, skipThreshold = 0)
  fm <- getFrequencyMatrix(c(g1))

  expect_equal(fm[1:5,1], c("'ll", "'m", "a", "cali", "Cubs"))
  expect_equal(as_tibble(fm[1:2,1:2]), 
                tibble(
                  word = c("'ll", "'m"),
                  `1gram:next you`= c(0.6666667, 0)),
                tolerance=1e-7
  )
  expect_equal(nrow(fm), 27)
  expect_equal(ncol(fm), 16)
  
  table3 <- getNGramTable(df, 3)
  g1 <- generate1GramNext(table1, table2, skipThreshold = 0)
  g2 <- generate2GramNext(table2, table3, skipThreshold = 0)
  fm <- getFrequencyMatrix(c(g1, g2))
  expect_equal(nrow(fm), 35)
  expect_equal(ncol(fm), 27)
  
  # sum(fm != 0)/(dim(fm)[1] * dim(fm)[2])
})  

test_that("getCorrelationTable", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  table1 <- getNGramTable(df, 1)
  table2 <- getNGramTable(df, 2)
  table3 <- getNGramTable(df, 3)
  g1 <- generate1GramNext(table1, table2, skipThreshold = 0)
  g2 <- generate2GramNext(table2, table3, skipThreshold = 0)
  fm <- getFrequencyMatrix(c(g1, g2))
  ct <- getCorrelationTable(fm)
  expect_equal(as_tibble(ct[1:3,]), 
               tibble(
                 w1 = c("'ll", "'m", "a"),
                 w2 = c("meet", "don't", "skool"),
                 prob = c(0.5313194, 1, 1)),
               tolerance=1e-7
  )
})    

test_that("divideByKMeans", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  df <- getTokenFrame(lines)
  coverage <- 0.7
  table1 <- getNGramTable(df, 1, coverage = coverage)
  table2 <- getNGramTable(df, 2, coverage = coverage)
  table3 <- getNGramTable(df, 3, coverage = coverage)
  table4 <- getNGramTable(df, 4, coverage = coverage)
  skipThreshold <- 2
  g1 <- generate1GramNext(table1, table2, skipThreshold = skipThreshold)
  g2 <- generate2GramNext(table2, table3, skipThreshold = skipThreshold)
  g3 <- generate3GramNext(table3, table4, skipThreshold = skipThreshold)
  fm <- getFrequencyMatrix(c(g1, g2, g3))
  
  groups <- divideByKMeans(fm, numMembersInGroup = 4)
  expect_equal(nrow(groups), 5) 
  expect_equal(unlist(groups$members[1]), 
               c("cali", "DC", "first", "get", "gorgeous", "no", "perfect", 
                 "Played", "the", "THIS", "Ughh", "work", "you"))
  expect_equal(unlist(groups$members[2]), c("game", "Go"))
  expect_equal(unlist(groups$members[3]), c("'m", "don't"))
  expect_equal(unlist(groups$members[4]), c("a", "skool"))
  expect_equal(unlist(groups$members[5]), c("know", "smile"))
})   

test_that("divideByKMeansForSmallerSize", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 100)
  df <- getTokenFrame(lines)
  coverage <- 0.7
  table1 <- getNGramTable(df, 1, coverage = coverage)
  table2 <- getNGramTable(df, 2, coverage = coverage)
  table3 <- getNGramTable(df, 3, coverage = coverage)
  table4 <- getNGramTable(df, 4, coverage = coverage)
  skipThreshold <- 2
  g1 <- generate1GramNext(table1, table2, skipThreshold = skipThreshold)
  g2 <- generate2GramNext(table2, table3, skipThreshold = skipThreshold)
  g3 <- generate3GramNext(table3, table4, skipThreshold = skipThreshold)
  fm <- getFrequencyMatrix(c(g1, g2, g3))
  
  groups1 <- divideByKMeansForSmallerSize(fm, drop=FALSE)
  expect_equal(sum(sapply(groups1$members, length)), nrow(fm))
  groups2 <- divideByKMeansForSmallerSize(fm, drop=TRUE)
  expect_equal(unlist(groups2$members[2]), c("'ll", "will"))
})     


test_that("divideByKMeansForSmallerSize", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 100)
  df <- getTokenFrame(lines)
  coverage <- 0.7
  table1 <- getNGramTable(df, 1, coverage = coverage)
  table2 <- getNGramTable(df, 2, coverage = coverage)
  table3 <- getNGramTable(df, 3, coverage = coverage)
  table4 <- getNGramTable(df, 4, coverage = coverage)
  skipThreshold <- 2
  g1 <- generate1GramNext(table1, table2, skipThreshold = skipThreshold)
  g2 <- generate2GramNext(table2, table3, skipThreshold = skipThreshold)
  g3 <- generate3GramNext(table3, table4, skipThreshold = skipThreshold)
  fm <- getFrequencyMatrix(c(g1, g2, g3))
  
  groups <- divideByKMeansForSmallerSize(fm, drop=TRUE)
  ct <- getCorrelationTableByGroups(fm, groups)
  expect_equal(nrow(ct), 103)
  expect_equal(as_tibble(ct[67,]),
               tibble(w1 = c("her"), w2 = c("its"), prob = c(1)))
})  
