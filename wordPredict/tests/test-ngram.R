source("../R/ngram.R", chdir = TRUE)

test_that("getNGram", {
  # start_time <- Sys.time()
  # lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  # end_time <- Sys.time()
  
  sampleNGram <- getNGram(c("Hello", "World"), c("w1"), c(".", "!"))
  expect_true(is.data.frame(sampleNGram))
  expect_false(is.data.table(sampleNGram))
  expect_identical( sampleNGram, 
                    data.frame(
                      w1 = c("Hello", "World"))
                    )

  sampleNGram2 <- getNGram(c("You", "are", "good", "!"), c("w1", "w2"), c(".", "!"))
  expect_identical( sampleNGram2, 
                    data.frame(
                      w1 = c("You", "are"),
                      w2 = c("are", "good"))
  )
  
  l <- list()
  l[[1]] <- sampleNGram
  l[[2]] <- sampleNGram
  res <- as.data.frame(rbindlist(l))
  expect_identical( res, 
                    data.frame(
                      w1 = c("Hello", "World", "Hello", "World"))
  )
})  

test_that("getNGram performance", {
  skip("Cause too much time")
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 900)
  
  timeLimits <- c(0.5, 0.6, 0.7)
  
  for (i in 1:3) {
    N <- i
    colnms <- c()
    for (k in 1:N) {
      colnms <- c(colnms, paste('w', k, sep = ""))
    }
    start_time <- Sys.time()
    df <- getTokenFrame(lines)
    excludes <- c(",", "!", "?", ".", "", " ", "-", "\"", "'")
    for (k in 1:nrow(df)) {
      now <- getNGram(unlist(df[k,]$tokens), colnms, excludes)
    }
    end_time <- Sys.time()
    expect_lt(end_time - start_time, timeLimits[i])
  }
  
})  

test_that("getNGramTable 1", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 2)
  df <- getTokenFrame(lines)
  tbl <- getNGramTable(df, 1)
  # https://testthat.r-lib.org/reference/equality-expectations.html
  expect_equal( data.frame(head(tbl, 5))[,1:2], 
                    data.frame(
                      w1 = c("you", "'ll", "for", "way", "and"),
                      count = c(5, 2, 2, 2, 1))
  )
  expect_identical(sum(tbl$prob), 1)
})  

test_that("getNGramTable 3", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 2)
  df <- getTokenFrame(lines)
  tbl <- getNGramTable(df, 3)
  # https://testthat.r-lib.org/reference/equality-expectations.html
  expect_equal( data.frame(head(tbl, 5))[,1:4], 
                data.frame(
                  w1 = c("'ll", "and", "be", "beat", "Btw"),
                  w2 = c("smile", "you", "in", "more", "thanks"),
                  w3 = c("for", "'ll", "DC", "rapidly", "for"),
                  count = c(1, 1, 1, 1, 1))
  )
  expect_identical(sum(tbl$prob), 1)
})  

test_that("getNGramTable performance", {
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 500)
  df <- getTokenFrame(lines)
  start_time <- Sys.time()
  tbl <- getNGramTable(df, 3)
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 0.4)
  expect_equal(nrow(tbl), 4417)
})  