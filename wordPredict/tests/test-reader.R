source("../R/reader.R", chdir = TRUE)

# testthat::test_dir("tests")
# testthat::auto_test(code_path = "./R", test_path = "./tests")

test_that("readFiles part of the file", {
  start_time <- Sys.time()
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"), 10)
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 0.02)
  expect_equal(length(lines), 10)
})  
  
test_that("readFiles whole file", {
  # https://rdrr.io/cran/testthat/man/skip.html
  skip("Cause too long time")
  
  start_time <- Sys.time()
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt"))
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 2.5)
  expect_equal(length(lines), 2360148)
  
  start_time <- Sys.time()
  lines <- readFiles(c("../data/raw/final/en_US/en_US.twitter.txt", "../data/raw/final/en_US/en_US.news.txt", "../data/raw/final/en_US/en_US.blogs.txt"))
  end_time <- Sys.time()
  expect_lt(end_time - start_time, 7.5)
  expect_equal(length(lines), 4269678)
})

test_that("getSampleLines", {
  sample <- getSampleLines(c(1, 2, 3, 4, 5))
  
  expect_equal(sample(lineToRead = 1, random = FALSE), 
               c(1))
  expect_equal(sample(lineToRead = 2, random = FALSE), 
               c(1, 2))
  expect_equal(sample(lineToRead = Inf, random = FALSE), 
               c(1, 2, 3, 4, 5))
  
  expect_equal(sample(lineToRead = 2, random_state = 1), 
               c(1, 4))
  expect_equal(sample(lineToRead = 4, random_state = 1), 
               c(1, 4, 3, 5))
  expect_equal(sample(lineToRead = Inf, random_state = 1), 
               c(1, 4, 3, 5, 2))
  expect_equal(sample(lineToRead = Inf, random_state = 2), 
               c(5, 3, 2, 4, 1))
})
  