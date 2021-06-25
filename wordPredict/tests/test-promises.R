library(promises)

source("../R/predict.R", chdir = TRUE)

test_that("promise chaining demo", {
  skip("demo")
  # https://cran.r-project.org/web/packages/promises/vignettes/overview.html
  # https://rstudio.github.io/promises/articles/overview.html
  p1 <- promise(function(resolve, reject) {
    message("resolve 1")
    resolve(list(a = 1))
  })
  
  p2 <- p1 %>%
    then(function(l) {
      c(l, b = 2)
    })
  
  p3 <- p2 %>%
    then(function(l) {
      c(l, c = 3)
  })
  
  then(p3,
       onFulfilled = function(x) { 
         print("onFulfilled: ")
         print(x) 
       },
       onRejected = ~message("Failure")
  )
})  

test_that("read model by promise ", {
  skip("demo")
  print("p1 +++")
  p1 <- promise(function(resolve, reject) {
    start_time <- Sys.time()
    predictFuzzy <- getPredictFuzzyFuncInner(
      read_csv("./data/processed/table2.bz2"),
      read_csv("./data/processed/table3.bz2"),
      read_csv("./data/processed/table4.bz2"),
      read_csv("./data/processed/ct.bz2")
    )
    end_time <- Sys.time()
    message("Setup time: ", end_time - start_time)
    ~resolve(1)
  })
  print("p1 ---")
  
  then(p1,
       onFulfilled = ~message("Success"),
       onRejected = ~message("Failure")
  )
  # expect_identical(sum(res$dprob), 1)
})  