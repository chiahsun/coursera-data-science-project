# https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/
# TODO: Change data.frame to data.table
# TODO: Write performance test before change
# TODO: Change all tokens to lower case
getNGram <- function(tokens, colnms, excludes) {
  N <- length(colnms)
  l <- list()
  
  # We skip for line with tokens less than N
  if (length(tokens) >= N) {
    for (i in 1:(length(tokens)-N+1)) {
      segment <- tokens[i:(i+N-1)]
      if (any(segment %in% excludes)) {
        next
      }
      elem <- list()
      for (k in 1:length(colnms)) {
        elem[colnms[k]] <- segment[k]
      }
      l[[length(l)+1]] <- elem
    }   
  }
  res <- rbindlist(l)
  # return (res)
  return (as.data.frame(res)) # Change data.table to data.frame
}

getNGramTable <- function(df, N, excludes = c(",", "!", "?", ".", "", " ", "-", "\"", "'"), coverage = 1) {
  
  colnms <- c()
  for (i in 1:N) {
    colnms <- c(colnms, paste('w', i, sep = ""))
  }
  l <- list()
  for (i in 1:nrow(df)) {
    l[[length(l)+1]] <- getNGram(unlist(df[i,]$tokens), colnms, excludes)
  }
  
  res <- rbindlist(l)
  nGramTable <- res %>%
    group_by_at(colnms) %>%
    # https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
    summarize(count = n(), .groups = 'drop') %>% 
    # ungroup() %>%
    arrange(desc(count))
  
  total <- sum(nGramTable$count)
  nGramTable <- nGramTable %>%
    mutate(prob = count/total)
  
  # Remove based on coverage
  if (coverage < 1) {
    for (i in 1:nrow(nGramTable)) {
      if (sum(nGramTable$prob[1:i]) >= coverage) {
        nGramTable = nGramTable[1:i, ]
        break
      }
    } 
  }
  
  return (nGramTable)
}


rbindlistOnly <- function(numRow) {
  l <- list()
  for (i in 1:numRow) {
    l[[length(l)+1]] <- list(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9, j = 10)
  }
  # l <- as.data.frame(rbindlist(l))
  l <- rbindlist(l)
}

benchBind <- function() {
  library(purrr)
  library(microbenchmark)
  numRow <- 1000
  mbm <- microbenchmark("rbind" = { 
    res <- NULL
    for (i in 1:numRow) {
      # res <- rbind(res, list(a = 1, b = 2, c = 3, d = 4, e = 5))
      #res <- as.data.frame(rbind(res, set_names(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
      #                                          c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"))))
      res <- rbind(res, set_names(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                                c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")))
    }
  }, "rbindlist" = {
    rbindlistOnly(numRow)
  }, "rbindlist.to.data.frame" = {
    as.data.frame(rbindlistOnly(numRow))
  }
  )
  mbm
}

benchBind2 <- function() {
  library(purrr)
  library(microbenchmark)
  N <- 1000
  mbm <- microbenchmark("use data.table" = { 
    l <- list()
    for (i in 1:N) {
      l[[length(l)+1]] <- data.table(a = 1, b = 2, c = 3)
    }
    l <- rbindlist(l)
  }, "use list" = {
    l <- list()
    for (i in 1:N) {
      l[[length(l)+1]] <- list(a = 1, b = 2, c = 3)
    }
    l <- rbindlist(l)
  })
  mbm
}