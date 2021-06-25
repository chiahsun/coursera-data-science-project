library(coro)

addDProbNextAndRemoveOnCoverage <- function(table, words, coverage) {
  N = length(words)
  nxtCol = paste("w", N+1, sep="")
  table <- table %>%
    mutate(dprob = prob , nxt = .data[[nxtCol]]) 
  total <- sum(table$dprob)
  table$dprob <- table$dprob/total
  
  # Remove based on coverage
  if (coverage < 1 & nrow(table) > 0) {
    for (i in 1:nrow(table)) {
      if (sum(table$dprob[1:i]) >= coverage) {
        table = table[1:i, ]
        break
      }
    } 
  }
  
  return (table)
}

predict1GramNext <- function(table, words, coverage = 0.7) {
  res <- table[table$w1 == words[1],]
  return (addDProbNextAndRemoveOnCoverage(res, words, coverage))
}

predict2GramNext <- function(table, words, coverage = 0.7) {
  res <- table[table$w1 == words[1] & table$w2 == words[2],]
  return (addDProbNextAndRemoveOnCoverage(res, words, coverage))
}

predict3GramNext <- function(table, words, coverage = 0.7) {
  res <- table[table$w1 == words[1] & table$w2 == words[2] & table$w3 == words[3],]
  return (addDProbNextAndRemoveOnCoverage(res, words, coverage))
}

generate1GramNext <- generator(function(tbl1, tbl2, topN = 15, skipThreshold = 30, coverage = 0.7) {
  for (i in 1:(min(topN, nrow(tbl1)))) {
    cur <- tbl1[i,]
    followed <- predict1GramNext(tbl2, pre <- c(cur$w1), coverage)
    feature <- paste("1gram:next", cur$w1)
    if (nrow(followed) >= skipThreshold) {
      for (k in 1:nrow(followed)) {
        cur <- list(word = followed$w2[k])
        cur[feature] <- followed$dprob[k]
        yield(cur)
      }
    }
  }
})

generate2GramNext <- generator(function(tbl2, tbl3, topN = 15, skipThreshold = 30, coverage = 0.7) {
  for (i in 1:(min(topN, nrow(tbl2)))) {
    cur <- tbl2[i,]
    followed <- predict2GramNext(tbl3, pre <- c(cur$w1, cur$w2), coverage)
    feature <- paste("2gram:next", paste(cur$w1, cur$w2, sep = "."))
    if (nrow(followed) > 0 & nrow(followed) >= skipThreshold) {
      for (k in 1:nrow(followed)) {
        cur <- list(word = followed$w3[k])
        cur[feature] <- followed$dprob[k]
        yield(cur)
      }
    }
  }
})

generate3GramNext <- generator(function(tbl3, tbl4, topN = 15, skipThreshold = 30, coverage = 0.7) {
  for (i in 1:(min(topN, nrow(tbl3)))) {
    cur <- tbl3[i,]
    followed <- predict3GramNext(tbl4, pre <- c(cur$w1, cur$w2, cur$w3), coverage)
    feature <- paste("3gram:next", paste(cur$w1, cur$w2, cur$w3, sep = "."))
    if (nrow(followed) > 0 & nrow(followed) >= skipThreshold) {
      for (k in 1:nrow(followed)) {
        cur <- list(word = followed$w4[k])
        cur[feature] <- followed$dprob[k]
        yield(cur)
      }
    }
  }
})

getPredictFuncInner <- function(table2, table3, table4) {
  predict <- function(words) {
    N <- length(words)
    if (N == 1) {
      return (predict1GramNext(table2, words))
    } else if (N == 2) {
      return (predict2GramNext(table3, words))
    } else if (N == 3) {
      return (predict3GramNext(table4, words))
    } else {
      stop("Not supported for ", N)
    }
  }
  return (predict)
}

getPredictFunc <- function(train, coverage = 0.7) {
  df <- getTokenFrame(train)
  table2 <- getNGramTable(df, 2, coverage = coverage)
  table3 <- getNGramTable(df, 3, coverage = coverage)
  table4 <- getNGramTable(df, 4, coverage = coverage)
  return (getPredictFuncInner(table2, table3, table4))
}

getN <- function(table) {
  return (sum(grepl("w[0-9]+", colnames(table))))
}

getCandidates <- function(curWord, ct) {
  candidates <- list()
  candidates[[length(candidates)+1]] <- list(w = curWord, cor = 1)
  
  df <- ct[ct$w2 == curWord,]
  for (i in seq_len(nrow(df))) {
    cur <- df[i,]
    candidates[[length(candidates)+1]] <- list(w = cur$w1, cor = cur$prob)
  }
  df <- ct[ct$w1 == curWord,]
  for (i in seq_len(nrow(df))) {
    cur <- df[i,]
    candidates[[length(candidates)+1]] <- list(w = cur$w2, cor = cur$prob)
  }
  
  return (candidates)
}

getPredictFuzzyFuncInner <- function(table2, table3, table4, ct) {
  predict <- getPredictFuncInner(table2, table3, table4)
  
  predictFuzzy <- function(words) {
    N <- length(words)
    
    w1Candidates <- list()
    w2Candidates <- list()
    w3Candidates <- list()
    
    if (N >= 1) {
      w1Candidates = getCandidates(words[1], ct)
    }
    if (N >= 2) {
      w2Candidates = getCandidates(words[2], ct)
    }
    if (N >= 3) {
      w3Candidates = getCandidates(words[3], ct)
    }
    
    res <- NULL
    if (N == 1) {
      it <- ihasNext(product(w1Candidates=w1Candidates))
      while (hasNext(it)) {
        x <- nextElem(it)
        res <- rbind(res, predict(c(x$w1Candidates$w)) %>%
                       mutate(cor1 = x$w1Candidates$cor) %>%
                       mutate(sprob = prob * cor1))
      }
    } else if (N == 2) {
      it <- ihasNext(product(w1Candidates=w1Candidates, w2Candidates = w2Candidates))
      while (hasNext(it)) {
        x <- nextElem(it)
        res <- rbind(res, predict(c(x$w1Candidates$w, x$w2Candidates$w)) %>%
                       mutate(cor1 = x$w1Candidates$cor, cor2 = x$w2Candidates$cor) %>%
                       mutate(sprob = prob * cor1 * cor2))
      }
    } else if (N == 3) {
      it <- ihasNext(product(w1Candidates = w1Candidates, 
                             w2Candidates = w2Candidates, 
                             w3Candidates = w3Candidates))
      while (hasNext(it)) {
        x <- nextElem(it)
        res <- rbind(res, predict(c(x$w1Candidates$w, x$w2Candidates$w, x$w3Candidates$w)) %>%
                       mutate(cor1 = x$w1Candidates$cor, cor2 = x$w2Candidates$cor, cor3 = x$w3Candidates$cor) %>%
                       mutate(sprob = prob * cor1 * cor2 * cor3))
      }
    } else {
      stop("Not supported for ", N)
    }
    
    total <- sum(res$sprob)
    res$dprob <- res$sprob/total
    res <- arrange(res, desc(dprob))
    return (res)
  }
  
  return (predictFuzzy)
}

getPredictFuzzyFunc <- function(train, coverage = 0.7, skipThreshold = 0, 
                                coverageForFirstGenerator = 0.9,
                                coverageForGeneratorDecay = 0.7,
                                kmeansMaxMembersInGroup = 20,
                                kmeansMaxDepth = Inf,
                                kmeansDrop = TRUE,
                                writePath = NULL) {
  df <- getTokenFrame(train)
  table1 <- getNGramTable(df, 1, coverage = coverage)
  library(readr)
  # https://readr.tidyverse.org/reference/write_delim.html
  if (!is.null(writePath)) {
    write_csv(table1, paste(writePath, "table1.bz2", sep = ""))
  }
  table2 <- getNGramTable(df, 2, coverage = coverage)
  if (!is.null(writePath)) {
    write_csv(table2, paste(writePath, "table2.bz2", sep = ""))
  }
  table3 <- getNGramTable(df, 3, coverage = coverage)
  if (!is.null(writePath)) {
    write_csv(table3, paste(writePath, "table3.bz2", sep = ""))
  }
  table4 <- getNGramTable(df, 4, coverage = coverage)
  if (!is.null(writePath)) {
    write_csv(table4, paste(writePath, "table4.bz2", sep = ""))
  }
  
  g1 <- generate1GramNext(table1, table2, skipThreshold = skipThreshold, coverage = coverageForFirstGenerator)
  g2 <- generate2GramNext(table2, table3, skipThreshold = skipThreshold, coverage = coverageForFirstGenerator * coverageForGeneratorDecay)
  g3 <- generate3GramNext(table3, table4, skipThreshold = skipThreshold, coverage = coverageForFirstGenerator * coverageForGeneratorDecay^2)
  fm <- getFrequencyMatrix(c(g1, g2, g3))
  groups <- divideByKMeansForSmallerSize(fm, maxMembersInGroup = kmeansMaxMembersInGroup, 
                                         maxDepth = kmeansMaxDepth, drop = kmeansDrop)
  library(purrr)
  if (!is.null(writePath)) { # Not used but to check data
    write_csv(groups %>% mutate(members = map_chr(members, toString)), 
              paste(writePath, "groups.tsv", sep = ""))
  }
  ct <- getCorrelationTableByGroups(fm, groups)
  if (!is.null(writePath)) {
    write_csv(ct, paste(writePath, "ct.bz2", sep = ""))
  }
  
  return (getPredictFuzzyFuncInner(table2, table3, table4, ct))
}

# TODO: get correlation table for candidates

library(itertools)
testProduct <- function() {
  it <- ihasNext(product(a=1:3, b=1:2))
  # it <- product(a=1:3, b=1:2)
  while (hasNext(it)) {
    x <- nextElem(it)
    cat(sprintf('a = %d, b = %d\n', x$a, x$b))
  }
  
  l <- list()
  l[[length(l)+1]] <- list(w1 = "the", cor = 0.9)
  l[[length(l)+1]] <- list(w1 = "at", cor = 0.5)
  it <- ihasNext(product(a=l, b=1:2))
  # it <- product(a=1:3, b=1:2)
  while (hasNext(it)) {
    x <- nextElem(it)
    # message("a: ", x$a, " b: ", x$b)
    message("w1: ", x$a$w1, " cor: ", x$a$cor, " b: ", x$b)
    # cat(sprintf('a = %d, b = %d\n', x$a, x$b))
  }
}

testDataFrameIter <- function() {
  df <- data.frame(a = 1:3, b = c("a", "b", "c"))
  for (r in df[df$a>=2,]) {
    print(r)
    print(typeof(r))
    # print(r$a) 
    # print(r$b)
  }
  # https://stackoverflow.com/questions/1699046/for-each-row-in-an-r-dataframe
  apply(df, 1, function(cur) {
    # print(cur)
    #print(cur$a)
    print(cur['a'])
    #print(cur$b)
  })
  
  l <- list()
  by(df, seq_len(nrow(df)), function(cur) {
    l[[length(l)+1]] <- list(a = cur$a) # l outside is not modified
    # print(cur$a)
  })
}
