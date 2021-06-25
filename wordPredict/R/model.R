getFrequencyMatrix <- function(generators) {
  l <- list()
  for (g in generators) {  
    loop(for (x in g) {
      l[[length(l)+1]] <- x
    })
  }
  tmp <- rbindlist(l, fill = TRUE)
  tmp[is.na(tmp)] <- 0
  
  # https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
  tmp2 <- aggregate(. ~ word, tmp, sum)
  return (tmp2)
}

getCorrelationTable <- function(curFm, absThreshold = 0.4) {
  if (nrow(curFm) == 0) {
    stop("row number is zero")
  }
  
  # If we only have 1 record, we need to handle it specially since we cannot take cor of it
  if (ncol(curFm) == 2) {
    words <- curFm$word
    l <- list()
    for (i in 1:(length(words)-1)) {
      for (k in (i+1):length(words)) {
        l[[length(l)+1]] <- list(w1 = words[i], w2 = words[k], prob = 0.5)
      } 
    }
    return (as.data.frame(rbindlist(l)))
  }
  rownames(curFm) <- curFm[,1]
  curFm <- curFm[,-1]
  curFm <- t(curFm)
  corFm <- cor(curFm)
  
  rnames <- rownames(corFm)
  cnames <- colnames(corFm)
  l <- list()
  for (i in 1:nrow(corFm)) {
    for (k in i:ncol(corFm)) {
      if (i != k & abs(corFm[i, k]) > absThreshold) {
        # message(rownames(df)[i], " | ", colnames(df)[k], " | with prob ", df[i, k])
        l[[length(l)+1]] <- list(w1 = rnames[i], w2 = cnames[k], prob = corFm[i, k])
      }
    }
  }
  return (as.data.frame(rbindlist(l)))
}

library(factoextra)
library(Matrix)
# Since if the rank of the matrix is too few, we cannot have enough number of groups 
divideByKMeans <- function(fm, nGroups = 20, numMembersInGroup = 12, random_state = 1) {
  a <- fm
  rownames(a) <- a[,1]
  wordLength <- nrow(a)
  a <- a[,-1]
  rankA <- rankMatrix(a)[1]
  # rankA <- rankMatrix(as.matrix(a), method = "qr")[1] # many other warning messages
  clusterSize <- min(nGroups, max(floor(wordLength/numMembersInGroup), 1), rankA)
  set.seed(random_state)
  km <- kmeans(a, clusterSize)
  l <- list()
  for (i in 1:nrow(km$centers)) {
    gp <- which(km$cluster == i)
    nms <- c()
    for (i in 1:length(gp)) {
      nms <- c(nms, names(gp[i]))
    }
    l[[length(l)+1]] <- list(members = list(nms))
  }
  
  return (as.data.frame(rbindlist(l)))
}

divideByKMeansForSmallerSizeInner <- function(fm, maxMembersInGroup = 8, maxDepth = Inf, drop = FALSE, depth = 1) {
  l <- list()
  curFM <- fm
  
  groups <- divideByKMeans(curFM) 
  if (nrow(groups) == 1 || depth >= maxDepth) {
    if (nrow(groups) == 1 & drop) {
      return (NULL)
    }
    for (i in 1:nrow(groups)) {
      l[[length(l)+1]] <- list(members = groups[i,])
    }
    return (l)
  }
  for (i in 1:nrow(groups)) {
    curGroup <- groups[i,]
    curWords <- unlist(curGroup)
    if (length(curWords) <= maxMembersInGroup) {
      l[[length(l)+1]] <- list(members = curGroup)
    } else {
      leftFM <- curFM %>% filter(word %in% curWords)
      res <- divideByKMeansForSmallerSizeInner(leftFM, maxMembersInGroup = maxMembersInGroup, maxDepth = maxDepth, drop = drop, depth = depth+1)
      l <- c(l, res)
    }
  }
  return (l)
}

divideByKMeansForSmallerSize <- function(fm, maxMembersInGroup = 8, maxDepth = Inf, drop = FALSE) {
  inner <- divideByKMeansForSmallerSizeInner(fm, maxMembersInGroup = maxMembersInGroup, maxDepth = maxDepth, drop = drop) 
  return (as.data.frame(rbindlist(inner)))
}

# TODO: Add parameter to limit member size?
# TODO: Only keep memebres of correlation > 0.5?
getCorrelationTableByGoupMembers <- function(fm, members) {
  curFM <- fm %>% 
    filter((word %in% members))
  if (nrow(curFM) == 1) {
    return (NULL)
    # return (data.frame(w1 = c(curFM$word), w2 ))
  }
  
  zz <- apply(curFM, 2, function(x) x == 0)
  zz2 <- apply(zz, 2, all)
  zz3 <- !zz2
  curFM <- curFM[, zz3]
  
  return (getCorrelationTable(curFM))
}

getCorrelationTableByGroups <- function(fm, groups) {
  l <- list()
  for (i in 1:nrow(groups)) {
    members <- unlist(groups[i,])
    l[[length(l) + 1]] <- getCorrelationTableByGoupMembers(fm, members)
  }
  
  return (as.data.frame(rbindlist(l)))
}
