library(dplyr)

readFiles <- function(filenames, numLine = Inf) {
  lines <- c()
  n <- numLine
  if (is.infinite(numLine)) {
    n <- -1
  }
  for (filename in filenames) {
    con <- file(filename, "r")
    
    # Warning (test-reader.R:15:3): readFiles and getSampleLines
    # line 268547 appears to contain an embedded nul
    # https://stackoverflow.com/questions/24734911/warning-message-line-appears-to-contain-embedded-nulls
    curLines <- readLines(con, n = n, skipNul = TRUE)
    close(con)
    lines <- c(lines, curLines)
  }
  return (lines)
}

getSampleLines <- function(lines) {
  lineCounts <- length(lines)
  readFunc <- function(lineToRead = Inf, random_state = -1, random = TRUE) {
    if (is.infinite(lineToRead)) {
      lineToRead = lineCounts
    }
    
    inSample <- 1:lineToRead
    if (random) {
      if (random_state >= 0) {
        set.seed(random_state) 
      }
      inSample <- sample(lineCounts, lineToRead)
    }
    return (lines[inSample])
  }
  return (readFunc)
}