library(stringi) # For stri_split_regex

totokens <- function(line) {
  line <- gsub("’", "'", line)
  
 
  # https://stringi.gagolewski.com/rapi/about_search_regex.html
  # This is after than str_split
  # We need to repeat  ?<= and ?= to seperate punctuations
  tokens <- stri_split_regex(line, "([ ]|(?='m|'re|'s|'ll|'d)|(?<=[\\?\\.,!])|(?=[\\?\\.,!]))", simplify = TRUE)
  tokens <- as.vector(tokens)
  tokens <- tokens[tokens != ""]
  return (tokens)
}

library(data.table) # For rbindlist
getTokenFrame <- function(lines) {
  allResults <- list()
  for (i in 1:length(lines)) {
    curLine <- lines[i]
    tokens <- totokens(curLine)
    allResults[[i]] <- list(a = curLine, b = list(tokens))
  }
  
  # https://www.rdocumentation.org/packages/data.table/versions/1.14.0/topics/rbindlist
  df <- rbindlist(allResults) # This is much faster than bind_rows
  colnames(df) <- c("line", "tokens")
  return (df)
}