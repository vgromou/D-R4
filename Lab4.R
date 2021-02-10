library(stringr)

# Patterns
email <- regex("[[:alpha:]+[:punct:]]+@[a-z]+[\\.]{1}[a-z]+", ingore_case = TRUE) ## email pattern
urlPat <- regex("((http://)|(https://))+[[:alnum:]+[:punct:]-[)(]]+(?<!\\.-?)") ## URL pattern
upTwo <- regex("[A-Z]{2}+[a-z]+") ## pattern for words with 2 upper-case first letters only

# Finds all lines with pattern. Needs a source to .txt text, returns data.frame with lines and ordinal number of them.  
findLines = function(source, pattern = email){
  text = readLines(source)
  n = c()
  line = c()
  for(i in 1: length(text)){
    if(str_detect(text[i], pattern)){
      n = c(n, i)
      line = c(line, text[i])
    }
  }
  lines = data.frame(n, line)
  return(lines)
}

# Finds words by pattern (not lines with them). Needs a source to .txt text, returns an array with info you need (string).
listInfo = function(source, pattern = email){
  lines = findLines(source, pattern)
  words <- str_extract_all(lines$line, pattern, simplify = TRUE)
  res <- words[,1]
  return(res)
}
