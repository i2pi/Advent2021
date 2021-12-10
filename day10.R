lines <- readLines('~/Desktop/Advent2021/data/day10.txt')

checkString <- function (str) {
  d <- unlist(strsplit(str,""))
  s <- c()
  valid <- rep(NA, length(d))
  for (i in 1:length(d)) {
    C <- d[i]
    if (C %in% c('(','{','[','<')) {
      s <- c(s, C) 
    } else {
      P <- s[length(s)]
      valid[i] <- switch(C,
                         ")" = {P == '('},
                         "}" = {P == '{'},
                         "]" = {P == '['},
                         ">" = {P == '<'})
      if (valid[i]) s <- s[1:(length(s)-1)]
    }
  }
  return(list(valid=valid, open=s))
}

# Part 1
score <- 0
for (i in 1:length(lines)) {
  valid <- checkString(lines[i])$valid
  if (!is.na(any(!valid))) {
    char <- unlist(strsplit(lines[i],''))[min(which(!valid))]
    score <- score + switch(char,
                            ')' = 3, ']' = 57, '}' = 1197, '>' = 25137)
  }
}
score


# Part 2
scores <- NA
for (i in 1:length(lines)) {
  r <- checkString(lines[i])
  valid <- r$valid
  s <- r$open
  if (is.na(any(!valid))) {
    points <- sapply(rev(s), \(x) which(c('(', '[', '{', '<') %in% x))
    new_score <- 0
    for (j in points) new_score <- new_score*5 + j
    scores <- c(scores, new_score)
  }
}
median(scores, na.rm=T)





