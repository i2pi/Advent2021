d <- readLines('~/Desktop/Advent2021/data/day9.txt')
d <- t(matrix(nrow=nchar(d[1]),as.numeric(unlist(strsplit(d, '')))))


# Part 1
fd <- \(x) c(TRUE, diff(x)<0)
rd <- \(x) c(rev(diff(rev(x)))<0,TRUE)
bd <- \(x) fd(x) & rd(x)
low <- t(apply(d, 1, bd)) & apply(d, 2, bd)
sum(d[low] + 1) 

# Part 2

# This is how the question should be defined :P

# bd <- \(x) fd(x) | rd(x)
# low <- t(apply(d, 1, bd)) | apply(d, 2, bd)

# But instead they think things can flow uphill :/
m <- d
m[] <- 1
m[d==9] <- 0

isElementInBasin <- function(i, j) {
  if ((i < 1) | (j < 1) | (i > nrow(m)) | (j > ncol(m))) return (0)
  if (!m[i,j])  return(0)
  m[i,j] <<- 0
  return (1 + isElementInBasin(i+1,j) + isElementInBasin(i,j+1) +
            isElementInBasin(i-1,j) + isElementInBasin(i,j-1) )
            
}
basin <- d * 0
for (i in 1:nrow(m)) for (j in 1:ncol(m)) basin[i,j] <- isElementInBasin(i, j)
top <- sort(basin[basin > 0], decreasing = TRUE)[1:3]
top[1] * top[2] * top[3]
