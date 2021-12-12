d <- readLines('~/Desktop/Advent2021/data/day11.txt')
d <- strsplit(d, '')
d <- t(matrix(nrow=length(d[[1]]), as.numeric(unlist(d))))
origd <- d

K <- matrix(nrow=3, ncol=3, 1)
K[2,2] <- 0

conv2d <- function(M, K) {
  # Convolve 2D matrix M with square kernel K, with 0's for wraps
  w <- (nrow(K)-1)/2
  m <- matrix(nrow=nrow(M)+2*w, ncol=ncol(M)+2*w,0)
  n <- m
  m[1+(1:nrow(M)), 1+(1:ncol(M))] <- M
  
  w <- (nrow(K)-1)/2
  for (i in (1+w):(ncol(m)-w)   ) 
    for (j in (1+w):(nrow(m)-w) ) {
      n[i,j] <- sum(m[i+(-w:w), j+(-w:w)] * K)
    }
  
  n[1+(1:nrow(M)), 1+(1:ncol(M))]  
}

# Part 1

flashes <- 0
for (i in 1:100) {
  # First, the energy level of each octopus increases by 1.
  d <- d + 1
  # Then, any octopus with an energy level greater than 9 flashes.
  f <- d > 9
  flashed <- f
  
  while (sum(f) > 0) {
    # This increases the energy level of all adjacent octopuses by 1
    d <- d + conv2d(f, K)  
    # If this causes an octopus to have an energy level greater than 9, it also flashes
    f <- (d > 9) * !flashed
    flashed <- (flashed + f) > 0
    d[flashed] <- 0  
  }
  flashes <- flashes + sum(flashed)
}  
flashes
  


# Part 2
# What is the first step during which all octopuses flash?
d <- origd
flashed <- d * 0
i <- 0
while (sum(flashed) < nrow(d) * ncol(d)) {
  i <- i + 1
  # First, the energy level of each octopus increases by 1.
  d <- d + 1
  # Then, any octopus with an energy level greater than 9 flashes.
  f <- d > 9
  flashed <- f
  
  while (sum(f) > 0) {
    # This increases the energy level of all adjacent octopuses by 1
    d <- d + conv2d(f, K)  
    # If this causes an octopus to have an energy level greater than 9, it also flashes
    f <- (d > 9) * !flashed
    flashed <- (flashed + f) > 0
    d[flashed] <- 0  
  }
}
i

