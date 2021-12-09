d <- readLines('~/Desktop/Advent2021/day8.txt')
d <- unlist(strsplit(gsub('\\|', '', d), ' '))
d<-d[d != ""]

# Part 1
n<-t(matrix(nrow=14, sapply(d, nchar)))
sum(n[,11:14] %in% c(2, 4, 3, 7))

# Part 2
lettersToMatrix <- function(letters) t(sapply(strsplit(letters,''), \(y) c('a', 'b', 'c', 'd', 'e', 'f', 'g') %in% y)) * 1

digits <- c('abcefg', 'cf', 'acdeg','acdfg','bcdf','abdfg','abdefg','acf', 'abcdefg','abcdfg')
digits <- lettersToMatrix(digits)

x<-t(matrix(nrow=14, d))

total <- 0
for (i in 1:nrow(x)) {
  y <- lettersToMatrix(x[i,1:10])
  
  map <- rep(NA, 10)
  map[apply(y, 1, sum) == 2] <- 1
  map[apply(y, 1, sum) == 4] <- 4
  map[apply(y, 1, sum) == 3] <- 7
  map[apply(y, 1, sum) == 7] <- 8
  
  # Find 3 
  z <- which(apply(y, 1, sum) == 5)
  map[z[which(apply(apply(y[z,], 1, function(x) x * y[which(map == 1),]), 2, sum) == 2)]] <- 3
  
  # Find 5
  z <- z[is.na(map[z])]
  map[z[apply(apply(y[z,], 1, function(x) x * y[which(map == 4),]), 2, sum)==3]] <- 5
  
  # Find 2
  z <- z[is.na(map[z])]
  map[z] <- 2
  
  # Find 6 
  z <- which(apply(y, 1, sum) == 6)
  map[z[which(apply(apply(y[z,], 1, function(x) x * y[which(map == 1),]), 2, sum) == 1)]] <- 6
  
  # Find 9
  z <- z[is.na(map[z])]
  map[z[which(apply(apply(y[z,], 1, function(x) x * y[which(map == 3),]), 2, sum) == 5)]] <- 9
  
  # And 0
  map[is.na(map)] <- 0
  
  total <- total + sum(sapply(11:14, \(j) map[which(apply(y, 1, \(r) all(r == lettersToMatrix(x[i,j]))))]) * 10^(3:0))
}

total