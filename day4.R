d <- readLines('~/Desktop/Advent2021/day4.txt')
width <- 5
numbers <- as.numeric(unlist(strsplit(d[1], ',')))
b <- as.numeric(unlist(strsplit(d[3:length(d)], ' +')))
b <- t(matrix(nrow=width, b[!is.na(b)]))
boards <- vector("list", nrow(b) / width)

for(i in seq(1, nrow(b), by=width)) {
  boards[[floor(i / width) + 1]] <- b[i:(i+width-1),]
}



# Part 1
matches <- lapply(boards, function(x) x<0)
for (i in numbers) {
  m <- lapply(boards, function(x) x == i)
  for(j in 1:length(matches)) matches[[j]] = matches[[j]] | m[[j]]
  winners <- unlist(lapply(matches, function(m) max(apply(m, 1, sum) == width, apply(m, 2, sum) == width)))
  if (sum(winners) > 0) {
    j <- which(winners == TRUE)
    score <- sum(boards[[j]] * (!matches[[j]])) * i
    break
  }
}

# Part 2

winTime <- rep(length(numbers), length(boards))
matches <- lapply(boards, function(x) x<0)
for (r in 1:length(numbers)) {
  i <- numbers[r]
  m <- lapply(boards, function(x) x == i)
  for(j in 1:length(matches)) matches[[j]] = matches[[j]] | m[[j]]
  winners <- which(unlist(lapply(matches, function(m) max(apply(m, 1, sum) == width, apply(m, 2, sum) == width)))==1)
  winTime[winners[winTime[winners] > r]] <- r
}
worstBoard <- which(winTime == max(winTime))
matches <- lapply(boards, function(x) x<0)
for (i in numbers) {
  m <- lapply(boards, function(x) x == i)
  for(j in 1:length(matches)) matches[[j]] = matches[[j]] | m[[j]]
  winners <- which(unlist(lapply(matches, function(m) max(apply(m, 1, sum) == width, apply(m, 2, sum) == width)))==1)
  if (any(winners == worstBoard)) {
    score <- sum(boards[[worstBoard]] * (!matches[[worstBoard]])) * i
    break
  }
}

