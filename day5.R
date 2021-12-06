d <- readLines('~/Desktop/Advent2021/data/day5.txt')
d <- sub(' -> ', ',', d)
lines <- data.frame(t(matrix(nrow=4,as.numeric(unlist(strsplit(d, ','))))))
colnames(lines) <- c('x1', 'y1', 'x2', 'y2')
lines <- lines + 1
hv <- (lines$x1 == lines$x2) | (lines$y1 == lines$y2)
vents <- matrix(ncol=max(lines$x1, lines$x2), nrow=max(lines$y1, lines$y2), 0)

# Part 1

for (i in which(hv)) {
  l <- as.numeric(lines[i,])
  vents[l[2]:l[4], l[1]:l[3]] <- vents[l[2]:l[4], l[1]:l[3]] + 1
}
sum(vents  >= 2)

# Part 2

for (i in which(!hv)) {
  l <- as.numeric(lines[i,])
  x <- l[1]:l[3]
  y <- l[2]:l[4]
  for (j in 1:length(x)) {
    vents[y[j],x[j]] <- vents[y[j],x[j]] + 1
  }
}
sum(vents  >= 2)
