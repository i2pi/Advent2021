d <- readLines('~/Desktop/Advent2021/day3.txt')
d <- t(data.frame(lapply(strsplit(d, ''), function(a) a == "1")))
rownames(d) <- 1:nrow(d)

# Part 1

mostCommonBits <- apply(d, 2, function(x) sum(x) > nrow(d)/2)
pow2 <- rev(2^(0:(ncol(d)-1)))
gamma <- sum(mostCommonBits * pow2)
epsilon <- sum((!mostCommonBits) * pow2)

gamma * epsilon

# Part 2

mostCommonBits <- function(d) apply(d, 2, function(x) sum(x) >= nrow(d)/2)

a <- d
for (i in 1:ncol(d)) {
  a <- a[a[,i] == mostCommonBits(a)[i],]
  if (is.null(nrow(a))) break;
}
ogr <- sum(a * pow2)

a <- d
for (i in 1:ncol(d)) {
  a <- a[a[,i] == !mostCommonBits(a)[i],]
  if (is.null(nrow(a))) break;
}
csr <- sum(a * pow2)

csr * ogr
