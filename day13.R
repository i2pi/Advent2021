d <- read.csv('~/Desktop/Advent2021/data/day13.txt', header=F)
colnames(d) <- c('x','y')
folds <- d$x[is.na(d$y)]
folds<-strsplit(substr(folds, 12, 100), '=')
d <- d[!is.na(d$y),]
d <- data.frame(apply(d, 2, as.numeric) + 1)

dots <- matrix(nrow=max(d$y), ncol=max(d$x), 0)
for (i in 1:nrow(d)) dots[d$y[i], d$x[i]] <- 1

doFold <- function(dots, fold) {
  at <- as.numeric(fold[2]) + 1
  if (fold[1] == 'y') {
    r <- dots[1:(at-1),] + apply(dots[(at+1):nrow(dots),], 2, rev)
  } else {
    r <- dots[,1:(at-1)] + t(apply(dots[,(at+1):ncol(dots)], 1, rev))
  }
  r
}

# Part 1
sum(doFold(dots, folds[[1]]) > 0)

# Part 2
for (fold in folds) dots <- doFold(dots, fold)
dots <- apply(dots, 2, rev)
image(t(dots>0))
