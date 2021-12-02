d <- read.csv('~/Desktop/Advent2021/day2.txt', sep=' ', header=F)

# Part 1
colnames(d) <- c('direction', 'amount')
forward <- sum(d$amount[d$direction=='forward'])
d$amount[d$direction == 'up'] <- -d$amount[d$direction == 'up']
depth <- sum(d$amount[d$direction != 'forward'])
depth * forward

# Part 2
d$aim <- d$amount
d$aim[d$direction == 'forward'] <- 0
d$aim <- cumsum(d$aim)
depth <- sum(d$amount[d$direction == 'forward'] * d$aim[d$direction == 'forward'])
depth * forward