d<-read.csv('~/Desktop/advent1.txt', header=F)
measurements <- d$V1
#measurements <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

# Part 1
sum(diff(measurements) > 0)
# Part 2
sum(diff(apply(embed(measurements, 3), 1, sum)) > 0)