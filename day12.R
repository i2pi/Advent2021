d <- readLines('~/Desktop/Advent2021/data/day12.txt')

# Create adjacency matrix
nodeNames <- unique(unlist(strsplit(d, '-')))
N <- length(nodeNames)
a <- matrix(nrow=N, ncol=N, 0)
colnames(a) <- rownames(a) <- nodeNames
x<-lapply(strsplit(d, '-'), \(l) a[l[1],l[2]] <<- a[l[2],l[1]] <<- 1)

dfs <- function(cur, a, prev, limit) {
  # Create a vector (vp) of previously visited nodes that can't be revisted
  vp <- prev[toupper(prev) != prev]
  lvp <- length(vp)
  if (limit & lvp > 1) {
    # If we haven't visited a small cave more than once, then all is fair, 
    # except returning to the start cave
    if (lvp == length(unique(vp))) vp <- 'start'
    if (cur %in% vp) return()
  }
  
  nexts <- names(which(a[cur,]==1)) # Find the adjacent caves
  nexts <- nexts[!(nexts %in% vp)]  # Drop the ones we can't revisit
  
  for (n in nexts) if (n != 'end') {
    dfs(n, a, c(prev, cur), limit) 
  } else {
    paths[[length(paths)+1]] <<- c(prev, cur, 'end')  # Found a path
  }
}

# Part 1
paths <- list()
dfs('start', a, NULL, 0)
length(paths)

# Parts 2
paths <- list()
dfs('start', a, NULL, 1)
length(paths)