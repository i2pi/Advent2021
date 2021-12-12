d <- readLines('~/Desktop/Advent2021/data/day12.txt')

nodeNames <- unique(unlist(strsplit(d, '-')))
N <- length(nodeNames)
a <- matrix(nrow=N, ncol=N, 0)
colnames(a) <- rownames(a) <- nodeNames
x<-lapply(strsplit(d, '-'), \(l) a[l[1],l[2]] <<- a[l[2],l[1]] <<- 1)


dfs <- function(cur, a, prev, limit) {
  nexts <- names(which(a[cur,]==1))
  vp <- prev[toupper(prev) != prev]
  if (length(vp) > 0) {
    vp <- aggregate(rep(1, length(vp)), list(nodeName=vp), sum)
    # TODO: This doesn't capture all the double visits...
    if (any(vp$x > limit)) {
      vp <- vp$nodeName
    } else {
      vp <- vp$nodeName[vp$x > limit]  
    }
  }
  
  nexts <- nexts[!(nexts %in% vp)]
  nexts <- nexts[nexts != 'start']
    
  for (n in nexts) if (n != 'end') dfs(n, a, c(prev, cur), limit) else {
    paths[[length(paths)+1]] <<- c(prev, cur, 'end')
    pathCount <<- pathCount + 1
  }
}

# Part 1
paths <- list()
pathCount <- 0
dfs('start', a, NULL, 0)
pathCount

# Parts 2
paths <- list()
pathCount <- 0
dfs('start', a, NULL, 1)
pathCount

# Only allow one double visit to a small cave
paths <- paths[sapply(paths, \(p) {
  x <- p[tolower(p) == p]
  sum(aggregate(rep(1,length(x)), list(name=x), sum)$x > 1) <= 1
})]

length(paths)