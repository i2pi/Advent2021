d <- readLines('~/Desktop/Advent2021/data/day12.txt')

nodeNames <- unique(unlist(strsplit(d, '-')))

# Create adjacency matrix
N <- length(nodeNames)
a <- matrix(nrow=N, ncol=N, 0)
colnames(a) <- rownames(a) <- nodeNames
x<-lapply(strsplit(d, '-'), \(l) a[l[1],l[2]] <<- a[l[2],l[1]] <<- 1)


dfs <- function(cur, a, prev, limit) {
  # Depth first search
  
  # Create a vector (vp) of previously visited nodes that can't be revisted
  vp <- prev[toupper(prev) != prev]
  if (length(vp) > 0) {
    vpCount <- aggregate(rep(1, length(vp)), list(nodeName=vp), sum)
    if (any(vpCount$x > limit)) {
      vp <- vpCount$nodeName
    } else {
      vp <- vpCount$nodeName[vpCount$x > limit]  
    }
  }
  if (cur %in% vp) return()
  
  nexts <- names(which(a[cur,]==1))
  nexts <- nexts[!(nexts %in% vp)]
  nexts <- nexts[nexts != 'start']
    
  for (n in nexts) if (n != 'end') {
    dfs(n, a, c(prev, cur), limit) 
  } else {
    paths[[length(paths)+1]] <<- c(prev, cur, 'end')
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