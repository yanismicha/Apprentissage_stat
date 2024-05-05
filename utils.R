
plot_with_contours = function(X, my_func, n.grid = 200, y = NULL, show.labels = FALSE){
  
  if(ncol(X) != 2){
    stop("only valid for matrix made of two columns")
  }
  if(is.null(y)){
    y = rep(1, nrow(X))
  }
  
  # generate grid of values
  x1.grid = seq(min(X[,1]), max(X[,1]), length.out = n.grid)
  x2.grid = seq(min(X[,2]), max(X[,2]), length.out = n.grid)
  # init result
  F = matrix(0, nrow = length(x1.grid), ncol = length(x2.grid))
  # compute values
  for(i in 1:nrow(F)){
    for(j in 1:ncol(F)){
      xy = matrix( c(x1.grid[i],x2.grid[j]), nrow = 1)
      F[i,j]  = my_func(xy) 
    }
  }
  # plot
  plot(X[,1], X[,2], col = y, xlab = "x1", ylab = "x2", pch = 19)
  grid()
  contour(x1.grid, x2.grid, F, add = TRUE, col = "red", drawlabels = show.labels)
  
}