set.pos = function ( N,grid.size = 100){
  # initialise position
  pos = matrix(c( rnorm( N,grid.size/2,5),
                  rnorm( N,grid.size/2,5)), N,2)
  return(pos)
}