attract = function ( pos, i, nei,grid.size = 100, bordersTF = F) {

  # loop to return true neighbour position
  nei.pos = matrix(NA, length(nei), 2)
  c = 1
  for( j in nei) {
    nei.pos[c,] = dis( i, j, pos , grid.size = grid.size, return.boundary = T,bordersTF = bordersTF)[1:2]
    c = c+1
  }
  
  # Centroid of neighbours
  CoM = colMeans(nei.pos) # What is the individual's percieved centre?

  # velocity
  vel.att.dt = (CoM - pos[i,]) 
  return(vel.att.dt)


}
