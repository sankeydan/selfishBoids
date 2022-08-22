alignment = function ( vel , i, nei) {
  Av.vel = c( x = mean( vel[nei,1]),
              y = mean( vel[nei,2])) # Average of neighbour's velocity
  return(Av.vel)
}
