
centroid = function ( pos, group.max.rad = Inf, d2c.t1 = NULL,grid.size = 100){
  
  arr = array( 0, c(N,N,2) )
  for ( i in 1:N){
    for ( j in 1:N){
      if(i!=j){
        arr[i,j,] = dis(i, j, pos, grid.size=grid.size, return.boundary = T)[3:4]
      }
    }
  }
  lr = sum( as.vector(arr[,,1]))
  tb = sum( as.vector(arr[,,2]))
  
  pos.cent = pos
  
  if ( lr != 0){
    movers = which( rowSums( arr[,,1]) !=0)
    for ( l in movers){
      stayer = which(arr[l,,1] == 1)[1]
      pos.cent[l,1] = dis( stayer, l, pos, grid.size=grid.size, return.boundary = T)[1]
    }
  }
  if ( tb != 0){
    movers = which( rowSums( arr[,,2]) !=0)
    stayers= c(1:N)[-movers]
    for ( l in movers){
      pos.cent[l,2] = dis( stayers[1], l, pos, grid.size=grid.size, return.boundary = T)[2]
    }
  }
  
  #plotR(pos.cent, lim = "min-max")
  
  d2c = rep(NA,N)
  for ( i in 1:N){
    d2c[i] = dis( i, pos=pos.cent, dist.2.centroid = T)
  }
  d2c.init = d2c
  
  while( max (na.omit(d2c)) > group.max.rad){
    pos.cent[which.max(d2c),] = NA
    d2c = rep(NA,N)
    for ( i in 1:N){
      d2c[i] = dis( i, pos=pos.cent, dist.2.centroid = T)
    }
  }
  
  # return
  if ( !is.null(d2c.t1)){
  if( identical( is.na(d2c) , is.na(d2c.t1)) ){
    cent = periodic.boundary(colMeans(pos.cent,na.rm = T), grid.size)
    return( list(cent, d2c, F,d2c.init))
  } else{
    return( list(rep(NA,2), d2c, T, d2c.init) )
  }
  } else {
    cent = periodic.boundary(colMeans(pos.cent,na.rm = T), grid.size)
    return( list(cent, d2c, F, d2c.init))
  }
}


