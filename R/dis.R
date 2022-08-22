dis = function(i = NULL, j = NULL, pos = NULL, speed.mat = NULL, grid.size = 100,
               return.boundary = F, dist.2.centroid = F, speed=F,return.x.y=F,bordersTF=F){
  
  # SPEED 
  if( speed){
    #speed.mat = d
    t1 = speed.mat[1:(nrow(speed.mat)-1),]
    t2 = speed.mat[2: nrow(speed.mat),]
    
    speed = apply( cbind(t1,t2),1, function (x){
      X = x[1:2]
      Y = x[3:4]
      c =  X[1] - Y[1]
      d =  X[2] - Y[2]
      px = c(abs( (X[1] - Y[1])),
             grid.size - abs(X[1] - Y[1]))
      py = c(abs((X[2] - Y[2])),
             grid.size - abs(X[2] - Y[2]))
      a = min( px)
      b = min( py)
      return(sqrt( a^2 + b^2 ))})
    
    speed = ifelse ( speed > grid.size/2 , grid.size-speed, speed ) 
    return(speed)
  } else {
    
    # DISTANCE
    X = pos[i,]
    if ( !dist.2.centroid){
      Y = pos[j,]
    } else {
      Y = colMeans(pos, na.rm = T)
    }
    if ( length(X) == 1 | length(Y) == 1) {
      stop( "both inputs should be vectors!!!!")
    }
    c =  X[1] - Y[1]
    d =  X[2] - Y[2]
    px = c(abs( (X[1] - Y[1])),
           grid.size - abs(X[1] - Y[1]))
    py = c(abs((X[2] - Y[2])),
           grid.size - abs(X[2] - Y[2]))
    if(bordersTF){
      a = abs(px[1])
      b = abs(py[1])
    } else{
    a = abs(min( px))
    b = abs(min( py))
    }
    
    # BOUNDARY CONDITIONS 
    if ( return.boundary ) {
      
      x.right = which( abs(px) == a ) == 2 && c > 0
      x.left  = which( abs(px) == a ) == 2 && c < 0
      y.top   = which( abs(py) == b ) == 2 && d > 0
      y.bot   = which( abs(py) == b ) == 2 && d < 0
      
      x.j = ifelse( x.right == T, grid.size + pos[j,1], pos[j,1])
      x.j = ifelse( x.left  == T, x.j - grid.size  , x.j  )
      y.j = ifelse( y.top   == T, grid.size + pos[j,2], pos[j,2])
      y.j = ifelse( y.bot   == T, y.j - grid.size  , y.j )
      
      return(c ( x.j, y.j,x.right,y.top, x.left, y.bot))
    }
    
    else {
      
      # return distance
      if ( return.x.y){
       return( list ( a ,  b))
      } else {
      return( sqrt( a^2 + b^2 ))
      }
    }
  }
}
