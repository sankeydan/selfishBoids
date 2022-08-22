xylims = function( x, y){
  # x = dat$x
  # y = dat$y
  
  r.x = range ( na.omit(x))
  r.y = range ( na.omit(y))
  
  limmax = max( diff( r.x),
                diff( r.y))/2
  
  x.start = diff(r.x)/2 + r.x[1]
  y.start = diff(r.y)/2 + r.y[1]
  
  xlim = x.start + ( c ( - limmax ,  limmax))
  ylim = y.start + ( c ( - limmax ,  limmax))

  return( list( xlim, ylim ))
  
}
