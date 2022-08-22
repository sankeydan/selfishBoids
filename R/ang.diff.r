ang.diff = function(x,y){

  # x=pred.angle
  # y=cent.head
zfunc = function(z){
  if ( z< 0){
    z2=  c( z,pi+abs (  diff( c(z,-pi) ) ))
  } else {
    z2=  c( z,-pi-abs( diff( c(z,pi))))
  }
  return(z2)
}
x2 = zfunc(x)
y2 = zfunc(y)

diff1=diff( c( x2[1], y2[1]))#
diff2=diff( c( x2[1], y2[2]))
 wm= which.min(c( abs(diff1 ),
      abs( diff2)))
 angdif=c(diff1,diff2)[wm]
 return (angdif)
}

