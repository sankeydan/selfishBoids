pert = function(length.output){
  sign = ifelse (rbinom (length.output,1,0.5) == 0, -1,1)
  apply( t(runif(length.output,0,1)),2,function(x){
    eta.dist$turn[sample( which( eta.dist$prob > x))[1]]
  })*sign

}
