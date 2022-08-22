rm(list = ls())

# Library
library(selfishBoids)

# Variables
make.videos = F
max.turn.rates = c( 0.1 ,0.5) 
field.of.inperceptions = c( 0.1, 0.5)
ali.zones = c(5,10,15)
vars = expand.grid(max.turn.rates, field.of.inperceptions , ali.zones)
names(vars) = c("max.turn.rate", "field.of.inperception" , "ali.zone" )

for ( i in 1:nrow( vars)){
  #i=1
  
  # Variables
    max.turn.rate         = vars$max.turn.rate        [i] 
    field.of.inperception = vars$field.of.inperception[i] 
    ali.zone              = vars$ali.zone             [i] 
    
    # Model
    pos.store.list = flock ( N = 20 ,
                             self.zones = c(200,0.0001 ,0),
                             coop.zones = c(200,  ali.zone[i]   ,0),
                             n.timesteps = 400 ,# how many timesteps in model
                             n.trials = 5 ,# repeats of whole model
                             n.proportions = 5 # proportion of defectors
    )
  
  # Data
  {
    pos.store = pos.store.list[[1]]
    grid.size = pos.store.list$grid.size
    prop.d    = pos.store.list$prop.d
    dims = dim (pos.store) 
    }
  
  # MAKE VIDEO
  if( make.videos){
    for ( i in 1:dims[5]){
      #i=1
      pos = pos.store[ ,,,1,i]#
      filePath = file.path ( PROJHOME , "Plots" , "Videos", paste0("video", i))
      if ( ! dir.exists(filePath)){
        unlink(filePath, recursive = TRUE)
        dir.create(filePath)
      }
      for ( j in 1:dims[3]){
        #j=1
        xy =   xylims(pos[,1,j],pos[,2,j])
        png ( file.path ( filePath,  paste0( stringr::str_pad ( j,3, pad="0") ,".png") ))
        plot ( pos[,1,j],
               pos[,2,j], xlim = xy[[1]]+c(-20,20),
               ylim = xy[[2]]+c(-20,20), pch=3, ylab = "y (units)", xlab = "x (units)")
        abline(h=seq(0,1000,40),lty=2,col="grey")
        abline(v=seq(0,1000,40),lty=2,col="grey")
        dev.off()
      }
      print(i)
    }
  }
  
  # GROUP SPEED
  group.speeds = matrix( NA, dims[4], dims[5])
  for ( i in 1:dims[5]){
    #i=1
    for ( j in 1:dims[4]){
      pos = pos.store[ ,,,j,i]
      cent = t(  apply ( pos, 3, colMeans))
      speed = dis(speed.mat = cent, speed=T)
      speed [speed > 5 ] = NA
      group.sped = mean(speed,na.rm = T)
      
      group.speeds[j,i] =  group.sped
    }
  }
  boxplot(group.speeds)
  
  # ARE DEFECTORS CLOSER TO CENTROID?
  
  prop.d = pos.store.list[[2]]
  for ( i in 2:(length(prop.d)-1)){
    #i=3
    for ( j in 1:dims[4]){
    pos = pos.store[ ,,,j,i]
    cent = t(  apply ( pos, 3, colMeans))
    dist2cent = apply( pos,1, function (x){ 
      # x = pos[1,,]
      x = t(x)
      c =  x[,1]    - x[,2]
      d =  cent[,1] - cent[,2]
      px = cbind(abs( (x[,1] - cent[,1])),
                 grid.size - abs(x[,1] - cent[,1]))
      py = cbind(abs((x[,2] - cent[,2])),
                 grid.size - abs(x[,2] - cent[,2]))
      a = apply( px , 1, function(x) x[x == min(x)])
      b = apply( py , 1, function(x) x[x == min(x)])
      return( sqrt( a^2 + b^2 ))
    })
    df = rbind ( 
      data.frame ( d2c = as.vector ( dist2cent[,1:prop.d[i]])                  , type = "self"),
      data.frame ( d2c = as.vector ( dist2cent[,(prop.d[i]+1):ncol(dist2cent)]), type = "coop")
    )
 
    }
    if( i == 1){
      dfs = df
    } else { 
      dfs = rbind( dfs, df)
      }
  }
}
boxplot(dfs[,1]~dfs[,2])