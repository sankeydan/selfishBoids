rm(list=ls())
timE = Sys.time()
date = "2022-07-27"


{
  
  # library(selfishBoids)
  
  
  
  Niter                      = 10000
  prop.d                     = c(0.5,0.1,0.9) # prop.defectors 
  continuous.method = c(0,1)  # 0 = zones ;  # 1 = continuous
  n.timesteps  = 300 
  pred.start = 200
  
  
  
  # modify variables 
  vars = list ( 
    N                          = c(20,10,30)  ,
    ali.zone                   = c(0.9 , 0.5, 0.1),
    sense.range                = c(25, 20, 30  ),
    max.turn.rate              = c(0.5, 0.1, 1) ,
    max.pred.turn.rate         = c(0.1 ,0.05,0.2),
    pred.speed                 = c(1.5,1,2,3,4,5),
    eta                        = c(0.1,0.05,0.2 ),
    eta.predate                = c(0.01, 0.005, 0.02),
    w                          = c(0.1 ,0.5, 0.9),
    future.predict.multip      = c(1  ,2,5),
    future.predict.multip.pred = c(1  ,2,5 ) ,
    border.dist.range          = c(NA, 2, 5, 10)
  )
  
  vars1 = unlist ( lapply ( vars , function(x)x[1]))
  
  expand.funct = function (vec , name.of.vec ){
    # vec=c(1  ,2,5)
    # name.of.vec = "future.predict.multip.pred"
    exg = expand.grid( prop.d,continuous.method, vec )
    mat = matrix ( c(exg[,1],exg[,2],rep ( vars1,each = nrow(exg) )), 
                   ncol = length(vars1)+2, 
                   dimnames = list ( NULL, c("prop.d","continuous.method",names(vars1))))
    mat[,name.of.vec] = exg[,3]
    mat
  }
  rb = NULL
  namvar = names(vars)
  for (i in 1:length(vars)){
    rb = rbind ( rb , expand.funct ( vars[[i]] , namvar[i]))
  }
  rb[ rb[,"continuous.method"] == 1 , "ali.zone"] = NA
  rb2 = apply ( rb , 1, function(x) {
    #x=as.character(rb[1,])
    paste (as.character(x),collapse="-")
  })
  table(rb2)
  rb = rb [ !duplicated(rb2),]
  save(rb , file = file.path ( getwd() , "rb_current.rda"))
}
{
  # Loop
  library ( foreach)
  library( doParallel)
  library (parallel)
  detectCores()
  doParallel::registerDoParallel(cores = 70 )
  #is=49:96
  is= 1:nrow(rb)
  i=4
  j=1
  filePath=file.path (getwd(),  date)
  if(!dir.exists(filePath) )dir.create( filePath)
}
# Parallel loop
greb = foreach (  i = is, .inorder = T  ) %dopar% {
  #i=94
  fold = file.path (getwd(), "R")
  files=  list.files( fold)
  for( j in 1:length(files)){
    source ( file.path(fold, files[j]))
  }
  
  #Assign default variables
  for ( j in 1:ncol(rb)){
    #j=2
    var = rb[i,j]
    assign (names( var),var)
  }
  
  
  # stopifnot 
  if ( floor ( N*prop.d) != N*prop.d ){
    stop ( "N multiplied by prop.d must be integer")
  }
  
  
  #set up
  N.att = N*prop.d
  N.ali = ceiling(N*(1-prop.d))
  N = N.att + N.ali
  att.zones = c( sense.range, 1, 1)
  ali.zones = att.zones
  ali.zones[2] = ( (sense.range - 1) * ali.zone ) +1 
  zones = t ( matrix ( c( rep ( att.zones , N.att),
                          rep ( ali.zones , N.ali)), nrow = 3))
  bordersTF = ifelse ( is.na(border.dist.range),F,T)
  
  
  # model
  pos.store.lists = list()
  
  
  for ( l in 1:Niter){
    pos.store.list = 
      flock.pred ( 
        N = N,
        zones =zones,
        speeds = rep ( 1,N),
        avoidance.weight = rep(w, N),
        max.turn.rate = max.turn.rate,
        n.timesteps = n.timesteps,
        eta.predate = eta.predate,
        max.pred.turn.rate = max.pred.turn.rate,
        pred.speed =  pred.speed,
        grid.size = 100,
        future.predict.multip =  future.predict.multip,
        future.predict.multip.pred = future.predict.multip.pred,
        eta = eta,
        plot = F,
        pred.start = pred.start,
        bordersTF = bordersTF,
        border.dist.range= border.dist.range,
        continuous.method = continuous.method,
        q.att.if.cont = 0.9,
        q.ali.if.cont = 0.1
      )
    
    end = pos.store.list$end.store
    pos.store.list$pos.store  = pos.store.list$pos.store  [,,pred.start:end]
    pos.store.list$pred.store = pos.store.list$pred.store [,pred.start:end]
    str(pos.store.list)
    pos.store.lists[[l]] = pos.store.list
    print(l)
  }
  
  isave=  paste0 ( stringr::str_pad(i,3,pad="0"),".rda")
  save ( pos.store.lists , file = file.path ( filePath, isave  ))
}
