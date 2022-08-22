rm(list=ls())




{
  
  # library(selfishBoids)
  
  
  
  Niter                      = 10
  times = 100
  prop.d                     = c(0.1,0.9) # prop.defectors 
  continuous.method = c(1)  # 0 = zones ;  # 1 = continuous
  n.timesteps  = 400 
  pred.start = 300
  
  
  
  # modify variables 
  vars = list ( 
    N                          = c(20)  ,
    ali.zone                   = c(0.9 ),
    sense.range                = c(25),
    max.turn.rate              = c(0.5) ,
    max.pred.turn.rate         = c(0.2),
    pred.speed                 = c(1.2),
    eta                        = c(0.1),
    eta.predate                = c(0.01),
    w                          = c(0.5),
    future.predict.multip      = c(1  ),
    future.predict.multip.pred = c(1  ) ,
    border.dist.range          = c(NA)
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
  doParallel::registerDoParallel(cores = 2 )
  #is=49:96
  is= 1:nrow(rb)
  i=4
  j=1
  filePath=file.path (getwd(),  "Output" , "mls_test")
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
        q.att.if.cont = 0.8,
        q.ali.if.cont = 0.2
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

fold = ( file.path  (getwd() , "Output" , "mls_test"))
files = list.files ( fold)
i=1
d = list()
for ( i in 1:length(files)){
  
  load( file.path (fold, files[i]))
  d[[i]] = unlist(lapply ( pos.store.lists , function(x) x$end.store))
}

par(mfrow = c(2,1))
hist(d[[1]])
hist(d[[2]])

boxplot(cbind(d[[1]],d[[2]]))
