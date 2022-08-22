rm(list=ls())
{
  att.zone                   = c(25  )
  Niter                      = 1000
  # prop.defectors 
  prop.ds                     = c(0.5,0.1,0.9) 
  
  # modify variables 
  vars = list ( 
    N                          = c(20)  ,
    max.turn.rate              = c( 0.5) ,
    field.of.inperception      = c( 1.5),
    ali.zone                   = c(15),
    pred.speed                 = c(2.0 ),
    pred.view                  = c(3  ),
    eta.predate                = c(0.01),
    future.predict.multip      = c(1  ),
    future.predict.multip.pred = c(1   ) ,
    starting.pos.sd            = c(3   ) ,
    eta                        = c(0.1 ),
    max.pred.turn.rate         = c(0.1 ),
    v                          = c(1.5 ),
    w                          = c(1 ) ,
    pred.Ra                    = c(75 ),
    bordersTF = c(TRUE, FALSE)
  )
  vars1 = unlist ( lapply ( vars , function(x)x[1]))
  
  expand.funct = function (vec , name.of.vec ){
    # vec= c(20,10,30)
    # name.of.vec = "N"
    exg = expand.grid( prop.ds, vec )
    mat = matrix ( c(exg[,1],rep ( vars1,each = nrow(exg) )), ncol = length(vars1)+1, dimnames = list ( NULL, c("prop.ds",names(vars1))))
    mat[,name.of.vec] = exg[,2]
    mat
  }
  rb = NULL
  namvar = names(vars)
  for (i in 1:length(vars)){
    rb = rbind ( rb , expand.funct ( vars[[i]] , namvar[i]))
  }
  rb2 = apply ( rb , 1, function(x) {
    #x=as.character(rb[1,])
    paste (as.character(x),collapse="-")
  })
  table(rb2)
  rb = rb [ !duplicated(rb2),]
  rb
}
# Loop
library(parallel)
library(foreach)
library(doParallel)
detectCores()
registerDoParallel(cores = 6 )
is=1:nrow(  rb )
i=1
greb = foreach (  i = is, .inorder = T #
                  #  ,.export=c("model")
) %dopar% {
  
  library(selfishBoids)
  #i=1
  
  for(k in 1:ncol(rb)){
    assign ( names(rb[i,k]) , rb[i,k])
  }
  
  #set up
  N.att = N *prop.ds
  N.ali = ceiling(N*(1-prop.ds))
  N = N.att + N.ali
  self.zones = c( att.zone, 1, 1)
  coop.zones = self.zones
  coop.zones[2] = ali.zone 
  zones = t ( matrix ( c( rep ( self.zones , N.att),
                          rep ( coop.zones , N.ali)), nrow = 3))
  
  # model
  pos.store.lists = list()
  timE = Sys.time()
  for ( l in 1:Niter){
    pos.store.list = 
      flock.pred ( 
        N = N,
        zones =zones,
        speeds = rep ( v,N),
        avoidance.weight = rep(w, N),
        field.of.inperception = field.of.inperception,
        max.turn.rate = max.turn.rate,
        n.timesteps = 100,
        pred.view = pred.view,
        eta.predate = eta.predate,
        max.pred.turn.rate = max.pred.turn.rate,
        pred.speed =  pred.speed,
        grid.size = 100,
        starting.pos.sd = starting.pos.sd,
        future.predict.multip =  future.predict.multip,
        future.predict.multip.pred = future.predict.multip.pred,
        eta = eta,
        plot = F,
        pred.Ra = pred.Ra,
        bordersTF = bordersTF 
      )
    
    pos.store.lists[[l]] = pos.store.list
    #print(l)
  }
  #print ( timE - Sys.time())
  
  #     # Combine and save
  #     pos.store.list2 = list( pos.store.lists  = pos.store.lists, vars =  vars, var.of.interest = c(  name, k) )
  #     filePath = file.path  ( fold , paste ( c( name , k , "prop.d", o),collapse = "-"))
  #     filePath = paste0 ( filePath , ".rda") 
  #     save( pos.store.list2 , file =   filePath )
  #     
  #     # Takestock
  #     print ( paste ( "o =", m2 , "/" , length(prop.ds)))
  #     m2=m2+1
  #   }  
  #   print ( paste ( "k =", m , "/" , length(ks)))
  #   m=m+1
  # }
  # # Takestock
  # print ( paste ( "i =", i , "/" , length(whis)))
  pos.store.lists
}

greb = list ( greb , rb)
save( greb , file = file.path(PROJHOME , "Output" , "BorderData" , "data1.rda"))


rm(list= ls())
load ( file.path(PROJHOME , "Output" , "BorderData" , "data1.rda"))
dats = NULL#
for ( i in 1:200){
  vaL = 500 + i
  ch = chisq.test( c ( vaL , 1000-vaL ) , p=c(0.5,0.5))
  dats = rbind ( dats , c( ch$statistic,
                           ch$p.value))
}
plot(dats[,2])
which ( dats[,2] < 0.05)[1]
save.effect.size = dats[31,1]

# For each variable
dats=NULL
j=1
i=1
pos.list = greb[[1]]
vars = greb[[2]]
for (  j in 1:length(pos.list)){
  
  
  pos.store.list = pos.list[[j]]
  
  # Were caught individuals selfish
  
  N =  vars[j,"N"]
  prop.d=vars[j,"prop.ds"] 
  last.selfish = prop.d * N
  caught = unlist ( lapply( pos.store.list , function(x) ( x$catch.store)))
  tb = table ( ifelse ( caught <= last.selfish , 1,2))
  if ( any ( is.na ( tb ))){
    stop( "there were no deaths under one condition")
  }
  chisQ = chisq.test(tb, p =   c (  prop.d, 1-prop.d))
  mult. = (1-prop.d) / prop.d
  tb2 = tb
  tb2[1] = tb2[1] * mult.
  statistic = ifelse ( diff (tb2) < 0 , chisQ$statistic, -chisQ$statistic)
  
  dats = rbind( dats, c( effect.size = statistic, p = chisQ$p.value))
}
plot(dats[,1])

ggitem = cbind( dats , vars)
ggitem = as.data.frame( ggitem)

library(ggplot2)

ggplot ( ggitem, aes( x=bordersTF, y = effect.size.2 , col = as.character ( prop.ds) ))+
  geom_line()+
  geom_hline(-4,4)

save( ggitem, file=  file.path ( PROJHOME , "Plots" , "PlotData", "P01_borderTest.rda"))
