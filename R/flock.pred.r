flock.pred = function (         N                            = NULL,
                                zones                        = NULL,
                                speeds                       = NULL,
                                avoidance.weight             = NULL,
                                max.turn.rate                = NULL,
                                n.timesteps                  = NULL,
                                eta.predate                  = NULL,
                                max.pred.turn.rate           = NULL,
                                pred.speed                   = NULL,
                                grid.size                    = NULL,
                                starting.pos.sd              = NULL,
                                future.predict.multip        = NULL,
                                future.predict.multip.pred   = NULL,
                                eta                          = NULL,
                                plot                         = NULL,
                                pred.start                   = NULL,
                                bordersTF                    = NULL,
                                border.dist.range            = NULL,
                                continuous.method            = NULL,
                                q.att.if.cont                = NULL,
                                q.ali.if.cont                = NULL
){
  
  # FOR QUICK RUN
  
  # N                            = NULL
  # zones                        = NULL
  # speeds                       = NULL
  # avoidance.weight             = NULL
  # max.turn.rate                = NULL
  # n.timesteps                  = NULL
  # eta.predate                  = NULL
  # max.pred.turn.rate           = NULL
  # pred.speed                   = NULL
  # grid.size                    = NULL
  # starting.pos.sd              = NULL
  # future.predict.multip        = NULL
  # future.predict.multip.pred   = NULL
  # eta                          = NULL
  # plot                         = NULL
  # pred.start                   = NULL
  # bordersTF                    = NULL
  # border.dist.range            = NULL
  # continuous.method            = NULL
  # q.att.if.cont                = NULL
  # q.ali.if.cont                = NULL
  
  
  # FOR SHINY
  

    # N = N
    # zones =zones
    # speeds = rep ( 1,N)
    # avoidance.weight = rep(input$w, N)
    # max.turn.rate = input$max.turn.rate
    # n.timesteps = 200
    # eta.predate = input$eta.predate
    # max.pred.turn.rate = input$max.pred.turn.rate
    # pred.speed =  input$pred.speed
    # grid.size = 100
    # future.predict.multip =  input$future.predict.multip
    # future.predict.multip.pred = input$future.predict.multip.pred
    # eta = input$eta
    # plot = F
    # pred.start = 100
    # bordersTF = input$bordersTF
    # border.dist.range= input$border.dist.range
    # continuous.method = F
  
  
  
  # FOR RUN ALL ITERATIONS
  
  # N = N
  # zones =zones
  # speeds = rep ( 1,N)
  # avoidance.weight = rep(w, N)
  # max.turn.rate = max.turn.rate
  # n.timesteps = 200
  # eta.predate = eta.predate
  # max.pred.turn.rate = max.pred.turn.rate
  # pred.speed =  pred.speed
  # grid.size = 100
  # future.predict.multip =  future.predict.multip
  # future.predict.multip.pred = future.predict.multip.pred
  # eta = eta
  # plot = F
  # pred.start = 100
  # bordersTF = bordersTF
  # border.dist.range= border.dist.range
  # continuous.method = continuous.method
  # q.att.if.cont = 0.8
  # q.ali.if.cont = 0.2
  
  
  
  { # skip to loop
    
    # set up agents , params
    
    
    # set speed
    heading = suppressWarnings( as.numeric(  circular::rvonmises(N,pi,0)))-pi
    vel    =  matrix(c( speeds * sin(heading),
                        speeds * cos(heading)),N,2)
    
    # set positions
    pos = matrix( c( runif ( N,0,grid.size),
                     runif ( N,0,grid.size)),N,2)
    
    
    # set predator head and pos
    # pred.pos =  sample(c(runif(1, 0,grid.size),rbinom(1,1,0.5)*grid.size))
    pred.pos =  c(runif(1, 0,grid.size),
                  runif(1, 0,grid.size))
    pred.head= suppressWarnings( as.numeric ( circular::rvonmises(1,1,0))-pi)
    pred.vel = c(pred.speed * sin( pred.head),
                 pred.speed * cos( pred.head))
    
    if ( continuous.method ){
      zones.save = zones
      cont.var  = c(rep ( q.att.if.cont, length ( which ( !is.na ( zones.save[,2])))),
                    rep ( q.ali.if.cont, length ( which (  is.na ( zones.save[,2])))))
      zones [,2] = 1
    }
    
    
    
    
    # for storing data
    pos.store = array(NA, c(N,2,n.timesteps))
    pred.store = array(NA, c(2,n.timesteps))
    
    # store initial position
    pos.store [,,1] = pos
    pred.store [,1] = pred.pos
    catch.store = NA
    pred.catch = F
    k = 1
    grid.edges = c(0,grid.size)
  }
  
  #########################################
  ######### looooooooooooop ###############
  #########################################
  
  
  # FOR EACH TIMESTEP
  while ( k < n.timesteps){
    #k=1
    
    # if ( k == pred.start){
    #   cent = centroid(pos)[[1]]
    #   transpose = - ( cent$pos.i - (grid.size/2))
    #   pos[,1] = pos[,1] + transpose[1]
    #   pos[,2] = pos[,2] + transpose[2]
    # }
    
    # move back onto grid if they have gone off
    xyforborders = matrix(1,N,2)
    for ( i in 1:N){
      foo = periodic.boundary(pos[i,], grid.size=grid.size,borders = bordersTF) 
      pos[i,] = foo$pos.i
      xyforborders[i,] = foo$xyforborders
    }
    
    
    # same for predator
    foo =  periodic.boundary(pred.pos,grid.size=100,borders=bordersTF)
    pred.pos = foo[[1]]
    pred.vel = pred.vel * foo[[2]]
    
    # Dist 2 predator
    if ( k >= pred.start){
      pred.dists = vector()
      for ( m in 2:(nrow(pos)+1)){
        pred.dists = c( pred.dists , ( dis ( i = 1 , j = m, pos = rbind(pred.pos, pos),bordersTF=bordersTF)))
      }
    }
    
    # Change direction if hit a border
    if ( bordersTF){
      vel = matrix ( as.vector(vel)*
                       as.vector(xyforborders), ncol = 2)
    }
    
    
    # store interaction dynamics
    vel.dt     = matrix(NA,N,2)
    vel.att.dt = matrix(NA,N,2)
    vel.ali.dt = matrix(NA,N,2)
    vel.rep.dt = matrix(NA,N,2)
    vel.pred   = matrix( 0,N,2)
    vel.wall   = matrix( 0,N,2)
    wall.dist  = matrix( 0,N,2)
    
    # pos[1,1] = 15
    # pos[2,2] = 95
    # pos[3,1] = 95
    # pos[3,2] = 95
    
    
    # FOR EACH INDIVIDUAL
    for ( i in 1:N){
      #i=1
      
      # shortest distance to neighbour
      dists.vec = rep ( NA, N)
      for ( x in 1:N){
        if ( i != x){
          dists.vec[x] = dis( i, x, pos,bordersTF=bordersTF )
        }
      }
      
      
      # REPULSION
      if ( any(na.omit(dists.vec) < zones[i,3])){
        nei = which(dists.vec < zones[i,3])
        a = repel( pos , i , nei)
        vel.rep.dt[i,]= a/sqrt(a[1]^2 + a[2]^2)
        vel.dt[i,] = vel.rep.dt[i,] 
        repulsion = T
      } else {
        repulsion = F
        att = F
        ali = F
        
        
        # ATTRACTION
        if ( length(which( dists.vec > zones[i,2] & dists.vec < zones[i,1])) > 0  ){
          nei =  which( dists.vec > zones[i,2] & dists.vec < zones[i,1])
          a = attract ( pos+vel*future.predict.multip, i, nei)
          vel.att.dt[i,] =  a/sqrt(a[1]^2 + a[2]^2)
          att = T
        }
        
        # ALIGNMENT
        if ( length(which( dists.vec > zones[i,3] & dists.vec < zones[i,2])) > 0 ){ # [2] is zone of alignment
          nei =  which( dists.vec > zones[i,3] & dists.vec < zones[i,2])
          a = alignment(vel, i , nei)
          vel.ali.dt[i,] = a/sqrt(a[1]^2 + a[2]^2)
          ali = T
        }
        
        # combine att/ali or not
        if ( att ==T & ali == T){
          vel.dt[i,] = ( vel.att.dt[i,] + vel.ali.dt[i,])*0.5
        }
        if ( att ==T & ali == F){
          if ( continuous.method){
            vel.att.dt[i,]
            nei =  which( dists.vec > zones[i,2] & dists.vec < zones[i,1])
            a = alignment(vel, i , nei)
            vel.ali.dt[i,] = a/sqrt(a[1]^2 + a[2]^2)
            vel.dt[i,] = vel.ali.dt[i,] * (1-cont.var[i]) + 
              vel.att.dt[i,] *    cont.var[i]
          } else {
            vel.dt[i,] = vel.att.dt[i,]
          }
        }
        if( att == F & ali == T){
          vel.dt[i,] = vel.ali.dt[i,]
        }
        if (att == F & ali == F){
          a = vel[i,]
          vel.dt[i,] = a/sqrt(a[1]^2 + a[2]^2)
        }
      }
      
      
      ## Avoid predator
      if ( k >= pred.start){
        if ( pred.dists[i] < zones[i,1]){ # zone of attraction
          a = repel( rbind( pos[i,] , pred.pos),1,2)
          vel.pred[i,]= a/sqrt(a[1]^2 + a[2]^2)
        }
      }
      
      # avoid walls
      if ( bordersTF){
        xborder = abs ( pos[i,1] - c(0,grid.size))
        yborder = abs ( pos[i,2] - c(0,grid.size))
        minwalldist = min(c(xborder,yborder))
        if( minwalldist<border.dist.range){
          whi.x= which ( xborder < border.dist.range)
          if( length(whi.x)>0){
            foo.x =  grid.edges[whi.x]
            wall.dist[i,1] = border.dist.range-min(xborder)
          } else{ 
            foo.x =pos[i,1]
          }
          whi.y= which ( yborder < border.dist.range)
          if( length(whi.y)>0){
            foo.y =  grid.edges[whi.y]
            wall.dist[i,2] = border.dist.range-min(yborder)
          } else{ 
            foo.y =pos[i,2]
          }
          a = repel( rbind( pos[i,] ,c(foo.x,foo.y)),1,2)
          vel.wall[i,]= a/sqrt(a[1]^2 + a[2]^2)
          
          
        }
        
      }
    }
    
    ## GROUP LEVEL
    
    # turn angle including walls 
    pref.minus.wallsX = ((1-avoidance.weight)*vel.dt[,1])+(vel.pred[,1]*avoidance.weight)
    pref.minus.wallsY = ((1-avoidance.weight)*vel.dt[,2])+(vel.pred[,2]*avoidance.weight)
    if ( bordersTF){
      relativewall = wall.dist/border.dist.range
      pref.with.wallsX = pref.minus.wallsX*(1-relativewall[,1]) + vel.wall[,1]*(relativewall[,1]) 
      pref.with.wallsY = pref.minus.wallsY*(1-relativewall[,2]) + vel.wall[,2]*(relativewall[,2]) 
    } else {
      pref.with.wallsX = pref.minus.wallsX
      pref.with.wallsY = pref.minus.wallsY
    }
    pref.head = atan2( pref.with.wallsX,pref.with.wallsY)
    pref.turn = atan2( sin(pref.head-heading),
                       cos(pref.head-heading))
    
    # turn angle restricted by max.turn.rate
    turn = sign( pref.turn) *ifelse ( abs(pref.turn) < max.turn.rate , abs( pref.turn) , max.turn.rate)
    heading = heading+turn
    heading = ifelse (  heading < -pi, 2*pi+heading, 
                        ifelse (heading >  pi,-2*pi+(heading), heading))
    
    # VELOCITY/PERTUBATION Save computer power by combining
    new.vel = matrix( rnorm( N*2,c(speeds * sin( heading),
                                   speeds * cos( heading)),eta),N,2)
    heading =  atan2(new.vel[,1],new.vel[,2])
    vel = new.vel # new prey vel
    pos = pos +vel
    
    # PREDATOR MOVEMENT
    if( k>= pred.start ){
      {
      prey = which.min(pred.dists)[1]
      chase = attract (  rbind ( pred.pos , pos[prey,]+vel[prey,]*future.predict.multip.pred),1,2,bordersTF = bordersTF) 
      vel.chase = chase/sqrt(chase[1]^2 + chase[2]^2)
      # turn angle restricted by max.turn.rate
      pref.head = atan2(vel.chase[1],vel.chase[2])
      pref.turn = atan2( sin(pref.head-pred.head),
                         cos(pref.head-pred.head))
      turn = sign( pref.turn) *ifelse ( abs(pref.turn) < max.pred.turn.rate , abs(pref.turn) , max.pred.turn.rate)
      pred.head = pred.head+turn
      pred.head = ifelse (  pred.head < -pi, 2*pi+pred.head, 
                            ifelse (pred.head >  pi,-2*pi+(pred.head), pred.head))
      # VELOCITY/PERTUBATION Save computer power by combining
      pred.vel = matrix( rnorm( 2,c(pred.speed * sin( pred.head),
                                    pred.speed * cos( pred.head)),eta.predate),1,2)
      pred.pos = pred.pos + pred.vel
      pred.head = atan2(pred.vel[1],pred.vel[2])
      
      
      # new prey pos
      pred.dists = vector()
      for ( m in 2:(nrow(pos)+1)){
        pred.dists = c( pred.dists , ( dis ( i = 1 , j = m, pos = rbind(pred.pos, pos),bordersTF=bordersTF)))
      }
      }
      
     
      # DID PREDATOR CATCH A PREY? 
      if ( any(pred.dists < zones[1,3])){ # zone of repulsion
        
        catch.store = which.min(pred.dists)
        end.store = k
        k = n.timesteps
        
      } else {
        end.store = k
        k = k+1
      }
    }else {
      end.store = k
      k = k+1
    }
    
    # store position
    pos.store [,,k] = pos
    pred.store[,k] = pred.pos
    
    #Plot??
    if(plot){
      po = rbind( pos,pred.pos)
      xy =   xylims(po[,1],po[,2])
      # png ( file.path ( filePath,  paste0( stringr::str_pad ( k,3, pad="0") ,".png") ))
      
      colour = c(rep("red",(N)),rep("blue",1))
      plot ( po[,1],
             po[,2], xlim = xy[[1]]+c(-20,20),
             ylim = xy[[2]]+c(-20,20), pch=3, ylab = "y (units)", xlab = "x (units)",col = colour)
      abline(h=seq(0,1000,40),lty=2,col="grey")
      abline(v=seq(0,1000,40),lty=2,col="grey")
      
    }
    
    
  }
  
  pos.store.list  = list ( pos.store = pos.store ,  
                           pred.store = pred.store,
                           catch.store = catch.store,
                           end.store = end.store)
  return( pos.store.list )
}
