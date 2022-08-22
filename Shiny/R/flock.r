flock = function (  N = 20 ,
                    self.zones = c(200,0.0001 ,0),
                    coop.zones = c(200,10   ,0),
                    n.timesteps = 60 ,# how many timesteps in each model?
                    n.trials = 1 ,# repeats of whole model
                    n.proportions = 5 # proportion of defectors. Main hypothesis
){
  
  # N = 20
  # self.zones = c(200,0.0001 ,0)
  # coop.zones = c(200,10     ,0)
  # n.timesteps = 60
  # n.trials = 1
  # n.proportions = 5
  # max.turn.rate         = 0.9
  # field.of.inperception = 0.9
  # ali.zone              = 7
  
  { # skip to loop
    
    
    # set up agents , params
    source( file.path ( PROJHOME , "Source" , "initialisation.r"))
    
    
    {
      
      
      # proportion of defectors
      prop.d = c( 0 ,seq ( N/n.proportions , N , length.out = n.proportions))
      if ( all.equal(prop.d, as.integer(prop.d)) != T){
        stop( "Need number of defectors to be an integer! Change N or n.proportions")
      }
      prop.d
    }
    
    # set speed
    speeds = rep ( speed.param , N)
    vel    =  matrix(c( speeds * sin(heading),
                        speeds * cos(heading)),N,2)
    
    # for storing data
    pos.store = array(NA, c(N,2,n.timesteps,n.trials,n.proportions+1), dimnames = list(NULL,NULL,NULL,NULL,prop.d))
  }
  
  #########################################
  ######### looooooooooooop ###############
  #########################################
  
  
  # FOR EACH PROPORTION OF DEFECTORS
  for ( l in 1:(n.proportions+1)){
    #l=1
    
    # set pos
    pos = set.pos(N)
    
    zones = rbind ( do.call("rbind", replicate(  prop.d[l], self.zones, simplify = F)),
                    do.call("rbind", replicate(N-prop.d[l], coop.zones, simplify = F)))
    
    # FOR EACH TRIAL 
    for ( k in 1:n.trials){
      #k=1
      
      # store initial position
      pos.store [,,1,k,l] = pos
      
      # FOR EACH TIMESTEP
      for ( j in 1: n.timesteps){
        #j=1
        
        # move back onto grid if they have gone off
        for ( i in 1:N){
          pos[i,] = periodic.boundary(pos[i,], grid.size) 
        }
        
        # store interaction dynamics
        vel.dt     = matrix(NA,N,2)
        vel.att.dt = matrix(NA,N,2)
        vel.ali.dt = matrix(NA,N,2)
        vel.rep.dt = matrix(NA,N,2)
        
        # FOR EACH INDIVIDUAL
        for ( i in 1:N){
          #i=1
          
          # shortest distance to neighbour
          dists.vec = rep ( NA, N)
          for ( x in 1:N){
            if ( i != x){
              dists.vec[x] = dis( i, x, pos , grid.size = grid.size)
            }
          }
          
          # FIELD OF INPERCEPTION (looks like a pizza slice)
          dists.vec = pizza( dists.vec, head.pizza = heading[i], pos,  i, zones)
          
          # REPULSION
          if ( any(na.omit(dists.vec) < zones[i,3])){
            nei = which(dists.vec < zones[i,3])
            vel.rep.dt[i,] = repel( pos , i , nei)
            vel.dt[i,] = vel.rep.dt[i,] 
          } else {
            att = F
            ali = F
            
            # ATTRACTION
            if ( length(which( dists.vec > zones[i,2] & dists.vec < zones[i,1])) > 0  ){
              nei =  which( dists.vec > zones[i,2] & dists.vec < zones[i,1])
              a = attract(pos, i , nei)
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
              vel.dt[i,] = vel.att.dt[i,]
            }
            if( att == F & ali == T){
              vel.dt[i,] = vel.ali.dt[i,]
            }
            if (att == F & ali == F){
              vel.dt[i,] = vel[i,]
            }
          }
          
        }
        
        ## GROUP LEVEL
        
        # turn angle restricted by max.turn.rate
        pref.head = atan2(vel.dt[,1],vel.dt[,2])
        pref.turn = atan2( sin(pref.head-heading),
                           cos(pref.head-heading))
        turn = sign( pref.turn) *ifelse ( abs(pref.turn) < max.turn.rate , abs(pref.turn) , max.turn.rate)
        heading = heading+turn
        new.head = ifelse (  heading < -pi, 2*pi+heading, 
                             ifelse (heading >  pi,-2*pi+(heading), heading))
        
        # VELOCITY/PERTUBATION Save computer power by combining
        new.vel = matrix( rnorm( N*2,c(speed.param * sin( new.head),
                                       speed.param * cos( new.head)),eta),N,2)
        
        # NEW POSITION + VEL
        pos = pos+new.vel
        vel = new.vel
        
        # store initial position
        pos.store [,,j,k,l] = pos
        
        
      }
     # print ( paste ( k,"/",n.trials, " - Trials"))
    }
    print( paste( l , "/", n.proportions+1 , " - Proportions"))
  }
  pos.store.list  = list ( pos.store = pos.store , prop.d = prop.d, grid.size = grid.size)
  return( pos.store.list )
}