periodic.boundary = function( pos.i, grid.size=100, borders=F) {

  xyforborders = c(1,1)
  if(borders==F){
  if ( pos.i[1] > grid.size){
    pos.i[1] = pos.i[1] - grid.size
  }

  if ( pos.i[2] > grid.size){
    pos.i[2] = pos.i[2] - grid.size
  }

  if ( pos.i[1] < 0){
    pos.i[1] = pos.i[1] + grid.size
  }

  if ( pos.i[2] < 0){
    pos.i[2] = pos.i[2] + grid.size
  }
  } else {
    if ( pos.i[1] > grid.size){
      pos.i[1] =  grid.size + (  grid.size - pos.i[1])
      xyforborders [1] = -1
    }
    
    if ( pos.i[2] > grid.size){
      pos.i[2] = grid.size + (  grid.size - pos.i[2])
      xyforborders [2] = -1
    }
    
    if ( pos.i[1] < 0){
      pos.i[1] = 0 + (  0 - pos.i[1])
      xyforborders [1] = -1
    }
    
    if ( pos.i[2] < 0){
      pos.i[2] = 0 + (  0 - pos.i[2])
      xyforborders [2] = -1
    }
  }
  
  return(list( pos.i=pos.i,xyforborders= xyforborders))
}

