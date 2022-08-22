pizza = function ( dists.vecr, head.pizza, posr, i,zones,field.of.inperceptionr,pred =F ){

  # dists.vecr = pred.dists
  # head.pizza = pred.head
  # posr = rbind( pred.pos, pos)
  # i = 1
  # zonesr = pred.dists
  # field.of.inperceptionr = pred.view
  # pred = T
  
  if( !pred){
  test.pizza = which(dists.vecr < zones[i,1])

  # angle which corresponds to middle of pizza
  eps = -head.pizza
  } else{
    test.pizza = 2:nrow(posr)
    eps = head.pizza
  }
  
  # neighbour's relative position
  rel.head = atan2(posr[test.pizza,1] - posr[i,1], 
                   posr[test.pizza,2] - posr[i,2])

  # difference between the angles
  diff = abs(atan2(sin(rel.head-eps), cos(rel.head-eps)))

  # is this difference bigger than half of the field of inperception? (Remember eps is in  the middle of the pizza slice)
  if ( !pred){
  dists.vecr[test.pizza[diff < (field.of.inperceptionr/2)]] = NA
  } else {
    dists.vecr[diff > (field.of.inperceptionr)] = NA
  }
  dists.vecr
  return(dists.vecr)
}
