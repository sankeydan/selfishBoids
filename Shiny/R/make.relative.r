make.relative = function(dir.rel.ali){
  dir.rel.ali = ifelse(dir.rel.ali < -(pi/2), dir.rel.ali+(2*pi),dir.rel.ali)
  dir.rel.ali = ifelse(dir.rel.ali >  (pi/2), dir.rel.ali-(2*pi),dir.rel.ali)
  return(dir.rel.ali)
}