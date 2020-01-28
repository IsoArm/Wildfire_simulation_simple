wildfire<-function(p, burning, total_trees){
  burned<-burning
  while(total_trees > burned && burning > 0){
    danger_zone<-sum(rgeom(burning, prob = 0.5))
    new_burns<-sum(rbinom(danger_zone, 1, p))
    burning<-new_burns
    burned<- burned + new_burns
  }
  if(burned > total_trees){
    return(1000)
  }else{
    return(burned)
  }
}

x=c()
y=c()
for (i in 1:10000){
  x[i]<-wildfire(0.9, 1, 1000)
}
for (i in 1:10000){
  y[i]<-wildfire(0.4, 6, 1000)
}
mean(x)
mean(y)
t.test(x)
t.test(y)
