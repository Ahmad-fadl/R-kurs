m <- function(mydata,k){
  len = length(mydata)
  z = rep(1,k)
  for (i in seq_along(z)) z[i] = mydata[as.integer(((len/k) * i)-(len/(k*2)))]
  return(z)
}
kmean <- function(mydata,k) {
  zent = m(mydata,k)
  mydata = as.list(mydata)
  iter = 0
  while (TRUE) {
    for (i in (seq_along(mydata))) {
      mydata[[i]]=`attr<-`(mydata[[i]],"cluster",zent[1]) 
      for (j in 2:length(zent)) {
        #print(as.integer(attr(mydata[[i]],'cluster')))
        if(abs(as.integer(mydata[[i]]) - as.integer(attr(mydata[[i]],'cluster'))) > abs(as.integer(mydata[[i]]) - as.integer(zent[[j]])))
          {mydata[[i]]=`attr<-`(mydata[[i]],"cluster",zent[[j]])}
      }
    }
    #cat("this ist the time",toString(iter))
    #print(mydata)
    newzent = rep(0,length(zent))
    for (i in seq_along(zent)) {
      n=0
      for (j in seq_along(mydata)){
        if(attr(mydata[[j]],"cluster")==zent[i]){
          n=n+1
          newzent[i]= newzent[i]+ as.integer(mydata[[j]])
        }
      }
      newzent[i] = newzent[i]/n
    }
    konvergenz =0
    for (i in seq_along(zent)) {
      konvergenz = konvergenz + abs(zent[i]- newzent[i])
    }
    if (konvergenz<=1) {
      break
     # print("konvergenz")
    }
    #if (iter>50){print("iter")
     # break
     # }
    zent = round(newzent)
    iter = iter +1
  }
  return(mydata)
}



