
#' @description chose random centers for clusters.
#'   it will only be used internely in kmean clustering algorithm.
#' @param data matrix; matrix columns are (mathematical) vectors. Input data.
#'
#' @param k double; number of clusters
#'
#'
#'
#' @return matrix; with k columns .
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' m(data, 3)
m <- function(mydata,k){
  len = ncol(mydata)
  z = matrix(1,ncol = k,nrow = nrow(mydata))
  for (i in 1:k) z[,i] = mydata[,as.integer(((len/k) * i)-(len/(k*2)))]
  return(z)
}
source("R\\math_utility.r")

#' @description kmean algorithm see Richter19 9.10
#' @param data matrix; matrix columns are (mathematical) vectors. Input data.
#'
#' @param k double; number of clusters
#'
#'
#'
#' @return matrix; with k columns with additionall attribuets
#'               1 clusters : vector of values between 1 to k has length ncol(mydata) .
#'               2 centers  : matrix has k col in each col is saved the center of each cluster.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' kmean(data, 3)
kmean <- function(mydata,k=3,distanceFunction=euclidean_norm_squared) {
  stopifnot( "data has to be a matrix. columns are vectors." = is.matrix(mydata));
  stopifnot("data cannot be empty!" = length(mydata) > 0)
  zent = m(mydata,k)
  mydata= `attr<-`(mydata,"clusters",rep(1,ncol(mydata)))
  iter = 0
  while (TRUE) {
    for (i in seq_along(mydata[1,])) {
      for (j in 2:k) {
        #print(as.integer(attr(mydata[[i]],'cluster')))
        if(distanceFunction(mydata[,i]-zent[,attr(mydata,"clusters")[i]]) > distanceFunction(mydata[,i]-zent[,j]))
          {attr(mydata,"clusters")[i]=j}
      }
    }
    #cat("this ist the time",toString(iter))
    #print(mydata)
    newzent = matrix(0, ncol = k,nrow = nrow(mydata))
    for (i in seq_along(zent[1,])) {
      n=0
      for (j in seq_along(mydata[1,])){
        if(all(zent[,attr(mydata,"clusters")[j]]==zent[,i])){
          n=n+1
          newzent[,i]= newzent[,i]+ mydata[,j]
        }
      }
      newzent[,i] = newzent[,i]/n
    }
    konvergenz = 0
    for (i in seq_along(zent[1,])) {
      konvergenz = konvergenz + distanceFunction(zent[,i]- newzent[,i])
    }

    if(konvergenz<=1){
      break
    }

    zent = newzent
    iter = iter +1
    if (iter>200){
      print("es gibt keine eindeutige Zetren von clusters")
      break
      }
  }
  mydata = `attr<-`(mydata,"centers",round(zent))
  return(mydata)
}

