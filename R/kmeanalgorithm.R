#' Chose random centerns for k clusters
#' @description chose random centers for cluster   it will only be used internely in kmean clustering algorithm.
#' @param mydata matrix; matrix columns are (mathematical) vectors. Input data.
#'
#' @param k double; number of cluster
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
  for (i in 1:k) z[,i] = mydata[,round(((len/k) * i)-(len/(k*2)))]
  return(z)
}

#' K-means algorithm
#' @description kmean algorithm see Richter19 9.10
#' @param mydata matrix; matrix columns are (mathematical) vectors. Input data.
#'
#' @param k double; number of cluster
#'
#' @param distanceFunction the function we use to calculate distances
#'
#' @return matrix; with k columns with additionall attribuets 1 cluster : vector of values between 1 to k has length ncol(mydata).2 centers  : matrix has k col in each col is saved the center of each cluster.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' kmean(data, 3)
kmean <- function(mydata,k=3,distanceFunction=euclidean_distance_squared) {
  stopifnot( "data has to be a matrix. columns are vectors." = is.matrix(mydata));
  stopifnot("data cannot be empty!" = length(mydata) > 0)
  zent = m(mydata,k)
  mydata= `attr<-`(mydata,"cluster",rep(1,ncol(mydata)))
  iter = 0
  while (TRUE) {
    for (i in seq_along(mydata[1,])) {
      for (j in 1:k) {
        #print(as.integer(attr(mydata[[i]],'cluster')))
        if(distanceFunction(mydata[,i],zent[,attr(mydata,"cluster")[i]]) > distanceFunction(mydata[,i],zent[,j]))
          {attr(mydata,"cluster")[i]=j}
      }
    }
    #cat("this ist the time",toString(iter))
    #print(mydata)
    newzent = matrix(0, ncol = k,nrow = nrow(mydata))
    for (i in seq_along(zent[1,])) {
      n=0
      for (j in seq_along(mydata[1,])){
        if(all(zent[,attr(mydata,"cluster")[j]]==zent[,i])){
          n=n+1
          newzent[,i]= newzent[,i]+ mydata[,j]
        }
      }
      newzent[,i] = newzent[,i]/n
    }
    konvergenz = 0
    for (i in seq_along(zent[1,])) {
      konvergenz = konvergenz + distanceFunction(zent[,i], newzent[,i])
    }

    if(konvergenz<=10e-20){
      break
    }

    zent = newzent
    iter = iter +1
    #if (iter>200){
     # print("es gibt keine eindeutige Zetren von cluster")
      #break
      #}
  }
  mydata = `attr<-`(mydata,"centers",zent)
  return(mydata)
}

