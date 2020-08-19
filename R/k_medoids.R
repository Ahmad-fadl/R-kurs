#' dissimialrity
#'
#' @description calculates all dissimilarities from a data point
#'
#' @param data matrix; columns are vectors
#' @param k integer; column of which the dissimilarities are calculated
#'
#' @return vector of all dissimilarities
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' dissim(data, 3)
dissim <- function(data, k){
  res <- NULL
  for (i in 0:ncol(data)) {
    current_dissim <- 0
    for (j in 1:nrow(data)) {
      current_dissim <- current_dissim + abs(data[j,i]-data[j,k])
    }
    res <- c(res, current_dissim)
  }
  res
}

#' set closest medoid
#'
#' @description assigns each vector to the closest medoid
#'
#' @param data matrix; columns are vectors
#' @param medoids vector; datapoints that are set as medoids
#'
#' @return input data with attribute "cluster" assigning a cluster to every column-vector and "mindissims" assigning the dissimilarity to the medoid
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' medoids <- round(runif(3, 1, ncol(data)))
#' k_medoids(data, medoids)
set_closest <- function(data, medoids){
  res <- data
  dissims <- NULL
  for (i in medoids){
    dissims <- c(dissims, dissim(data, i))
  }
  mindissims <- dissims[1:ncol(data)]
  cluster <- rep(1, ncol(data))
  for (i in 1:length(medoids)){
    for(l in 1:ncol(data)){
      if(mindissims[l] > dissims[l+(i-1)*ncol(data)]){
        mindissims[l] <- dissims[l+(i-1)*ncol(data)]
        cluster[l] <- i
      }
    }
  }
  attr(res, "cluster") <- cluster
  attr(res, "mindissims") <- mindissims
  res
}

#' k-medoids algorithm
#'
#' @description Finds clusters in the input data using the k_medoids algorithm.
#'
#' @param data matrix; columns are vectors
#' @param k integer; number of clusters
#'
#' @return input data with attribute "cluster" assigning a cluster to every column-vector.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' k_medoids(data, 2)
k_medoids <- function(data, k=2){
  stopifnot("data cannot be empty!" = length(data) > 0);
  stopifnot("data must be a matrix!" = is.matrix(data) == TRUE);
  medoids <- round(stats::runif(k, 1, ncol(data)))
  start <- medoids
  cluster <- set_closest(data, medoids)
  cost <- sum(attributes(cluster)[[3]])
  while (TRUE) {
    best_cost <- cost
    best_medoids <- NULL
    current_medoids <- NULL
    for (l in 1:k){
      for (i in 1:ncol(data)){
        test_medoids <- medoids
        test_medoids[l] <- i
        test_cluster <- set_closest(cluster, test_medoids)
        test_cost <- sum(attributes(test_cluster)[[3]])
        if(test_cost < best_cost){
          best_cost <- test_cost
          best_medoids <- i
          current_medoids <- l
        }
      }
    }
    if(best_cost < cost){
      medoids[current_medoids] <- best_medoids
      cluster <- set_closest(cluster, medoids)
      cost <- sum(attributes(cluster)[[3]])
    }else break
  }
  attributes(cluster)[3]<-NULL
  cluster
}
