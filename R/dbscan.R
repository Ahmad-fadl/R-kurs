
#' Density-Based Spatial Clustering of Applications with Noise (DBSCAN)
#' @description Finds clusters in the input data using the DBSCAN algorithm.
#'
#' @param data matrix; matrix columns are (mathematical) vectors. Input data.
#' @param eps double; neighborhood radius
#' @param minPts integer; minimum number of points in neighborhood required
#' @param distanceFunction function; function calculating the distance between two vectors of length \code{nrow(data)}
#'
#' @return matrix; input data with attribute "cluster" assigning a cluster to every column-vector.
#' @export
#'
#' @examples
#' data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
#' DBSCAN(data, .2, 1)
DBSCAN <- function (data, eps, minPts, distanceFunction=euclidean_distance) {
  stopifnot("data cannot be empty!" = length(data) > 0);
  stopifnot( "data has to be a matrix. columns are vectors." = is.matrix(data));
  stopifnot( "eps has to be a positive number" = eps > 0);
  stopifnot( "minPts has to be a positive integer." = as.integer(minPts) > 0);

  n <- ncol(data);

  regionQuery <- function (P) {
    distances <- apply(data, 2, function(D){ distanceFunction(P, D)});
    seq(n)[apply(data, 2, function(D){ distanceFunction(P, D) < eps; })];
  }


  C <- 0;
  attr(data, "visited") <- rep(FALSE, n);
  attr(data, "isNoise") <- rep(FALSE, n);
  attr(data, "cluster") <- rep(-1, n);

  for (P_idx in seq(n)) {
    # skip previously visited vectors
    if (attr(data, "visited")[P_idx] == TRUE) next;

    attr(data, "visited")[P_idx] <- TRUE;
    N <- regionQuery(data[, P_idx]);
    if (length(N) < minPts) {
      attr(data, "isNoise")[P_idx] <- TRUE;
    } else {
      C <- C + 1;
      # expand cluster
      attr(data, "cluster")[P_idx] <- C;
      while(length(N)) {
        Q_idx <- N[1];
        if (attr(data, "visited")[Q_idx] == FALSE) {
          attr(data, "visited")[Q_idx] <- TRUE;
          N_prime <- regionQuery(data[, Q_idx]);
          if (length(N_prime) >= minPts) {
            N <- c(N, N_prime);
          }
        }
        if (attr(data, "cluster")[Q_idx] == -1) {
          attr(data, "cluster")[Q_idx] = C;
          attr(data, "isNoise")[Q_idx] = FALSE;
        }
        N <- N[-1];
      }
    }
  }

  attr(data, "visited") <- NULL;
  attr(data, "isNoise") <- NULL;
  return(data);
}
