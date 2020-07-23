
regionQuery <- function (data, P, eps, distanceFunction) {
  seq(ncol(data))[apply(data, 2, function(D){ distanceFunction(P, D) < eps; })]
}

DBSCAN <- function (data, eps, MinPts, distanceFunction=euclidean_distance) {
  stopifnot("data cannot be empty!" = length(data) > 0);

  n <- ncol(data);

  C <- 0;
  attr(data, "visited") <- rep(FALSE, n);
  attr(data, "isNoise") <- rep(FALSE, n);
  attr(data, "cluster") <- rep(-1, n);

  for (P_idx in seq(n)) {
    # skip previously visited vectors
    if (attr(data, "visited")[P_idx] == TRUE) next;

    attr(data, "visited")[P_idx] <- TRUE;
    N <- regionQuery(data, data[, P_idx], eps, distanceFunction);
    if (length(N) < MinPts) {
      attr(data, "isNoise")[P_idx] <- TRUE;
    } else {
      C <- C + 1;
      # expand cluster
      attr(data, "cluster")[P_idx] <- C;
      for (Q_idx in N) {
        if (attr(data, "visited")[Q_idx] == FALSE) {
          attr(data, "visited")[Q_idx] <- TRUE;
          N_prime <- regionQuery(data, data[, Q_idx], eps, distanceFunction);
          if (length(N_prime) >= MinPts) {
            N <- c(N, N_prime);
          }
        }
        if (attr(data, "cluster")[Q_idx] == -1) {
          attr(data, "cluster")[Q_idx] = C;
          attr(data, "isNoise")[Q_idx] = FALSE;
        }
      }
    }
  }

  attr(data, "visited") <- NULL;
  attr(data, "isNoise") <- NULL;
  return(data);
}

