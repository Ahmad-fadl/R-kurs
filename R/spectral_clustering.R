
# expecting input vectors to be the columns of a matrix
#' Spectral Clustering over a Matrix's Column-Vectors.
#'
#' @description Performs the spectral clustering algorithm describe in "Definition 10.50" in reference Richter19
#' @references Richter19 -- Richter, S. (2019). Statistisches und maschinelles Lernen. Springer Spektrum.
#'
#' @param data matrix whose columns are training vectors
#' @param k integer; projection target dimension; defaults to \code{1}
#' @param mercer_core function; a function object to a mercer_core (see "Definition 4.32" in Richter19); defaults to the gaussian core (see p.116, Richter19)
#'
#' @return a matrix containing the optimal \code{k}-dimensional projections of the input vectors as its columns
#' @export
#'
#' @examples
#' X <- create_training_data(400)
#' optimal_projections <- spectral_clustering(X)
spectral_clustering <- function (data, k=1, mercer_core=gaussian_core) {
  n <- ncol(data);
  W_tilde <- matrix(0, ncol=n, nrow=n);

  # mercer_core is a symmetric function
  # => W_tilde is symmetric
  # TODO: optimize this
  for (i in seq(n)) {
    for (j in seq(n)) {
      W_tilde[i,j] <- mercer_core(data[,i], data[, j]);
    }
  }

  D_tilde <- matrix(0, ncol=n, nrow=n);
  for (i in seq(n)) {
    D_tilde[i,i] <- sum(W_tilde[i,]);
  }

  L_tilde <- D_tilde - W_tilde;

  D_inverse_sqrt <- D_tilde;
  for (i in seq(n)) {
    D_inverse_sqrt[i,i] <- 1 / sqrt(D_tilde[i,i]);
  }

  # eigen returns a list containing two attributes:
  # values is a vector of the eigenvalues
  # vectors corresponding eigenvectors of (euclidian) length 1
  p <- eigen(D_inverse_sqrt %*% L_tilde %*% D_inverse_sqrt);

  b <- p$vectors[, order(p$values)];

  beta <- apply(b, 2, function(j) { sqrt(n) * D_inverse_sqrt %*% j });

  alpha <- matrix(0, ncol=n, nrow=k);
  for (i in seq(k)) {
    alpha[i,] = beta[, i+1];
  }

  return(alpha);
}
