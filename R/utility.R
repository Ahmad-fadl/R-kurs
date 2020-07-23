
#' Generate a Single Training Vector
#' @description This function generates a single training vector as specified in "Beispiel 10.23" in reference "Richter19"
#' @return returns a single training vector
#' @example create_training_vector()
#' @references Richter, S. (2019). Statistisches und maschinelles Lernen. Springer Spektrum.
generate_training_vector <- function () {
  R <- function(z) {
    Delta <- runif(1,-0.1, 0.1)
    if (z == 1L) {
      return(Delta + .3)
    } else {
      stopifnot("z should be either 1 or 2" = z == 2L)
      return(Delta + 1)
    }
  }

  Z <- sample.int(2, size = 1)
  U <- runif(1, 0, 2 * pi)
  X_1 <- R(Z) * c(cos(U), sin(U))

  return(X_1)
}

#' Generate Training Data.
#' @description This function generates training data as specified in "Beispiel 10.23" in reference "Richter19". It is meant for testing purposes.
#' @param n integer; the number of vectors in the returned training data.
#' @return returns a matrix with two rows and \code{n} columns; each column is a training vector.
generate_training_data <- function (n) {
  replicate(n, generate_training_vector())
}
