% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeanalgorithm.R
\name{kmean}
\alias{kmean}
\title{K-means algorithm}
\usage{
kmean(mydata, k = 3, distanceFunction = euclidean_distance_squared)
}
\arguments{
\item{mydata}{matrix; matrix columns are (mathematical) vectors. Input data.}

\item{k}{double; number of cluster}

\item{distanceFunction}{the function we use to calculate distances}
}
\value{
matrix; with k columns with additionall attribuets 1 cluster : vector of values between 1 to k has length ncol(mydata).2 centers  : matrix has k col in each col is saved the center of each cluster.
}
\description{
kmean algorithm see Richter19 9.10
}
\examples{
data <- matrix(c(1,1.1,1,1,2,2,2,2.1), ncol=4)
kmean(data, 3)
}
