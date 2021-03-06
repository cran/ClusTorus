% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmeans.torus.R
\name{kmeans.torus}
\alias{kmeans.torus}
\title{K-Means Clustering on Torus}
\usage{
kmeans.torus(data, centers = 10, iter.max = 100, nstart = 1)
}
\arguments{
\item{data}{n x d matrix of toroidal data on \eqn{[0, 2\pi)^d}}

\item{centers}{either the number of clusters or a set of initial
cluster centers. If a number, a random set of row in x is
chosen as the initial centers.}

\item{iter.max}{the maximum number of iterations}

\item{nstart}{if \code{centers} is a number, how many random sets
should be chosen?}
}
\value{
returns a \code{kmeans} object, which contains
  input data, cluster centers on torus, membership,
  total sum of squares, within cluster sum of squares,
  between cluster centers sum of squares, and the size of
  each cluster.
}
\description{
\code{kmeans.torus} implements extrinsic k-means clustering
 on toroidal space.
}
\details{
In Euclidean space, we know that the total sum of squares
  is equal to the summation of the within cluster sum of squares and
  the between cluster centers sum of squares. However, toroidal space
  does not satisfy the property; the equality does not hold. Thus,
  you need to be careful to use the sum of squares.
}
\examples{
data <- ILE[1:200, 1:2]

kmeans.torus(data, centers = 2,
             iter.max = 100, nstart = 1)
}
\references{
'S. Jung, K. Park, and B. Kim (2021),
  "Clustering on the torus by conformal prediction"
}
\seealso{
\code{\link[stats]{kmeans}}, \code{\link[ClusTorus]{ang.minus}}
}
