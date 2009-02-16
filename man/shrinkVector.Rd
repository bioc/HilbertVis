\name{shrinkVector}
\alias{shrinkVector}

\title{ shrink a vector by partitioning it into bins and taking the maxima in
the bins }
\description{
Given a (potentially very long) vector, the vector is partitioned into a given 
number of (up to rounding errors) equally long bins, and a vector summerizing  
each of the bins with one number it returned. 
}
\usage{
shrinkVector(vec, newLength,  mode = c("max", "min", "absmax", "mean"))
}
\arguments{
  \item{vec}{ The vector to be shrunk. May be numeric or integer.}
  \item{newLength}{ The desired size of the return vector, i.e., the number of
  partitions }
  \item{mode}{ the summerization mode:
     'max': take the maximal value of each bin;
     'min': take the minimal value of each bin;
     'absmax': take the value with largest absolute value;
     'mean': take the mean of the bin values. }
}
\value{
   A vector of length \code{newLength} with the summary values of each of the bin
   of \code{vector}.
}
\author{ Simon Anders, EMBL-EBI (sanders\@fs.tum.de) }
\seealso{ \code{\link{plotLongVector}},
\code{\link[ShortRead:pileup]{ShortRead::pileup}}, 
\code{\link[HilbertVisGUI:simpleLinPlot]{HilbertVisGui::simpleLinPlot}} }
\examples{
   shrinkVector( 100000 + 1:1000, 17 )
}
