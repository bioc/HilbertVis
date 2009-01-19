\name{seqBin}
\alias{seqBin}
\title{ Shorten a vector by sequentially binning it }
\description{
  The length of the vector 'x' is divided in bins of size 'binWidth', 
  all of which (execpt for possibly the last one) have the same
  length. Each bin is summerized to a single value and a vector of
  these values is returned. Four modes for summerization are provided:
  taking the maximal value of each bin, taking the minimal value, taking 
  the value with largest absolute value and taking the mean of the bin
  values.
}
\usage{
seqBin(x, binWidth, mode = c("max", "min", "absmax", "mean"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the vector to be binned }
  \item{binWidth}{ the size of each bin. The last bin may be shorter }
  \item{mode}{ the summerization mode }
}
\value{
  a vector with one element per bin, summerizing the bin
}
\author{ Simon Anders, EMBL-EBI, sanders\@fs.tum.de }
\seealso{ \code{\link{plotLongVector}} }
\examples{
   seqBin( 1:1000, 50 )
}
