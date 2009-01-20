\name{hilbertImage}
\alias{hilbertImage}

\title{ Produce a matrix that visualizes a long data vector along a Hilbert curve }

\description{
   Calculate a Hilbert curve visualization of a long data vector and return it as a square matrix.
}

\usage{
hilbertImage(data, level = 9, mode = "absmax")
}

\arguments{
  \item{data}{ A (potentially very long) vector of numerical data. }
  \item{level}{ The level of the Hilbert curve, determining the size of the returned matrix }
  \item{mode}{ The binning mode. See \code{\link{shrinkVector}} for details.  }
}

\details{
See the package vignette for an explanation of this visualization technique.
}

\value{
  A matrix of dimension \code{2^level} x \code{2^level}. Each matrix element corresponds to a bin of consecutive elements
    of the data vector, the bins arranged to follow the Hilbert curve of the given level. By default, the value of a 
    matrix element is either the largest or smallest element in the bin, whichever is larger by absolute value. (See 
    \code{\link{shrinkVector}} for other possible binning modes.)
    
  To display such a matrix graphically, you can use the standard functions \code{image} or \code{levelplot}
  but the function \code{\link{showHilbertImage}} may be more convenient.
}
\author{ Simon Anders, EMBL-EBI, sanders@fs.tum.de }
\note{ For an interactive GUI to explore a Hilbert curve visualisation, use the function
\code{\link[hilbertVisGUI:hilbertDisplay]{hilbertDisplay}} in the \code{HilbertVisGUI} package.
}
\examples{
   # Get a vector with example data
   dataVec <- makeRandomTestData( )
   
   # Plot it in conventional (linear) fashion
   plotLongVector( dataVec )
   
   # Note how the peaks look quite uniform

   # Get the Hilbert curve matrix
   hMat <- hilbertImage( dataVec )
   
   # Plot it with the 'showHilbertImage' function
   showHilbertImage( hMat )
   
   # Note how you can now see the non-uniformity hidden in the previous plot.
   # Note also the ugly aliasing when you change the size of the plot window.
   # Using EBImage allows to display in each matrix element as one pixel:
   
   # if( require ( EBImage ) )
   #    showHilbertImage( hMat, mode="EBImage" )
}
