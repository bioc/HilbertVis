\name{showHilbertImage}
\alias{showHilbertImage}
\title{ display a hilbert }
\description{
   A convenient wrapper around \code{levelplot} to display a hilbert image matrix as
   it is returned by \code{\link{hilbertImage}}. Alternatively to \code{levelplot},
   \code{EBImage} is available as well.
}
\usage{
showHilbertImage( mat, 
   palettePos = colorRampPalette(c("white", "red"))(300), 
   paletteNeg = colorRampPalette(c("white", "blue"))(300), 
   maxPaletteValue = max(abs(mat)), 
   mode = c("lattice", "EBImage", "EBImage-batch") )
}
\arguments{
  \item{mat}{ The matrix to be displayed. In principle this can be any matrix, 
  but typically, it is one returned by \code{\link{hilbertImage}}. }
  \item{palettePos}{ The colour palette to be used for the positive entries in \code{mat} (including 0). }
  \item{paletteNeg}{ The colour palette to be used for the negative entries in \code{mat}. }
  \item{maxPaletteValue}{ The absolute value to which the right end of the palettes should correspond. 
  (The left ends correspond to 0.) }
  \item{mode}{ For mode "lattice", the function \code{levelplot} from the \code{lattice} package is used. An (invisible) lattice object
    is returned that can be displayed with \code{show}. In interactive mode, the image is displayed automatically.
    For mode "EBImage" the image is displayed with the \code{EBImage} package, and for "EBImage-batch", the same image is 
    produced and not displayed but rather returned as a value suitable to be passed to EBImage's \code{display} function.
  }
}
\value{
  A lattice or EBImage graphics object. For all modes except ``EBImage-batch'' it is marked ``invisible''.
}
\author{ Simon Anders, EMBL-EBI (sanders\@fs.tum.de) }
\seealso{ \code{\link{hilbertImage}} }
\examples{
  # See ?hilbertImage for examples.
}
