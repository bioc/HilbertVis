shrinkVector <- function( vec, newLength, 
      mode = c( "max", "min", "absmax", "mean" ) ) {
   stopifnot( length( newLength ) == 1 )
   stopifnot( is.numeric( newLength ) )
   stopifnot( newLength > 0 )
   stopifnot( floor(newLength) == newLength )
   stopifnot( length(vec) >= newLength )
   stopifnot( is.numeric( vec ) )
   match.arg( mode )
   modeNum <- as.integer( match( mode, c( "max", "min", "absmax", "mean" ) ) )   
   if( is.integer( vec ) ) {
      .Call( `shrink_vector_int`, vec, as.integer( newLength ), modeNum )
   } else {
      .Call( `shrink_vector_double`, vec, as.integer( newLength ), modeNum )
   }   
}   

plotLongVector <- function (vec, offset = 1, shrinkLength = 4000, xlab = "", ylab = "", ... ) {
   shrinkLength <- floor( shrinkLength )
   plot( NULL, xlab = xlab, ylab = ylab, 
      xlim = offset + c( 0, shrinkLength-1 ) * (length(vec)/shrinkLength) ,
      ylim = c( min( vec, na.rm=TRUE ), max( vec, na.rm=TRUE ) ), ... )
   segments( 
      offset + 0:(shrinkLength - 1) * (length(vec)/shrinkLength), 
      shrinkVector(vec, shrinkLength, mode="min"), 
      offset + 1:shrinkLength * (length(vec)/shrinkLength), 
      shrinkVector(vec, shrinkLength, mode="max"), ... ) }
   
makeWiggleVector <- function( start, end, value, chrlength ) {
   stopifnot( is.numeric( start ) )
   stopifnot( is.numeric( end ) )
   stopifnot( is.numeric( value ) )
   stopifnot( is.numeric( chrlength ) )
   stopifnot( length( end ) == length( start ) )
   stopifnot( length( value ) == length( start ) )
   stopifnot( length( chrlength ) == 1 )
   .Call( `make_wiggle_vector`, as.integer(start), as.integer(end),
      as.numeric(value), as.integer(chrlength) )
}      

