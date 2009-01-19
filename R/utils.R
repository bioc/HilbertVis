shrinkVector <- function( vec, newLength ) {
   stopifnot( length( newLength ) == 1 )
   stopifnot( is.numeric( newLength ) )
   stopifnot( newLength > 0 )
   stopifnot( floor(newLength) == newLength )
   stopifnot( length(vec) >= newLength )
   stopifnot( is.numeric( vec ) )
   if( is.integer( vec ) ) {
      .Call( `shrink_vector_int`, vec, as.integer( newLength ) )
   } else {
      .Call( `shrink_vector_double`, vec, as.integer( newLength ) )
   }   
}   

plotLongVector <- function( vec, offset=1, shrinkLength=4000, 
      xlab="", ylab="", type="h", ... )
   plot( 
      offset + 0:(shrinkLength-1) * (length(vec)/shrinkLength), 
      shrinkVector( vec, shrinkLength ), 
      xlab=xlab, ylab=ylab, type=type, ... )

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

seqBin <- function( x, binWidth, mode = c( "max", "min", "absmax", "mean" ) ) {
   match.arg( mode )
   modeNum <- as.integer( match( mode, c( "max", "min", "absmax", "mean" ) ) )
   if( storage.mode( x ) == "integer" )
      .Call( `seq_bin_int`, x, as.integer( binWidth ), modeNum )
   else
      .Call( `seq_bin_double`, x, as.integer( binWidth ), modeNum )

}
