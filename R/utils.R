shrinkVector <- function( vec, newLength, 
      mode = c( "max", "min", "absmax", "mean" ) ) {
   stopifnot( length( newLength ) == 1 )
   stopifnot( is.numeric( newLength ) )
   stopifnot( newLength > 0 )
   stopifnot( floor(newLength) == newLength )
   stopifnot( length(vec) >= newLength )
   mode <- match.arg( mode )
   modeNum <- as.integer( match( mode, c( "max", "min", "absmax", "mean" ) ) )   
   if( is( vec, "Rle" ) ) {
      shrinkRleVector( vec, newLength, mode )   
   } else if( is.integer( vec ) ) {
      .Call( `shrink_vector_int`, vec, as.integer( newLength ), modeNum )
   } else if( is.numeric( vec ) ) {
      .Call( `shrink_vector_double`, vec, as.integer( newLength ), modeNum )
   } else
      stop( "vec must be an integer or numeric vector or an IRanges::Rle object" )
}   

shrinkRleVector <- function (vec, newLength, mode = c("max", "min", "absmax", "mean")) 
{
   mode <- match.arg( mode )
   segStarts <- length(vec) / newLength * 0:(newLength-1) + 1
   segEnds   <- length(vec) / newLength * 1:newLength + 1
   switch( mode,
      `max` = aggregate( vec, FUN=max, start=segStarts, end=segEnds ),
      `min` = aggregate( vec, FUN=min, start=segStarts, end=segEnds ),
      `absmax` = {
         mx = aggregate( vec, FUN=max, start=segStarts, end=segEnds )
         mn = aggregate( vec, FUN=min, start=segStarts, end=segEnds )
         ifelse( abs(mx) > abs(mn), mx, mn ) },
      `mean` = aggregate( vec, FUN=mean, start=segStarts, end=segEnds ) )
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

