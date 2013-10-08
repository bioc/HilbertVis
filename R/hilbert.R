hilbertCurvePoint <- function( t, lv )
   .Call( hilbertCurveR, as.integer(t), as.integer(lv) )
   
   
hilbertCurve <- function(lv) { 
   a <- t( sapply( 0:(4**lv-1), hilbertCurvePoint, lv ) )
   colnames(a) <- c( "x", "y" )
   data.frame(a)
}   
    
plotHilbertCurve <- function( lv, new.page=TRUE ) {
   if( new.page )
      grid.newpage()
   pushViewport( plotViewport( c( 3, 3, 2, 2 ) ) )
   pushViewport( viewport( x=.5, y=.5, width=1, height=1, default.units="snpc" ) )
   bgd <- (-.5):(2^lv-.5) 
   pushViewport( dataViewport( xscale=range(bgd), 
      yscale=range(bgd) ) )
   tics <- unique( c( as.integer( 0:7 * 2^(lv-3) ), 2^lv-1 ) )
   grid.xaxis( tics )
   grid.yaxis( tics )
   grid.rect()
   cv <- hilbertCurve(lv)
   grid.lines( cv$x, cv$y, default.units="native", gp=gpar(col="red") )
   grid.points( cv$x, cv$y, default.units="native", gp=gpar(col="magenta"), 
      size = unit( .45, "native" ) )
   grid.segments( bgd, rep(min(bgd),length(bgd)), bgd, rep(max(bgd),length(bgd)),
      default.units = "native", gp=gpar(col="blue") )
   grid.segments( rep(min(bgd),length(bgd)), bgd, rep(max(bgd),length(bgd)), bgd,
      default.units = "native", gp=gpar(col="blue") )
   popViewport( 3 )
}

hilbertImage <- function( data, level=9, mode = "absmax" ) {
   hc <- hilbertCurve( level )
   hcl <- ( 2^level-1 - hc$y ) * 2^level + hc$x + 1
   a <- matrix( NA_real_, nrow=2^level, ncol=2^level )
   a[hcl] <- shrinkVector( data, 4^level, mode )
   a
}   

showHilbertImage <- function( mat,
      palettePos = colorRampPalette( c( "white", "red" ) )( 300 ),
      paletteNeg = colorRampPalette( c( "white", "blue" ) )( 300 ),
      maxPaletteValue = max( abs( mat ) ),
      mode = c( "lattice", "EBImage", "EBImage-batch" ) ) {
   mode <- match.arg( mode )
   if( mode == "lattice" )
      levelplot( mat,
         xlab = NULL, ylab = NULL, scales = list( draw=FALSE ),
         col.regions = c( rev(paletteNeg), palettePos ),
         at = c( seq( -maxPaletteValue, 0, length.out = length(paletteNeg) )[ -length(paletteNeg) ], 
            seq( 0, maxPaletteValue, length.ou = length(palettePos) ) ) )      
   else {
      if( ! require( "EBImage" ) ) {
         stop( "The 'EBImage' package is not installed. Hence, you cannot use the option useEBImage=TRUE." ) }
      matsc <- mat / maxPaletteValue
      matsc <- pmax( -1, pmin( 1, matsc ) )
      matsc <- ifelse( matsc >= 0,
         matsc * ( length( palettePos ) - 1 ) + 1,
         matsc * ( length( paletteNeg ) - 1 ) - 1 )
      matsc <- as.integer( matsc )
      palettePos <- col2rgb( palettePos )
      paletteNeg <- col2rgb( paletteNeg )
      img <- sapply( 1:3, function( cc )
         ifelse( matsc >= 0, 
            palettePos[ cc, pmax( 1, matsc ) ], 
            paletteNeg[ cc, pmax( 1, -matsc ) ] ) )
      `HilbertPlot` <- flip( rgbImage( 
         Image( array( img[,1]/255, dim = dim(mat) ) ), 
         Image( array( img[,2]/255, dim = dim(mat) ) ), 
         Image( array( img[,3]/255, dim = dim(mat) ) ) ) )      
      if( mode == "EBImage" ) {
         display( HilbertPlot )
         invisible( HilbertPlot ) }
      else
         HilbertPlot }
}      

makeRandomTestData <- function( len = 10000000, numPeaks=500 ) {
   y <- rep( 0, len )
   for( i in 1:numPeaks ) {
      mean <- runif( 1, max=len )
      sd <- rgamma( 1, shape=3 ) * len/300000 * 
         (1 + dnorm( mean, mean=len*.3, sd=len/30 )*len ) 
      mn <- max( 1, mean-6*sd )
      mx <- min( len, mean+6*sd ) 
      sgn <- sample( c( 1, -1 ), size = 1, prob = c( .7, .3 ) )
      y[ mn:mx ] <- y[ mn:mx ] +
         dnorm( mn:mx - mean, sd=sd ) * sd * rpois( 1, lambda=10 ) * 30 * sgn
   }
   y 
}
   

