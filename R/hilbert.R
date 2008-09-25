hilbertDefaultPalette <- function( size, asArray=TRUE ) {
   a <- 
   col2rgb( c( 
      colorRampPalette( c("white","blue") )(size/4), 
      colorRampPalette( c("blue","red") )(size/4*3) ) )
   if( asArray )
      a
   else
      apply( a, 2, function(x) 
         rgb( x["red"], x["green"], x["blue"], maxColorValue=255 ) ) }


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
   pushViewport( viewport( x=.5, y=.5, width=1, height=1, default.unit="snpc" ) )
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

hilbertImage <- function( data, level=9, forEBImage=FALSE ) {
   hc <- hilbertCurve( level )
   hcl <- ( 2^level-1 - hc$y ) * 2^level + hc$x + 1
   a <- matrix( NA_real_, nrow=2^level, ncol=2^level )
   a[hcl] <- shrinkVector( data, 4^level )
   
   if( ! forEBImage )
      a
   else {
      if( ! require( "EBImage" ) ) {
         stop( "The 'EBImage' package is not installed. Hence, you cannot use the option forEBImage=TRUE." )
      }
      pal = hilbertDefaultPalette( min( max(a), 1000 )  )
      flip( rgbImage( 
	 Image( array( pal[ 1, a+1 ]/255, dim = dim(a) ) ), 
	 Image( array( pal[ 2, a+1 ]/255, dim = dim(a) ) ), 
	 Image( array( pal[ 3, a+1 ]/255, dim = dim(a) ) ) ) )      
   }	 
}   

makeRandomTestData <- function( len = 10000000, numPeaks=500 ) {
   y <- rep( 0, len )
   for( i in 1:numPeaks ) {
      mean <- runif( 1, max=len )
      sd <- rgamma( 1, shape=3 ) * len/300000 * 
         (1 + dnorm( mean, mean=len*.3, sd=len/30 )*len ) 
      mn <- max( 1, mean-6*sd )
      mx <- min( len, mean+6*sd ) 
      y[ mn:mx ] <- y[ mn:mx ] +
         dnorm( mn:mx - mean, sd=sd ) * sd * rpois( 1, lambda=10 ) * 30
   }	 
   y	 
}	
   

