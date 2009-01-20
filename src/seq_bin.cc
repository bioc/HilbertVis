#include <R.h>
#include <Rinternals.h>

/* binning styles:
   1: maximum
   2: minimum
   3: abs max
   4: average */

extern "C" SEXP seq_bin_double( SEXP vector, SEXP bin_width, SEXP binning_mode ) 
{
   const int length = LENGTH(vector) / INTEGER(bin_width)[0] +
      ( LENGTH(vector) % INTEGER(bin_width)[0] ? 1 : 0 );
   Rprintf( "%d %d %d\n", LENGTH(vector), INTEGER(bin_width)[0], length );
   SEXP res = allocVector( REALSXP, length );
   PROTECT( res );
   for( int i = 0; i < LENGTH(res); i++ ) {
      int bin_start = i * INTEGER( bin_width )[ 0 ];
      double val = REAL(vector)[ bin_start ];
      int j;
      for( j = bin_start + 1; j < bin_start + INTEGER(bin_width)[0] && 
            j < LENGTH(vector); j++ )
         switch( INTEGER( binning_mode )[ 0 ] ) {
            case 1: 
               if( REAL( vector )[ j ] > val )
                  val = REAL( vector )[ j ];
               break;
            case 2: 
               if( REAL( vector )[ j ] < val )
                  val = REAL( vector )[ j ];
               break;
            case 3: 
               if( abs( REAL( vector )[ j ] ) > abs( val ) )
                  val = REAL( vector )[ j ];
               break;
            case 4:
               val += REAL( vector )[ j ];
               break;
            default:
               error( "Unknown binning mode." );
         }
      if( INTEGER( binning_mode )[ 0 ] == 4 )
         val /= ( j - bin_start );
      REAL( res )[ i ] = val;
   }
   UNPROTECT(1);
   return res;
}   

extern "C" SEXP seq_bin_int( SEXP vector, SEXP bin_width, SEXP binning_mode ) 
{
   const int length = LENGTH(vector) / INTEGER(bin_width)[0] +
      ( LENGTH(vector) % INTEGER(bin_width)[0] ? 1 : 0 );
   SEXP res;
   if( INTEGER( binning_mode )[ 0 ] != 4 )
      res = allocVector( INTSXP, length );
   else
      res = allocVector( REALSXP, length );
   PROTECT( res );
   for( int i = 0; i < LENGTH(res); i++ ) {
      int bin_start = i * INTEGER( bin_width )[ 0 ];
      int val = INTEGER(vector)[ bin_start ];
      int j;
      for( j = bin_start + 1; j < bin_start + INTEGER(bin_width)[0] && 
            j < LENGTH(vector); j++ )
         switch( INTEGER( binning_mode )[ 0 ] ) {
            case 1: 
               if( INTEGER( vector )[ j ] > val )
                  val = INTEGER( vector )[ j ];
               break;
            case 2: 
               if( INTEGER( vector )[ j ] < val )
                  val = INTEGER( vector )[ j ];
               break;
            case 3: 
               if( abs( INTEGER( vector )[ j ] ) > abs( val ) )
                  val = INTEGER( vector )[ j ];
               break;
            case 4:
               val += INTEGER( vector )[ j ];
               break;
            default:
               error( "Unknown binning mode." );
         }
      if( INTEGER( binning_mode )[ 0 ] != 4 )
         INTEGER( res )[ i ] = val;
      else
         REAL( res )[ i ] = (double ) val / ( j - bin_start );
   }
   UNPROTECT(1);
   return res;
}   

