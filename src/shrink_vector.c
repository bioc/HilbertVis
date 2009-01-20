#include <limits.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Arith.h>

SEXP shrink_vector_int( SEXP vector, SEXP new_size, SEXP binning_mode ) 
{
   const double step = (double) LENGTH(vector) / INTEGER(new_size)[0];
   int curbin, nxtbin, i, j;
   SEXP res;

   if( INTEGER(binning_mode)[0] != 4 )
      PROTECT( res = allocVector( INTSXP, INTEGER(new_size)[0] ) );
   else
      PROTECT( res = allocVector( REALSXP, INTEGER(new_size)[0] ) );
   nxtbin = 0;
   for( i = 0; i < LENGTH(res); i++ ) {
      curbin = nxtbin;
      nxtbin = (int) round( step * (i+1) );
      if( nxtbin > LENGTH(vector) )
         nxtbin = LENGTH(vector);
      int val =  INTEGER(vector)[curbin];
      for( j = curbin+1; j < nxtbin; j++ )
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
         REAL( res )[ i ] = val / ( nxtbin - curbin );
   }
   UNPROTECT(1);
   return res;
}   
SEXP shrink_vector_double( SEXP vector, SEXP new_size, SEXP binning_mode ) 
{
   const double step = (double) LENGTH(vector) / INTEGER(new_size)[0];
   int curbin, nxtbin, i, j;
   SEXP res;

   PROTECT( res = allocVector( REALSXP, INTEGER(new_size)[0] ) );
   nxtbin = 0;
   for( i = 0; i < LENGTH(res); i++ ) {
      curbin = nxtbin;
      nxtbin = (int) round( step * (i+1) );
      if( nxtbin > LENGTH(vector) )
         nxtbin = LENGTH(vector);
      double val =  REAL(vector)[curbin];
      for( j = curbin+1; j < nxtbin; j++ )
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
         val /= ( nxtbin - curbin );
      REAL( res )[ i ] = val;
   }
   UNPROTECT(1);
   return res;
}   
