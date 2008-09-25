#include <R.h>
#include <Rinternals.h>

typedef struct {
   int x, y;
} coord;    

coord hilbert( long long t, int lv ) 
{
   long long q, r;
   coord c;
   if( lv == 0 )
      return (coord) { 0, 0 };
   else {
      r = 1ll << (2*(lv-1));
      q = t / r ;      
      c = hilbert( t % r, lv - 1 );
      switch( q ) {
         case 0: { return (coord) { c.y, c.x }; }
         case 1: { return (coord) { c.x, c.y + ( 1l << (lv-1) ) }; }
         case 2: { return (coord) { c.x + ( 1l << (lv-1) ), c.y + ( 1l << (lv-1) ) }; }
         case 3: { return (coord) { (1l<<lv) - 1 - c.y, ( 1l << (lv-1) ) - 1 - c.x }; }
         default: abort( );
      }
   }
   return (coord) {-1,-1}; /* dummy statement, never reached */
}      

SEXP hilbertCurveR( SEXP t, SEXP lv )
{
   coord c = hilbert( INTEGER(t)[0], INTEGER(lv)[0] );
   SEXP res;
   res = Rf_allocVector( INTSXP, 2 );
   INTEGER(res)[0] = c.x;
   INTEGER(res)[1] = c.y;
   return res;
}

