#include <R.h>
#include <Rinternals.h>
#include "Endpoint.h"
#include <vector>
#include <set>
#include <algorithm>

/*
  What we require to prevent segfaults is the same as for
  interval_overlap.cpp. See details there. Everything should be checked by the
  calling code in R.

  For plotting without visual overlap, we do not care about endpoint closure,
  and we always open new intervals before closing old ones.

  When an interval opens, we assign it the minimal free interior option, if
  there is one; otherwise, we assign it the current count of open
  intervals. When an interval closes, if it does not have the current maximum y,
  its y gets added to the free_interior set.
*/




const int reduce_order[2][2][2] = {
  {{2,2},{1,1}}, // Target: {{ ), ] }, { (, [ }}      
  {{0,0},{0,0}}  // Query:  {{ ), ] }, { (, [ }}
};




extern "C"
{

  SEXP _plot_overlap(SEXP e, SEXP c, SEXP full) {

    // Load data and sort
    int n = nrows(e);
    bool full_bool = *LOGICAL(full); 
    Endpoints ep ( REAL(e), LOGICAL(c), n, false, full_bool );

    // Set sorting order, then sort
    Endpoint::set_state_array( reduce_order );
    sort( ep.begin(), ep.end() );

    // Process
    int i;
    int active_count = 0;
    std::set<int> free_interior;
    std::vector<int> y (n);    
    Endpoints::const_iterator it;    

    // Initialize to NA
    for ( i = 0; i < n; i++ ) y[i] = R_NaInt;

    for ( it = ep.begin(); it < ep.end(); it++ ) {
      if ( it->left ) {
	// Opening an interval
	if ( free_interior.size() > 0 ) {
	  y[ it->index ] = *free_interior.begin();
	  free_interior.erase( free_interior.begin() );
	}
	else y[ it->index ] = active_count;
	active_count++;
      }
      else{
	// Closing an interval
	active_count--;
	if ( y[ it->index ] < active_count + free_interior.size() )
	  free_interior.insert( y[ it->index ] );
      }
    }

    // Prepare and return result.
    SEXP result;

    PROTECT( result = allocVector( INTSXP, n ) );    
    copy( y.begin(), y.end(), INTEGER( result ) );
    UNPROTECT(1);
    return( result );    

  }

}
