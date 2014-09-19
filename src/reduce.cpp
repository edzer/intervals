#include <R.h>
#include <Rinternals.h>
#include "Endpoint.h"
#include <vector>
#include <algorithm>

/*
  What we require to prevent segfaults is the same as for
  interval_overlap.cpp. See details there. Everything should be checked by the
  calling code in R.
*/




const int reduce_order[2][2][2] = {
  {{2,4},{3,1}}, // Target: {{ ), ] }, { (, [ }}      
  {{0,0},{0,0}}  // Query:  {{ ), ] }, { (, [ }}
};




extern "C"
{

  SEXP _reduce(SEXP e, SEXP c, SEXP full) {

    // Load data and sort
    int n = nrows(e);
    bool full_bool = *LOGICAL(full); 
    Endpoints ep ( REAL(e), LOGICAL(c), n, false, full_bool );

    // Set sorting order, then sort
    Endpoint::set_state_array( reduce_order );
    sort( ep.begin(), ep.end() );

    // Process
    int score = 0;
    std::vector<double> start, end;
    std::vector<int> start_c, end_c;
    Endpoints::const_iterator it;    

    for ( it = ep.begin(); it < ep.end(); it++ ) {
      if ( score == 0 ) {
	if ( !it->left ) 
	  error("Internal error: unexpected endpoint type when score = 0.");
	start.push_back( it->pos );
	if ( full_bool ) start_c.push_back( (int) it->closed );
      }
      score += ( it->left ? +1 : -1 );
      if ( score == 0 ) {
	if ( it->left ) 
	  error("Internal error: unexpected endpoint type when score = 0.");
	end.push_back( it->pos );
	if ( full_bool ) end_c.push_back( (int) it->closed );
      }
    }

    if ( start.size() != end.size() )
      error("Internal error: mismatched start and end endpoint sets.");

    // Prepare and return result.
    SEXP result;

    PROTECT( result = allocVector( VECSXP, 2 ) );    

    SET_VECTOR_ELT( result, 0, allocMatrix( REALSXP, start.size(), 2 ) );
    copy( start.begin(), start.end(), REAL( VECTOR_ELT( result, 0 ) ) );
    copy( 
	 end.begin(), end.end(),
	 REAL( VECTOR_ELT( result, 0 ) ) + start.size() 
	  );

    if ( full_bool ) {
      SET_VECTOR_ELT( result, 1, allocMatrix( LGLSXP, start.size(), 2 ) );
      copy( 
    	   start_c.begin(), start_c.end(),
    	   LOGICAL( VECTOR_ELT( result, 1 ) )
    	    );
      copy( 
    	   end_c.begin(), end_c.end(),
    	   LOGICAL( VECTOR_ELT( result, 1 ) ) + start.size()
    	    );
    }
    else {
      SET_VECTOR_ELT( result, 1, allocVector( LGLSXP, 2 ) );
      LOGICAL( VECTOR_ELT( result, 1 ) )[0] = LOGICAL(c)[0];
      LOGICAL( VECTOR_ELT( result, 1 ) )[1] = LOGICAL(c)[1];
    }
    
    UNPROTECT(1);
    return( result );    

  }

}
