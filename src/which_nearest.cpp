#include "Endpoint.h"

#include <limits>
#include <vector>
#include <set>
#include <algorithm>

#include <R.h>
#include <Rinternals.h>

/*
  #### What we require to prevent segfaults:
  
  1. For Intervals_full objects, the endpoint and closure matrices must be of
  the same dimension, and have two columns each. For Intervals objects, we
  expect a closure vector of length 2.
  
  2. Endpoints must be type double, and closure must be R logical, which ends up
  as int in C++.
  
  The validity function for the classes should verify both of these properties,
  so we do no additional checks here, i.e., we assume that we've been passed
  valid objects.

  #### To-do 

  - Try hash_set instead of set, if it compiles OK.

  #### The "active" and "pending" sets

  - The q_active and t_active sets record indices of intervals which are
  currently open as we proceed through the list of points.

  - The q_pending set records the query interval (or intervals, in the case of
  ties) which has closed most recently to the left. If multiple target intervals
  come by before the next query interval is opened, each will need to be compare
  to the right endpoint of the intervals in q_pending. Once we open a new query
  interval, q_pending is cleared.

  - The t_pending set records indices of target intervals which have closed
  since the last time a query interval opened -- and distances to the left were
  therefor checked. Each of these target intervals needs to be checked against
  the next query interval to become active, as well as any additional query
  intervals that start at the same location. The t_pending set will be cleared
  when we get ready to add a new index to it but see that (i) a query interval
  has been opened since the last time we tried to add an index, and (ii) that
  pos has changed.

  the last closure of a query interval. When we open a new query interval,
  each of these target intervals needs to be compared to the new query
  interval's left endpoint. Once we do this -- and provided that the next query
  interval does not start in the same place -- then t_pending is cleared.

*/

const int overlap_order[2][2][2] = {
  {{1,5},{6,2}}, // Target: {{ ), ] }, { (, [ }}      
  {{0,4},{7,3}}  // Query:  {{ ), ] }, { (, [ }}
};

extern "C"
{

  SEXP _which_nearest(SEXP qe, SEXP te, SEXP qc, SEXP tc, SEXP q_full, SEXP t_full) {
    
    // Load data, combine
    int qn = Rf_nrows(qe);
    int tn = Rf_nrows(te);

    Endpoints q ( REAL(qe), LOGICAL(qc), qn, true, *LOGICAL(q_full) );
    Endpoints t ( REAL(te), LOGICAL(tc), tn, false, *LOGICAL(t_full) );

    double* q_right = REAL(qe) + qn;
    double* t_right = REAL(te) + tn;
    
    q.insert( q.end(), t.begin(), t.end() );

    // Set sorting order, then sort
    Endpoint::set_state_array( overlap_order );
    sort(q.begin(), q.end());

    // Process overlaps
    std::set<int> q_active, t_active;
    std::vector< std::set<int> > indices ( tn );
    
    // For distance_to_nearest and which_nearest
    double q_check_pos = std::numeric_limits<double>::infinity();
    std::set<int> q_pending, t_pending;
    std::vector<double> delta ( tn );
    std::vector< std::set<int> > which ( tn );
    double diff;
    
    // For looping
    Endpoints::const_iterator it;
    std::set<int>::iterator set_it;
    int i;

    // Initialize delta
    for ( i = 0; i < tn; i++ ) 
      delta[i] = std::numeric_limits<double>::infinity();

    for ( it = q.begin(); it < q.end(); it++ ) {
      if ( it->query ) {
    	if ( it->left ) {
	  // Query left
	  for( set_it = t_active.begin(); set_it != t_active.end(); set_it++ ) {
	    indices[ *set_it ].insert( it->index + 1 );
	    if ( delta[ *set_it ] > 0 ) which[ *set_it ].clear();
	    delta[ *set_it ] = 0;
	    which[ *set_it ].insert( it->index + 1 );
	  }
	  for( set_it = t_pending.begin(); set_it != t_pending.end(); set_it++ ) {
	    diff = it->pos - t_right[ *set_it ];
	    if ( delta[ *set_it ] == diff ) 
	      which[ *set_it ].insert( it->index + 1 );
	    if ( delta[ *set_it ] > diff ) {
	      which[ *set_it ].clear();
	      delta[ *set_it ] = diff;
	      which[ *set_it ].insert( it->index + 1 );
	    }
	  }
	  q_active.insert( it->index );
	  q_check_pos = it->pos;
 	  q_pending.clear();
    	}
    	else {
	  // Query right
	  q_active.erase( it->index );
//	  if ( q_right[ it->index ] > q_right[ *q_pending.begin() ] ) 
//	    q_pending.clear();
// EJP: trying to fix the clang-ASAN error,
// EJP: replace these to lines with
	  if (!q_pending.empty()) {
	    if (it->index < Rf_length(qe) && *q_pending.begin() < Rf_length(qe) ) {
	      if ( q_right[ it->index ] > q_right[ *q_pending.begin() ] )
	        q_pending.clear();
	    }
	  }
	  q_pending.insert( it->index );
	}
      }
      else {
    	if ( it->left ) {
	  // Target left
	  /*
	    Note that some care is required here. It is possible that there is
	    another interval at distance 0 to the left, but which, due to
	    endpoint closure, does not actually overlap the target interval we
	    are activating. In this case, q_active could be empty but we should
	    still set delta to 0 for this target interval and add the index of
	    the query interval immediately to the left to the which
	    set. Similarly, if there are both overlapping and non-overlapping
	    distance-zero query intervals, we want to return delta = 0 and the
	    indices for both types.
	  */
	  if ( q_active.size() > 0 ) {
	    delta[ it->index ] = 0;
	    for( set_it = q_active.begin(); set_it != q_active.end(); set_it++ ) {
	      indices[ it->index ].insert( *set_it + 1 );
	      which[ it->index ].insert( *set_it + 1 );
	    }
	  }
	  for ( set_it = q_pending.begin(); set_it != q_pending.end(); set_it++ ) {
	    diff = it->pos - q_right[ *set_it ];
	    if ( delta[ it->index ] == diff )
	      which[ it->index ].insert( *set_it + 1 );
	    if ( delta[ it->index ] > diff ) {
	      which[ it->index ].clear();
	      delta[ it->index ] = diff;
	      which[ it->index ].insert( *set_it + 1 );
	    }
	  }
    	  t_active.insert( it->index );	  
    	}
    	else {
	  // Target right
	  t_active.erase( it->index );
	  if ( it->pos > q_check_pos ) {
	    t_pending.clear();
	    q_check_pos = std::numeric_limits<double>::infinity();
	  }
	  t_pending.insert( it->index );
	}
      }
    }
    
    // Prepare and return result.
    SEXP result;

    PROTECT( result = Rf_allocVector( VECSXP, 3 ) );

    SET_VECTOR_ELT( result, 0, Rf_allocVector( REALSXP, tn ) ); // delta
    SET_VECTOR_ELT( result, 1, Rf_allocVector( VECSXP, tn ) ); // which
    SET_VECTOR_ELT( result, 2, Rf_allocVector( VECSXP, tn ) ); // which_overlap

    copy( 
	 delta.begin(), delta.end(),
	 REAL( VECTOR_ELT( result, 0 ) )
	  );
		    
    for( i = 0; i < tn; i++ ) {      
      SET_VECTOR_ELT( 
		     VECTOR_ELT( result, 1 ), i, 
		     Rf_allocVector( INTSXP, which[i].size() ) 
		      );
      copy( 
	   which[i].begin(), which[i].end(), 
	   INTEGER( VECTOR_ELT( VECTOR_ELT( result, 1 ), i ) )
	    );
      SET_VECTOR_ELT( 
		     VECTOR_ELT( result, 2 ), i, 
		     Rf_allocVector( INTSXP, indices[i].size() ) 
		      );
      copy( 
	   indices[i].begin(), indices[i].end(), 
	   INTEGER( VECTOR_ELT( VECTOR_ELT( result, 2 ), i ) )
	    );
    }

    UNPROTECT(1);
    return( result );    
  
  }

}
