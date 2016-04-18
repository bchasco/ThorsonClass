// Space time
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
	  DATA_VECTOR( Y_t );
	  DATA_NUMBER(S0);
	  
	  // Parameters
	  PARAMETER( a );
	  PARAMETER( b );
	  PARAMETER( lnsig_S );
	  PARAMETER( lnsig_y );
	  PARAMETER_VECTOR( u );

	  Type sig_S = exp(lnsig_S);
	  Type sig_y = exp(lnsig_y);

  	Type jnll = 0;
		int nt = Y_t.size();

		u(0) = S0;
  	for( int i=1; i<nt; i++){
			jnll -= dnorm(u(i),a + b*u[i-1],sig_S,true);
			jnll -= dnorm(Y_t(i),u(i),sig_y,true);
		}
	  return jnll;

}
