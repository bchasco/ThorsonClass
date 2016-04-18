// Space time
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
	  DATA_VECTOR( Y_t );

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

  	for( int i=1; i<nt; i++){
			jnll -= dnorm(u(i),a + b*u[i-1],sig_S,true);
		}
  	for( int i=0; i<nt; i++)
  	{
			jnll -= dnorm(Y_t(i),u(i),sig_y,true);
		}

	  ADREPORT(a)
	  ADREPORT(b)
	  ADREPORT(sig_S)
	  ADREPORT(sig_y)

	  return jnll;

}
