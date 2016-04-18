#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_INTEGER( n_y );
  DATA_INTEGER( n_s );
  DATA_IVECTOR( s_i );
  DATA_VECTOR( y_i );

  // Parameters
  PARAMETER( x0 );
  PARAMETER( log_sdz ); //variability in site effect

  // Objective funcction
  Type jnll = 0;

  // Probability of data conditional on fixed and random effect values
  vector<Type> ypred_i(n_y);
  for( int i=0; i<n_y; i++){
    ypred_i(i) = x0; //now the distribution is log-normal
    jnll -= dnbinom(y_i(i), ypred_i(i), exp(log_sdz), true );
  }


  // Reporting
  Type sdz = exp(log_sdz);

  REPORT( sdz );
  REPORT( x0 );

  ADREPORT( sdz );
  return jnll;
}
