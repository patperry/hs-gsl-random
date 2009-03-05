#include "gsl/gsl_qrng.h"

unsigned int gsl_qrng_get_dimension(gsl_qrng* p_qrng)
{
  return p_qrng->dimension;
}

unsigned int gsl_qrng_get_max_dimension(gsl_qrng_type* p_qrng_type)
{
  return p_qrng_type->max_dimension;
}

const gsl_qrng_type* gsl_qrng_get_niederreiter_2()
{
  return gsl_qrng_niederreiter_2;
}

const gsl_qrng_type* gsl_qrng_get_sobol()
{
  return gsl_qrng_sobol;
}

const gsl_qrng_type* gsl_qrng_get_halton()
{
  return gsl_qrng_halton;
}

const gsl_qrng_type* gsl_qrng_get_reversehalton()
{
  return gsl_qrng_reversehalton;
}
