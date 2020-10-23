#define R_NO_REMAP
#include <R_ext/Rdynload.h>
#include <R.h>
#include <Rinternals.h>

#include <VineCopula/evCopula.h>


extern void Tawn2(double* t, int* n, double* par, double* par2, double* par3, double* out) {
  static SEXP(*fun)(double*, int*, double*, double*, double*, double*) = NULL;
  if (fun == NULL)
    fun = (SEXP(*)(double*, int*, double*, double*, double*, double*)) R_GetCCallable("VineCopula", "Tawn2");
  fun(t, n, par, par2, par3, out);
}

static const R_CMethodDef CEntries[] = {
  {"Tawn2", (DL_FUNC) &Tawn2, 6},
  {NULL, NULL, 0}
};

void R_init_VC2copula(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
