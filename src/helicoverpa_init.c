#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP mine_gmatrix(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"mine_gmatrix",            (DL_FUNC) &mine_gmatrix,        2},
    {NULL, NULL, 0}
};

void R_init_helicoverpa(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}