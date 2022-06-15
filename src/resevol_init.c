#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP build_ownership(SEXP);
extern SEXP mine_gmatrix(SEXP, SEXP);
extern SEXP sim_farming(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"build_ownership",         (DL_FUNC) &build_ownership,     1},
    {"mine_gmatrix",            (DL_FUNC) &mine_gmatrix,        2},
    {"sim_farming",             (DL_FUNC) &sim_farming,         7},
    {NULL, NULL, 0}
};

void R_init_resevol(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}