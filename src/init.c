#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


extern SEXP _which_nearest(SEXP qe, SEXP te, SEXP qc, SEXP tc, SEXP q_full, SEXP t_full);
extern SEXP _plot_overlap(SEXP e, SEXP c, SEXP full);
extern SEXP _reduce(SEXP e, SEXP c, SEXP full);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

const static R_CallMethodDef R_CallDef[] = {
	CALLDEF(_which_nearest, 6),
	CALLDEF(_plot_overlap, 3),
	CALLDEF(_reduce, 3),
	{NULL, NULL, 0}
};

void
// attribute_visible  // optional
R_init_intervals(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
