#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void gen_fr_model_initmod_desolve(void *);
extern void gen_fr_model_rhs_dde(void *);
extern void gen_fr_model_rhs_desolve(void *);
extern void logistic_growth_model_initmod_desolve(void *);
extern void logistic_growth_model_rhs_dde(void *);
extern void logistic_growth_model_rhs_desolve(void *);
extern void rma_model_initmod_desolve(void *);
extern void rma_model_rhs_dde(void *);
extern void rma_model_rhs_desolve(void *);

/* .Call calls */
extern SEXP gen_fr_model_contents(SEXP);
extern SEXP gen_fr_model_create(SEXP);
extern SEXP gen_fr_model_initial_conditions(SEXP, SEXP);
extern SEXP gen_fr_model_metadata(SEXP);
extern SEXP gen_fr_model_rhs_r(SEXP, SEXP, SEXP);
extern SEXP gen_fr_model_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP gen_fr_model_set_user(SEXP, SEXP);
extern SEXP logistic_growth_model_contents(SEXP);
extern SEXP logistic_growth_model_create(SEXP);
extern SEXP logistic_growth_model_initial_conditions(SEXP, SEXP);
extern SEXP logistic_growth_model_metadata(SEXP);
extern SEXP logistic_growth_model_rhs_r(SEXP, SEXP, SEXP);
extern SEXP logistic_growth_model_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP logistic_growth_model_set_user(SEXP, SEXP);
extern SEXP rma_model_contents(SEXP);
extern SEXP rma_model_create(SEXP);
extern SEXP rma_model_initial_conditions(SEXP, SEXP);
extern SEXP rma_model_metadata(SEXP);
extern SEXP rma_model_rhs_r(SEXP, SEXP, SEXP);
extern SEXP rma_model_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP rma_model_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"gen_fr_model_initmod_desolve",          (DL_FUNC) &gen_fr_model_initmod_desolve,          1},
    {"gen_fr_model_rhs_dde",                  (DL_FUNC) &gen_fr_model_rhs_dde,                  1},
    {"gen_fr_model_rhs_desolve",              (DL_FUNC) &gen_fr_model_rhs_desolve,              1},
    {"logistic_growth_model_initmod_desolve", (DL_FUNC) &logistic_growth_model_initmod_desolve, 1},
    {"logistic_growth_model_rhs_dde",         (DL_FUNC) &logistic_growth_model_rhs_dde,         1},
    {"logistic_growth_model_rhs_desolve",     (DL_FUNC) &logistic_growth_model_rhs_desolve,     1},
    {"rma_model_initmod_desolve",             (DL_FUNC) &rma_model_initmod_desolve,             1},
    {"rma_model_rhs_dde",                     (DL_FUNC) &rma_model_rhs_dde,                     1},
    {"rma_model_rhs_desolve",                 (DL_FUNC) &rma_model_rhs_desolve,                 1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"gen_fr_model_contents",                    (DL_FUNC) &gen_fr_model_contents,                    1},
    {"gen_fr_model_create",                      (DL_FUNC) &gen_fr_model_create,                      1},
    {"gen_fr_model_initial_conditions",          (DL_FUNC) &gen_fr_model_initial_conditions,          2},
    {"gen_fr_model_metadata",                    (DL_FUNC) &gen_fr_model_metadata,                    1},
    {"gen_fr_model_rhs_r",                       (DL_FUNC) &gen_fr_model_rhs_r,                       3},
    {"gen_fr_model_set_initial",                 (DL_FUNC) &gen_fr_model_set_initial,                 4},
    {"gen_fr_model_set_user",                    (DL_FUNC) &gen_fr_model_set_user,                    2},
    {"logistic_growth_model_contents",           (DL_FUNC) &logistic_growth_model_contents,           1},
    {"logistic_growth_model_create",             (DL_FUNC) &logistic_growth_model_create,             1},
    {"logistic_growth_model_initial_conditions", (DL_FUNC) &logistic_growth_model_initial_conditions, 2},
    {"logistic_growth_model_metadata",           (DL_FUNC) &logistic_growth_model_metadata,           1},
    {"logistic_growth_model_rhs_r",              (DL_FUNC) &logistic_growth_model_rhs_r,              3},
    {"logistic_growth_model_set_initial",        (DL_FUNC) &logistic_growth_model_set_initial,        4},
    {"logistic_growth_model_set_user",           (DL_FUNC) &logistic_growth_model_set_user,           2},
    {"rma_model_contents",                       (DL_FUNC) &rma_model_contents,                       1},
    {"rma_model_create",                         (DL_FUNC) &rma_model_create,                         1},
    {"rma_model_initial_conditions",             (DL_FUNC) &rma_model_initial_conditions,             2},
    {"rma_model_metadata",                       (DL_FUNC) &rma_model_metadata,                       1},
    {"rma_model_rhs_r",                          (DL_FUNC) &rma_model_rhs_r,                          3},
    {"rma_model_set_initial",                    (DL_FUNC) &rma_model_set_initial,                    4},
    {"rma_model_set_user",                       (DL_FUNC) &rma_model_set_user,                       2},
    {NULL, NULL, 0}
};

void R_init_dynasymfitr(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
