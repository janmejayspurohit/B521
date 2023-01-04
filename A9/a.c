#include <setjmp.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "a.h"

void *ktr_maker__m__kr__m__multr__m__vr2(void *vr1r__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__multr__m__vr2_kt;
  _data->u._maker__m__kr__m__multr__m__vr2._vr1r__ex__ = vr1r__ex__;
  _data->u._maker__m__kr__m__multr__m__vr2._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__multr__m__vr1(void *xr2r__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__multr__m__vr1_kt;
  _data->u._maker__m__kr__m__multr__m__vr1._xr2r__ex__ = xr2r__ex__;
  _data->u._maker__m__kr__m__multr__m__vr1._envr__ex__ = envr__ex__;
  _data->u._maker__m__kr__m__multr__m__vr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__subr1(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__subr1_kt;
  _data->u._maker__m__kr__m__subr1._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__zero(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__zero_kt;
  _data->u._maker__m__kr__m__zero._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__if(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__if_kt;
  _data->u._maker__m__kr__m__if._conseqr__ex__ = conseqr__ex__;
  _data->u._maker__m__kr__m__if._altr__ex__ = altr__ex__;
  _data->u._maker__m__kr__m__if._envr__ex__ = envr__ex__;
  _data->u._maker__m__kr__m__if._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__throwr__m__vr__m__exp(void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__throwr__m__vr__m__exp_kt;
  _data->u._maker__m__kr__m__throwr__m__vr__m__exp._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__throwr__m__kr__m__exp(void *vr__m__expr__ex__, void *envr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__throwr__m__kr__m__exp_kt;
  _data->u._maker__m__kr__m__throwr__m__kr__m__exp._vr__m__expr__ex__ = vr__m__expr__ex__;
  _data->u._maker__m__kr__m__throwr__m__kr__m__exp._envr__ex__ = envr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__let(void *bodyr__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__let_kt;
  _data->u._maker__m__kr__m__let._bodyr__ex__ = bodyr__ex__;
  _data->u._maker__m__kr__m__let._envr__ex__ = envr__ex__;
  _data->u._maker__m__kr__m__let._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__rand(void *valr__m__ofr__m__ratorr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__rand_kt;
  _data->u._maker__m__kr__m__rand._valr__m__ofr__m__ratorr__ex__ = valr__m__ofr__m__ratorr__ex__;
  _data->u._maker__m__kr__m__rand._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_maker__m__kr__m__rator(void *randr__ex__, void *envr__ex__, void *kr__ex__) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__kr__m__rator_kt;
  _data->u._maker__m__kr__m__rator._randr__ex__ = randr__ex__;
  _data->u._maker__m__kr__m__rator._envr__ex__ = envr__ex__;
  _data->u._maker__m__kr__m__rator._kr__ex__ = kr__ex__;
  return (void *)_data;
}

void *ktr_emptyr__m__k(void *jumpout) {
kt* _data = (kt*)malloc(sizeof(kt));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__k_kt;
  _data->u._emptyr__m__k._jumpout = jumpout;
  return (void *)_data;
}

void *envrr_extendr__m__env(void *envr__ex__, void *valr__ex__) {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _extendr__m__env_envr;
  _data->u._extendr__m__env._envr__ex__ = envr__ex__;
  _data->u._extendr__m__env._valr__ex__ = valr__ex__;
  return (void *)_data;
}

void *envrr_emptyr__m__env() {
envr* _data = (envr*)malloc(sizeof(envr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _emptyr__m__env_envr;
  return (void *)_data;
}

void *closr_maker__m__closure(void *body, void *env) {
clos* _data = (clos*)malloc(sizeof(clos));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _maker__m__closure_clos;
  _data->u._maker__m__closure._body = body;
  _data->u._maker__m__closure._env = env;
  return (void *)_data;
}

void *exprr_const(void *cexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _const_expr;
  _data->u._const._cexp = cexp;
  return (void *)_data;
}

void *exprr_var(void *n) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _var_expr;
  _data->u._var._n = n;
  return (void *)_data;
}

void *exprr_if(void *test, void *conseq, void *alt) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _if_expr;
  _data->u._if._test = test;
  _data->u._if._conseq = conseq;
  _data->u._if._alt = alt;
  return (void *)_data;
}

void *exprr_mult(void *nexpr1, void *nexpr2) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _mult_expr;
  _data->u._mult._nexpr1 = nexpr1;
  _data->u._mult._nexpr2 = nexpr2;
  return (void *)_data;
}

void *exprr_subr1(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _subr1_expr;
  _data->u._subr1._nexp = nexp;
  return (void *)_data;
}

void *exprr_zero(void *nexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _zero_expr;
  _data->u._zero._nexp = nexp;
  return (void *)_data;
}

void *exprr_letcc(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _letcc_expr;
  _data->u._letcc._body = body;
  return (void *)_data;
}

void *exprr_throw(void *kexp, void *vexp) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _throw_expr;
  _data->u._throw._kexp = kexp;
  _data->u._throw._vexp = vexp;
  return (void *)_data;
}

void *exprr_let(void *exp, void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _let_expr;
  _data->u._let._exp = exp;
  _data->u._let._body = body;
  return (void *)_data;
}

void *exprr_lambda(void *body) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _lambda_expr;
  _data->u._lambda._body = body;
  return (void *)_data;
}

void *exprr_app(void *rator, void *rand) {
expr* _data = (expr*)malloc(sizeof(expr));
if(!_data) {
  fprintf(stderr, "Out of memory\n");
  exit(1);
}
  _data->tag = _app_expr;
  _data->u._app._rator = rator;
  _data->u._app._rand = rand;
  return (void *)_data;
}

int main()
{
r__t__envr__t__ = (void *)envrr_emptyr__m__env();
r__t__exprr__ex__r__t__ = (void *)exprr_let(exprr_lambda(exprr_lambda(exprr_if(exprr_zero(exprr_var((void *)0)),exprr_const((void *)1),exprr_mult(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_subr1(exprr_var((void *)0))))))),exprr_mult(exprr_letcc(exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_throw(exprr_var((void *)0),exprr_app(exprr_app(exprr_var((void *)1),exprr_var((void *)1)),exprr_const((void *)4))))),exprr_const((void *)5)));
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
mount_tram();
printf("Output: %d\n", (int)r__t__valr__m__applyr__m__kr__t__);}

void applyr__m__k()
{
kt* _c = (kt*)r__t__kr__m__cpsr__t__;
switch (_c->tag) {
case _maker__m__kr__m__multr__m__vr2_kt: {
void *vr1r__ex__ = _c->u._maker__m__kr__m__multr__m__vr2._vr1r__ex__;
void *kr__ex__ = _c->u._maker__m__kr__m__multr__m__vr2._kr__ex__;
r__t__kr__m__cpsr__t__ = (void *)kr__ex__;
r__t__valr__m__applyr__m__kr__t__ = (void *)(void *)((int)vr1r__ex__ * (int)r__t__valr__m__applyr__m__kr__t__);
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__kr__m__multr__m__vr1_kt: {
void *xr2r__ex__ = _c->u._maker__m__kr__m__multr__m__vr1._xr2r__ex__;
void *envr__ex__ = _c->u._maker__m__kr__m__multr__m__vr1._envr__ex__;
void *kr__ex__ = _c->u._maker__m__kr__m__multr__m__vr1._kr__ex__;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__multr__m__vr2(r__t__valr__m__applyr__m__kr__t__,kr__ex__);
r__t__exprr__ex__r__t__ = (void *)xr2r__ex__;
r__t__envr__t__ = (void *)envr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__kr__m__subr1_kt: {
void *kr__ex__ = _c->u._maker__m__kr__m__subr1._kr__ex__;
r__t__kr__m__cpsr__t__ = (void *)kr__ex__;
r__t__valr__m__applyr__m__kr__t__ = (void *)(void *)((int)r__t__valr__m__applyr__m__kr__t__ - 1);
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__kr__m__zero_kt: {
void *kr__ex__ = _c->u._maker__m__kr__m__zero._kr__ex__;
r__t__kr__m__cpsr__t__ = (void *)kr__ex__;
r__t__valr__m__applyr__m__kr__t__ = (void *)(r__t__valr__m__applyr__m__kr__t__ == 0);
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__kr__m__if_kt: {
void *conseqr__ex__ = _c->u._maker__m__kr__m__if._conseqr__ex__;
void *altr__ex__ = _c->u._maker__m__kr__m__if._altr__ex__;
void *envr__ex__ = _c->u._maker__m__kr__m__if._envr__ex__;
void *kr__ex__ = _c->u._maker__m__kr__m__if._kr__ex__;
r__t__kr__t__ = (void *)kr__ex__;
r__t__exprr__ex__r__t__ = (void *)(r__t__valr__m__applyr__m__kr__t__ ? conseqr__ex__ : altr__ex__);
r__t__envr__t__ = (void *)envr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__kr__m__throwr__m__vr__m__exp_kt: {
void *kr__ex__ = _c->u._maker__m__kr__m__throwr__m__vr__m__exp._kr__ex__;
r__t__kr__m__cpsr__t__ = (void *)kr__ex__;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _maker__m__kr__m__throwr__m__kr__m__exp_kt: {
void *vr__m__expr__ex__ = _c->u._maker__m__kr__m__throwr__m__kr__m__exp._vr__m__expr__ex__;
void *envr__ex__ = _c->u._maker__m__kr__m__throwr__m__kr__m__exp._envr__ex__;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__throwr__m__vr__m__exp(r__t__valr__m__applyr__m__kr__t__);
r__t__exprr__ex__r__t__ = (void *)vr__m__expr__ex__;
r__t__envr__t__ = (void *)envr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__kr__m__let_kt: {
void *bodyr__ex__ = _c->u._maker__m__kr__m__let._bodyr__ex__;
void *envr__ex__ = _c->u._maker__m__kr__m__let._envr__ex__;
void *kr__ex__ = _c->u._maker__m__kr__m__let._kr__ex__;
r__t__kr__t__ = (void *)kr__ex__;
r__t__exprr__ex__r__t__ = (void *)bodyr__ex__;
r__t__envr__t__ = (void *)envrr_extendr__m__env(envr__ex__,r__t__valr__m__applyr__m__kr__t__);
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _maker__m__kr__m__rand_kt: {
void *valr__m__ofr__m__ratorr__ex__ = _c->u._maker__m__kr__m__rand._valr__m__ofr__m__ratorr__ex__;
void *kr__ex__ = _c->u._maker__m__kr__m__rand._kr__ex__;
r__t__kr__m__closr__t__ = (void *)kr__ex__;
r__t__closurer__m__cpsr__t__ = (void *)valr__m__ofr__m__ratorr__ex__;
r__t__valr__m__closr__t__ = (void *)r__t__valr__m__applyr__m__kr__t__;
r__t__pcr__t__ = &applyr__m__closure;
break; }
case _maker__m__kr__m__rator_kt: {
void *randr__ex__ = _c->u._maker__m__kr__m__rator._randr__ex__;
void *envr__ex__ = _c->u._maker__m__kr__m__rator._envr__ex__;
void *kr__ex__ = _c->u._maker__m__kr__m__rator._kr__ex__;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__rand(r__t__valr__m__applyr__m__kr__t__,kr__ex__);
r__t__exprr__ex__r__t__ = (void *)randr__ex__;
r__t__envr__t__ = (void *)envr__ex__;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _emptyr__m__k_kt: {
void *jumpout = _c->u._emptyr__m__k._jumpout;
_trstr *trstr = (_trstr *)jumpout;
longjmp(*trstr->jmpbuf, 1);
break; }
}
}

void applyr__m__closure()
{
clos* _c = (clos*)r__t__closurer__m__cpsr__t__;
switch (_c->tag) {
case _maker__m__closure_clos: {
void *body = _c->u._maker__m__closure._body;
void *env = _c->u._maker__m__closure._env;
r__t__kr__t__ = (void *)r__t__kr__m__closr__t__;
r__t__exprr__ex__r__t__ = (void *)body;
r__t__envr__t__ = (void *)envrr_extendr__m__env(env,r__t__valr__m__closr__t__);
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

void applyr__m__env()
{
envr* _c = (envr*)r__t__envr__m__cpsr__t__;
switch (_c->tag) {
case _extendr__m__env_envr: {
void *envr__ex__ = _c->u._extendr__m__env._envr__ex__;
void *valr__ex__ = _c->u._extendr__m__env._valr__ex__;
if((r__t__valr__m__envr__t__ == 0)) {
  r__t__kr__m__cpsr__t__ = (void *)r__t__kr__ex__r__t__;
r__t__valr__m__applyr__m__kr__t__ = (void *)valr__ex__;
r__t__pcr__t__ = &applyr__m__k;

} else {
  r__t__envr__m__cpsr__t__ = (void *)envr__ex__;
r__t__valr__m__envr__t__ = (void *)(void *)((int)r__t__valr__m__envr__t__ - 1);
r__t__pcr__t__ = &applyr__m__env;

}
break; }
case _emptyr__m__env_envr: {
fprintf(stderr, "unbound identifier");
 exit(1);
break; }
}
}

void valuer__m__ofr__m__cps()
{
expr* _c = (expr*)r__t__exprr__ex__r__t__;
switch (_c->tag) {
case _const_expr: {
void *exprr1 = _c->u._const._cexp;
r__t__kr__m__cpsr__t__ = (void *)r__t__kr__t__;
r__t__valr__m__applyr__m__kr__t__ = (void *)exprr1;
r__t__pcr__t__ = &applyr__m__k;
break; }
case _mult_expr: {
void *xr1 = _c->u._mult._nexpr1;
void *xr2 = _c->u._mult._nexpr2;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__multr__m__vr1(xr2,r__t__envr__t__,r__t__kr__t__);
r__t__exprr__ex__r__t__ = (void *)xr1;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _subr1_expr: {
void *x = _c->u._subr1._nexp;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__subr1(r__t__kr__t__);
r__t__exprr__ex__r__t__ = (void *)x;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _zero_expr: {
void *x = _c->u._zero._nexp;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__zero(r__t__kr__t__);
r__t__exprr__ex__r__t__ = (void *)x;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _if_expr: {
void *test = _c->u._if._test;
void *conseq = _c->u._if._conseq;
void *alt = _c->u._if._alt;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__if(conseq,alt,r__t__envr__t__,r__t__kr__t__);
r__t__exprr__ex__r__t__ = (void *)test;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _letcc_expr: {
void *body = _c->u._letcc._body;
r__t__exprr__ex__r__t__ = (void *)body;
r__t__envr__t__ = (void *)envrr_extendr__m__env(r__t__envr__t__,r__t__kr__t__);
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _throw_expr: {
void *kr__m__exp = _c->u._throw._kexp;
void *vr__m__exp = _c->u._throw._vexp;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__throwr__m__kr__m__exp(vr__m__exp,r__t__envr__t__);
r__t__exprr__ex__r__t__ = (void *)kr__m__exp;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _let_expr: {
void *e = _c->u._let._exp;
void *body = _c->u._let._body;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__let(body,r__t__envr__t__,r__t__kr__t__);
r__t__exprr__ex__r__t__ = (void *)e;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
case _var_expr: {
void *y = _c->u._var._n;
r__t__kr__ex__r__t__ = (void *)r__t__kr__t__;
r__t__envr__m__cpsr__t__ = (void *)r__t__envr__t__;
r__t__valr__m__envr__t__ = (void *)y;
r__t__pcr__t__ = &applyr__m__env;
break; }
case _lambda_expr: {
void *body = _c->u._lambda._body;
r__t__kr__m__cpsr__t__ = (void *)r__t__kr__t__;
r__t__valr__m__applyr__m__kr__t__ = (void *)closr_maker__m__closure(body,r__t__envr__t__);
r__t__pcr__t__ = &applyr__m__k;
break; }
case _app_expr: {
void *rator = _c->u._app._rator;
void *rand = _c->u._app._rand;
r__t__kr__t__ = (void *)ktr_maker__m__kr__m__rator(rand,r__t__envr__t__,r__t__kr__t__);
r__t__exprr__ex__r__t__ = (void *)rator;
r__t__pcr__t__ = &valuer__m__ofr__m__cps;
break; }
}
}

int mount_tram ()
{
srand (time (NULL));
jmp_buf jb;
_trstr trstr;
void *dismount;
int _status = setjmp(jb);
trstr.jmpbuf = &jb;
dismount = &trstr;
if(!_status) {
r__t__kr__t__= (void *)ktr_emptyr__m__k(dismount);
for(;;) {
r__t__pcr__t__();
}
}
return 0;
}
