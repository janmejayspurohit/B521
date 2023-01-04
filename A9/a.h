void *r__t__closurer__m__cpsr__t__, *r__t__valr__m__closr__t__, *r__t__kr__m__closr__t__, *r__t__envr__m__cpsr__t__, *r__t__valr__m__envr__t__, *r__t__kr__ex__r__t__, *r__t__kr__m__cpsr__t__, *r__t__valr__m__applyr__m__kr__t__, *r__t__exprr__ex__r__t__, *r__t__envr__t__, *r__t__kr__t__;

void (*r__t__pcr__t__)();

struct expr;
typedef struct expr expr;
struct expr {
  enum {
    _const_expr,
    _var_expr,
    _if_expr,
    _mult_expr,
    _subr1_expr,
    _zero_expr,
    _letcc_expr,
    _throw_expr,
    _let_expr,
    _lambda_expr,
    _app_expr
  } tag;
  union {
    struct { void *_cexp; } _const;
    struct { void *_n; } _var;
    struct { void *_test; void *_conseq; void *_alt; } _if;
    struct { void *_nexpr1; void *_nexpr2; } _mult;
    struct { void *_nexp; } _subr1;
    struct { void *_nexp; } _zero;
    struct { void *_body; } _letcc;
    struct { void *_kexp; void *_vexp; } _throw;
    struct { void *_exp; void *_body; } _let;
    struct { void *_body; } _lambda;
    struct { void *_rator; void *_rand; } _app;
  } u;
};

void *exprr_const(void *cexp);
void *exprr_var(void *n);
void *exprr_if(void *test, void *conseq, void *alt);
void *exprr_mult(void *nexpr1, void *nexpr2);
void *exprr_subr1(void *nexp);
void *exprr_zero(void *nexp);
void *exprr_letcc(void *body);
void *exprr_throw(void *kexp, void *vexp);
void *exprr_let(void *exp, void *body);
void *exprr_lambda(void *body);
void *exprr_app(void *rator, void *rand);

struct clos;
typedef struct clos clos;
struct clos {
  enum {
    _maker__m__closure_clos
  } tag;
  union {
    struct { void *_body; void *_env; } _maker__m__closure;
  } u;
};

void *closr_maker__m__closure(void *body, void *env);

struct envr;
typedef struct envr envr;
struct envr {
  enum {
    _extendr__m__env_envr,
    _emptyr__m__env_envr
  } tag;
  union {
    struct { void *_envr__ex__; void *_valr__ex__; } _extendr__m__env;
    struct { char dummy; } _emptyr__m__env;
  } u;
};

void *envrr_extendr__m__env(void *envr__ex__, void *valr__ex__);
void *envrr_emptyr__m__env();

struct kt;
typedef struct kt kt;
struct kt {
  enum {
    _maker__m__kr__m__multr__m__vr2_kt,
    _maker__m__kr__m__multr__m__vr1_kt,
    _maker__m__kr__m__subr1_kt,
    _maker__m__kr__m__zero_kt,
    _maker__m__kr__m__if_kt,
    _maker__m__kr__m__throwr__m__vr__m__exp_kt,
    _maker__m__kr__m__throwr__m__kr__m__exp_kt,
    _maker__m__kr__m__let_kt,
    _maker__m__kr__m__rand_kt,
    _maker__m__kr__m__rator_kt,
    _emptyr__m__k_kt
  } tag;
  union {
    struct { void *_vr1r__ex__; void *_kr__ex__; } _maker__m__kr__m__multr__m__vr2;
    struct { void *_xr2r__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__kr__m__multr__m__vr1;
    struct { void *_kr__ex__; } _maker__m__kr__m__subr1;
    struct { void *_kr__ex__; } _maker__m__kr__m__zero;
    struct { void *_conseqr__ex__; void *_altr__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__kr__m__if;
    struct { void *_kr__ex__; } _maker__m__kr__m__throwr__m__vr__m__exp;
    struct { void *_vr__m__expr__ex__; void *_envr__ex__; } _maker__m__kr__m__throwr__m__kr__m__exp;
    struct { void *_bodyr__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__kr__m__let;
    struct { void *_valr__m__ofr__m__ratorr__ex__; void *_kr__ex__; } _maker__m__kr__m__rand;
    struct { void *_randr__ex__; void *_envr__ex__; void *_kr__ex__; } _maker__m__kr__m__rator;
    struct { void *_jumpout; } _emptyr__m__k;
  } u;
};

void *ktr_maker__m__kr__m__multr__m__vr2(void *vr1r__ex__, void *kr__ex__);
void *ktr_maker__m__kr__m__multr__m__vr1(void *xr2r__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_maker__m__kr__m__subr1(void *kr__ex__);
void *ktr_maker__m__kr__m__zero(void *kr__ex__);
void *ktr_maker__m__kr__m__if(void *conseqr__ex__, void *altr__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_maker__m__kr__m__throwr__m__vr__m__exp(void *kr__ex__);
void *ktr_maker__m__kr__m__throwr__m__kr__m__exp(void *vr__m__expr__ex__, void *envr__ex__);
void *ktr_maker__m__kr__m__let(void *bodyr__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_maker__m__kr__m__rand(void *valr__m__ofr__m__ratorr__ex__, void *kr__ex__);
void *ktr_maker__m__kr__m__rator(void *randr__ex__, void *envr__ex__, void *kr__ex__);
void *ktr_emptyr__m__k(void *jumpout);

void valuer__m__ofr__m__cps();
void applyr__m__env();
void applyr__m__closure();
void applyr__m__k();
int main();
int mount_tram();

struct _trstr;
typedef struct _trstr _trstr;
struct _trstr {
  jmp_buf *jmpbuf;
  int value;
};

