import sys
from enum import Enum
from greenlet import greenlet
from typing import Any, Callable

# Define the registers
g__closure_cps_: object = None
g__val_clos_: object = None
g__k_clos_: object = None
g__env_cps_: object = None
g__val_env_: object = None
g__k_cap_: object = None
g__k_cps_: object = None
g__val_apply_k_: object = None
g__expr_cap_: object = None
g__env_: object = None
g__k_: object = None

# Define the program counter
g__pc_ : Callable[[], Any] = None

# Define the dismount greenlet
_dismount_blank = None

# Define the union classes
class union_t(object):
    class expr(Enum):
        const = 0
        var = 1
        _if = 2
        mult = 3
        subr1 = 4
        zero = 5
        letcc = 6
        throw = 7
        let = 8
        _lambda = 9
        app = 10

    class clos(Enum):
        make_closure = 0

    class envr(Enum):
        extend_env = 0
        empty_env = 1

    class kt(Enum):
        make_k_mult_vr2 = 0
        make_k_mult_vr1 = 1
        make_k_subr1 = 2
        make_k_zero = 3
        make_k_if = 4
        make_k_throw_v_exp = 5
        make_k_throw_k_exp = 6
        make_k_let = 7
        make_k_rand = 8
        make_k_rator = 9
        empty_k = 10

    def __init__(self, type: Enum, **kwargs):
        self.type = type
        for key in kwargs:
            setattr(self, key, kwargs[key])


# Union functions
def kt_make_k_mult_vr2(vr1_cap, k_cap):
    return union_t(union_t.kt.make_k_mult_vr2, 
            vr1_cap=vr1_cap, 
            k_cap=k_cap)

def kt_make_k_mult_vr1(xr2_cap, env_cap, k_cap):
    return union_t(union_t.kt.make_k_mult_vr1, 
            xr2_cap=xr2_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_make_k_subr1(k_cap):
    return union_t(union_t.kt.make_k_subr1, 
            k_cap=k_cap)

def kt_make_k_zero(k_cap):
    return union_t(union_t.kt.make_k_zero, 
            k_cap=k_cap)

def kt_make_k_if(conseq_cap, alt_cap, env_cap, k_cap):
    return union_t(union_t.kt.make_k_if, 
            conseq_cap=conseq_cap, 
            alt_cap=alt_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_make_k_throw_v_exp(k_cap):
    return union_t(union_t.kt.make_k_throw_v_exp, 
            k_cap=k_cap)

def kt_make_k_throw_k_exp(v_exp_cap, env_cap):
    return union_t(union_t.kt.make_k_throw_k_exp, 
            v_exp_cap=v_exp_cap, 
            env_cap=env_cap)

def kt_make_k_let(body_cap, env_cap, k_cap):
    return union_t(union_t.kt.make_k_let, 
            body_cap=body_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_make_k_rand(val_of_rator_cap, k_cap):
    return union_t(union_t.kt.make_k_rand, 
            val_of_rator_cap=val_of_rator_cap, 
            k_cap=k_cap)

def kt_make_k_rator(rand_cap, env_cap, k_cap):
    return union_t(union_t.kt.make_k_rator, 
            rand_cap=rand_cap, 
            env_cap=env_cap, 
            k_cap=k_cap)

def kt_empty_k(jumpout):
    return union_t(union_t.kt.empty_k, 
            jumpout=jumpout)

def envr_extend_env(env_cap, val_cap):
    return union_t(union_t.envr.extend_env, 
            env_cap=env_cap, 
            val_cap=val_cap)

def envr_empty_env():
    return union_t(union_t.envr.empty_env, )

def clos_make_closure(body, env):
    return union_t(union_t.clos.make_closure, 
            body=body, 
            env=env)

def expr_const(cexp):
    return union_t(union_t.expr.const, 
            cexp=cexp)

def expr_var(n):
    return union_t(union_t.expr.var, 
            n=n)

def expr_if(test, conseq, alt):
    return union_t(union_t.expr._if, 
            test=test, 
            conseq=conseq, 
            alt=alt)

def expr_mult(nexpr1, nexpr2):
    return union_t(union_t.expr.mult, 
            nexpr1=nexpr1, 
            nexpr2=nexpr2)

def expr_subr1(nexp):
    return union_t(union_t.expr.subr1, 
            nexp=nexp)

def expr_zero(nexp):
    return union_t(union_t.expr.zero, 
            nexp=nexp)

def expr_letcc(body):
    return union_t(union_t.expr.letcc, 
            body=body)

def expr_throw(kexp, vexp):
    return union_t(union_t.expr.throw, 
            kexp=kexp, 
            vexp=vexp)

def expr_let(exp, body):
    return union_t(union_t.expr.let, 
            exp=exp, 
            body=body)

def expr_lambda(body):
    return union_t(union_t.expr._lambda, 
            body=body)

def expr_app(rator, rand):
    return union_t(union_t.expr.app, 
            rator=rator, 
            rand=rand)

# Generate functions
def apply_k():
    global g__pc_
    global g__env_
    global g__expr_cap_
    global g__k_
    global g__val_apply_k_
    global g__val_clos_
    global g__closure_cps_
    global g__k_clos_
    global g__k_cps_

    match g__k_cps_.type:
        case union_t.kt.make_k_mult_vr2:
            vr1_cap = g__k_cps_.vr1_cap
            k_cap = g__k_cps_.k_cap
            g__k_cps_ = k_cap
            g__val_apply_k_ = vr1_cap * g__val_apply_k_
            g__pc_ = apply_k

        case union_t.kt.make_k_mult_vr1:
            xr2_cap = g__k_cps_.xr2_cap
            env_cap = g__k_cps_.env_cap
            k_cap = g__k_cps_.k_cap
            g__k_ = kt_make_k_mult_vr2(g__val_apply_k_, k_cap)
            g__expr_cap_ = xr2_cap
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.make_k_subr1:
            k_cap = g__k_cps_.k_cap
            g__k_cps_ = k_cap
            g__val_apply_k_ = (g__val_apply_k_ - 1)
            g__pc_ = apply_k

        case union_t.kt.make_k_zero:
            k_cap = g__k_cps_.k_cap
            g__k_cps_ = k_cap
            g__val_apply_k_ = (g__val_apply_k_ == 0)
            g__pc_ = apply_k

        case union_t.kt.make_k_if:
            conseq_cap = g__k_cps_.conseq_cap
            alt_cap = g__k_cps_.alt_cap
            env_cap = g__k_cps_.env_cap
            k_cap = g__k_cps_.k_cap
            g__k_ = k_cap
            g__expr_cap_ = conseq_cap if g__val_apply_k_ else alt_cap
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.make_k_throw_v_exp:
            k_cap = g__k_cps_.k_cap
            g__k_cps_ = k_cap
            g__pc_ = apply_k

        case union_t.kt.make_k_throw_k_exp:
            v_exp_cap = g__k_cps_.v_exp_cap
            env_cap = g__k_cps_.env_cap
            g__k_ = kt_make_k_throw_v_exp(g__val_apply_k_)
            g__expr_cap_ = v_exp_cap
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.make_k_let:
            body_cap = g__k_cps_.body_cap
            env_cap = g__k_cps_.env_cap
            k_cap = g__k_cps_.k_cap
            g__k_ = k_cap
            g__expr_cap_ = body_cap
            g__env_ = envr_extend_env(env_cap, g__val_apply_k_)
            g__pc_ = value_of_cps

        case union_t.kt.make_k_rand:
            val_of_rator_cap = g__k_cps_.val_of_rator_cap
            k_cap = g__k_cps_.k_cap
            g__k_clos_ = k_cap
            g__closure_cps_ = val_of_rator_cap
            g__val_clos_ = g__val_apply_k_
            g__pc_ = apply_closure

        case union_t.kt.make_k_rator:
            rand_cap = g__k_cps_.rand_cap
            env_cap = g__k_cps_.env_cap
            k_cap = g__k_cps_.k_cap
            g__k_ = kt_make_k_rand(g__val_apply_k_, k_cap)
            g__expr_cap_ = rand_cap
            g__env_ = env_cap
            g__pc_ = value_of_cps

        case union_t.kt.empty_k:
            jumpout = g__k_cps_.jumpout
            jumpout.switch()

def apply_closure():
    global g__pc_
    global g__env_
    global g__val_clos_
    global g__expr_cap_
    global g__k_
    global g__k_clos_

    match g__closure_cps_.type:
        case union_t.clos.make_closure:
            body = g__closure_cps_.body
            env = g__closure_cps_.env
            g__k_ = g__k_clos_
            g__expr_cap_ = body
            g__env_ = envr_extend_env(env, g__val_clos_)
            g__pc_ = value_of_cps

def apply_env():
    global g__pc_
    global g__val_env_
    global g__env_cps_
    global g__val_apply_k_
    global g__k_cps_
    global g__k_cap_

    match g__env_cps_.type:
        case union_t.envr.extend_env:
            env_cap = g__env_cps_.env_cap
            val_cap = g__env_cps_.val_cap
            if (g__val_env_ == 0):
                g__k_cps_ = g__k_cap_
                g__val_apply_k_ = val_cap
                g__pc_ = apply_k
            else:
                g__env_cps_ = env_cap
                g__val_env_ = (g__val_env_ - 1)
                g__pc_ = apply_env

        case union_t.envr.empty_env:
            raise RuntimeError("unbound identifier")

def value_of_cps():
    global g__pc_
    global g__expr_cap_
    global g__k_
    global g__env_
    global g__val_apply_k_
    global g__k_cps_
    global g__val_env_
    global g__env_cps_
    global g__k_cap_

    match g__expr_cap_.type:
        case union_t.expr.const:
            exprr1 = g__expr_cap_.cexp
            g__k_cps_ = g__k_
            g__val_apply_k_ = exprr1
            g__pc_ = apply_k

        case union_t.expr.mult:
            xr1 = g__expr_cap_.nexpr1
            xr2 = g__expr_cap_.nexpr2
            g__k_ = kt_make_k_mult_vr1(xr2, g__env_, g__k_)
            g__expr_cap_ = xr1
            g__pc_ = value_of_cps

        case union_t.expr.subr1:
            x = g__expr_cap_.nexp
            g__k_ = kt_make_k_subr1(g__k_)
            g__expr_cap_ = x
            g__pc_ = value_of_cps

        case union_t.expr.zero:
            x = g__expr_cap_.nexp
            g__k_ = kt_make_k_zero(g__k_)
            g__expr_cap_ = x
            g__pc_ = value_of_cps

        case union_t.expr._if:
            test = g__expr_cap_.test
            conseq = g__expr_cap_.conseq
            alt = g__expr_cap_.alt
            g__k_ = kt_make_k_if(conseq, alt, g__env_, g__k_)
            g__expr_cap_ = test
            g__pc_ = value_of_cps

        case union_t.expr.letcc:
            body = g__expr_cap_.body
            g__expr_cap_ = body
            g__env_ = envr_extend_env(g__env_, g__k_)
            g__pc_ = value_of_cps

        case union_t.expr.throw:
            k_exp = g__expr_cap_.kexp
            v_exp = g__expr_cap_.vexp
            g__k_ = kt_make_k_throw_k_exp(v_exp, g__env_)
            g__expr_cap_ = k_exp
            g__pc_ = value_of_cps

        case union_t.expr.let:
            e = g__expr_cap_.exp
            body = g__expr_cap_.body
            g__k_ = kt_make_k_let(body, g__env_, g__k_)
            g__expr_cap_ = e
            g__pc_ = value_of_cps

        case union_t.expr.var:
            y = g__expr_cap_.n
            g__k_cap_ = g__k_
            g__env_cps_ = g__env_
            g__val_env_ = y
            g__pc_ = apply_env

        case union_t.expr._lambda:
            body = g__expr_cap_.body
            g__k_cps_ = g__k_
            g__val_apply_k_ = clos_make_closure(body, g__env_)
            g__pc_ = apply_k

        case union_t.expr.app:
            rator = g__expr_cap_.rator
            rand = g__expr_cap_.rand
            g__k_ = kt_make_k_rator(rand, g__env_, g__k_)
            g__expr_cap_ = rator
            g__pc_ = value_of_cps

def mount_tram():
    global g__pc_
    global g__k_
    global _dismount_blank
    g__k_= kt_empty_k(_dismount_blank)

    while True:
        greenlet(g__pc_).switch()


def racket_printf(s, *args):
    import re
    print(re.sub(r"~[a-z]", lambda x: "{}", s).format(*args))

if __name__ == '__main__':
    def _blank():
        pass
    jump_mount_tram = greenlet(mount_tram)
    _dismount_blank = greenlet(_blank)
    g__env_ = envr_empty_env()
    g__expr_cap_ = expr_let(    expr_lambda(    expr_lambda(    expr_if(    expr_zero(    expr_var(0)),     expr_const(1),     expr_mult(    expr_var(0),     expr_app(    expr_app(    expr_var(1),     expr_var(1)),     expr_subr1(    expr_var(0))))))),     expr_mult(    expr_letcc(    expr_app(    expr_app(    expr_var(1),     expr_var(1)),     expr_throw(    expr_var(0),     expr_app(    expr_app(    expr_var(1),     expr_var(1)),     expr_const(4))))),     expr_const(5)))
    g__pc_ = value_of_cps
    jump_mount_tram.switch()
    racket_printf("Output: ~s\n", g__val_apply_k_)
