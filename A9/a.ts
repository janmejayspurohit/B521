// Define the registers
declare global {
    var g__closure_cps_;
    var g__val_clos_;
    var g__k_clos_;
    var g__env_cps_;
    var g__val_env_;
    var g__k_cap_;
    var g__k_cps_;
    var g__val_apply_k_;
    var g__expr_cap_;
    var g__env_;
    var g__k_;
}
export {}
// Define the program counter
var g__pc_ : Function = undefined;

// Define the union classes
class UnionType extends Object {
    type: String = undefined;
    constructor(type: String, parameters: Object) {
        super();
        this.type = type;
        for (var key in parameters) {
            this[key] = parameters[key];
        }
    }
}
namespace UnionEnums {
    export enum expr {
        const = "EXPR_CONST",
        var = "EXPR_VAR",
        _if = "EXPR__IF",
        mult = "EXPR_MULT",
        subr1 = "EXPR_SUBR1",
        zero = "EXPR_ZERO",
        letcc = "EXPR_LETCC",
        throw = "EXPR_THROW",
        let = "EXPR_LET",
        _lambda = "EXPR__LAMBDA",
        app = "EXPR_APP",
    }
    export enum clos {
        make_closure = "CLOS_MAKE_CLOSURE",
    }
    export enum envr {
        extend_env = "ENVR_EXTEND_ENV",
        empty_env = "ENVR_EMPTY_ENV",
    }
    export enum kt {
        make_k_mult_vr2 = "KT_MAKE_K_MULT_VR2",
        make_k_mult_vr1 = "KT_MAKE_K_MULT_VR1",
        make_k_subr1 = "KT_MAKE_K_SUBR1",
        make_k_zero = "KT_MAKE_K_ZERO",
        make_k_if = "KT_MAKE_K_IF",
        make_k_throw_v_exp = "KT_MAKE_K_THROW_V_EXP",
        make_k_throw_k_exp = "KT_MAKE_K_THROW_K_EXP",
        make_k_let = "KT_MAKE_K_LET",
        make_k_rand = "KT_MAKE_K_RAND",
        make_k_rator = "KT_MAKE_K_RATOR",
        empty_k = "KT_EMPTY_K",
    }
}
