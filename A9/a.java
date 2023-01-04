import java.util.HashMap;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

// Define the registers
class RegistersGlobal {
    static Object Register_CLOSURE_CPS = null;
    static Object Register_VAL_CLOS = null;
    static Object Register_K_CLOS = null;
    static Object Register_ENV_CPS = null;
    static Object Register_VAL_ENV = null;
    static Object Register_K_CAP = null;
    static Object Register_K_CPS = null;
    static Object Register_VAL_APPLY_K = null;
    static Object Register_EXPR_CAP = null;
    static Object Register_ENV = null;
    static Object Register_K = null;

    // Define the program counter
    static Callable<Void> Register_PC = null;
}

// Define the union classes, functions and enums
class UnionEnums {
    enum expr {
       CONST,
       VAR,
       _IF,
       MULT,
       SUBR1,
       ZERO,
       LETCC,
       THROW,
       LET,
       _LAMBDA,
       APP,
    }

    enum clos {
       MAKE_CLOSURE,
    }

    enum envr {
       EXTEND_ENV,
       EMPTY_ENV,
    }

    enum kt {
       MAKE_K_MULT_VR2,
       MAKE_K_MULT_VR1,
       MAKE_K_SUBR1,
       MAKE_K_ZERO,
       MAKE_K_IF,
       MAKE_K_THROW_V_EXP,
       MAKE_K_THROW_K_EXP,
       MAKE_K_LET,
       MAKE_K_RAND,
       MAKE_K_RATOR,
       EMPTY_K,
    }

}

// Define the union classes
class UnionType extends Object {

    private Object type = null;
    private HashMap<String, Object> properties = new HashMap<>();

    public Object getType() {
        return type;
    }

    public Object get(String property) {
        return this.properties.get(property);
    }

    public UnionType(Object type, HashMap<String, Object> properties) {
        this.type = type;
        this.properties = properties;
    }

    @Override
    public String toString() {
        return "UnionType[type = " + type.toString() + ", properties = {"
                + properties.keySet().stream().map((String s) -> {
                    return s + ": " + properties.get(s).toString();
                }).reduce("", (String a, String p) -> {
                    return a + ", " + p;
                }) + "}]";
    }

    // Union functions
    public static UnionType kt_make_k_mult_vr2(Object vr1_cap, Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("vr1_cap", vr1_cap);
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_MULT_VR2, properties);
    }

    public static UnionType kt_make_k_mult_vr1(Object xr2_cap, Object env_cap, Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("xr2_cap", xr2_cap);
        properties.put("env_cap", env_cap);
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_MULT_VR1, properties);
    }

    public static UnionType kt_make_k_subr1(Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_SUBR1, properties);
    }

    public static UnionType kt_make_k_zero(Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_ZERO, properties);
    }

    public static UnionType kt_make_k_if(Object conseq_cap, Object alt_cap, Object env_cap, Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("conseq_cap", conseq_cap);
        properties.put("alt_cap", alt_cap);
        properties.put("env_cap", env_cap);
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_IF, properties);
    }

    public static UnionType kt_make_k_throw_v_exp(Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_THROW_V_EXP, properties);
    }

    public static UnionType kt_make_k_throw_k_exp(Object v_exp_cap, Object env_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("v_exp_cap", v_exp_cap);
        properties.put("env_cap", env_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_THROW_K_EXP, properties);
    }

    public static UnionType kt_make_k_let(Object body_cap, Object env_cap, Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("body_cap", body_cap);
        properties.put("env_cap", env_cap);
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_LET, properties);
    }

    public static UnionType kt_make_k_rand(Object val_of_rator_cap, Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("val_of_rator_cap", val_of_rator_cap);
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_RAND, properties);
    }

    public static UnionType kt_make_k_rator(Object rand_cap, Object env_cap, Object k_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("rand_cap", rand_cap);
        properties.put("env_cap", env_cap);
        properties.put("k_cap", k_cap);
        return new UnionType(UnionEnums.kt.MAKE_K_RATOR, properties);
    }

    public static UnionType kt_empty_k(Object jumpout) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("jumpout", jumpout);
        return new UnionType(UnionEnums.kt.EMPTY_K, properties);
    }

    public static UnionType envr_extend_env(Object env_cap, Object val_cap) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("env_cap", env_cap);
        properties.put("val_cap", val_cap);
        return new UnionType(UnionEnums.envr.EXTEND_ENV, properties);
    }

    public static UnionType envr_empty_env() {
        HashMap<String, Object> properties = new HashMap<>();

        return new UnionType(UnionEnums.envr.EMPTY_ENV, properties);
    }

    public static UnionType clos_make_closure(Object body, Object env) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("body", body);
        properties.put("env", env);
        return new UnionType(UnionEnums.clos.MAKE_CLOSURE, properties);
    }

    public static UnionType expr_const(Object cexp) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("cexp", cexp);
        return new UnionType(UnionEnums.expr.CONST, properties);
    }

    public static UnionType expr_var(Object n) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("n", n);
        return new UnionType(UnionEnums.expr.VAR, properties);
    }

    public static UnionType expr_if(Object test, Object conseq, Object alt) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("test", test);
        properties.put("conseq", conseq);
        properties.put("alt", alt);
        return new UnionType(UnionEnums.expr._IF, properties);
    }

    public static UnionType expr_mult(Object nexpr1, Object nexpr2) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("nexpr1", nexpr1);
        properties.put("nexpr2", nexpr2);
        return new UnionType(UnionEnums.expr.MULT, properties);
    }

    public static UnionType expr_subr1(Object nexp) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("nexp", nexp);
        return new UnionType(UnionEnums.expr.SUBR1, properties);
    }

    public static UnionType expr_zero(Object nexp) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("nexp", nexp);
        return new UnionType(UnionEnums.expr.ZERO, properties);
    }

    public static UnionType expr_letcc(Object body) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("body", body);
        return new UnionType(UnionEnums.expr.LETCC, properties);
    }

    public static UnionType expr_throw(Object kexp, Object vexp) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("kexp", kexp);
        properties.put("vexp", vexp);
        return new UnionType(UnionEnums.expr.THROW, properties);
    }

    public static UnionType expr_let(Object exp, Object body) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("exp", exp);
        properties.put("body", body);
        return new UnionType(UnionEnums.expr.LET, properties);
    }

    public static UnionType expr_lambda(Object body) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("body", body);
        return new UnionType(UnionEnums.expr._LAMBDA, properties);
    }

    public static UnionType expr_app(Object rator, Object rand) {
        HashMap<String, Object> properties = new HashMap<>();
        properties.put("rator", rator);
        properties.put("rand", rand);
        return new UnionType(UnionEnums.expr.APP, properties);
    }


}

// Generate functions
class ContinuationDriver {

    private ExecutorService executorService;

    public ContinuationDriver(ExecutorService executorService) {
        this.executorService = executorService;
    }

    public Future<Void> apply_k() throws Exception {
        UnionType union_object = ((UnionType) RegistersGlobal.Register_K_CPS);
        UnionEnums.kt target = (UnionEnums.kt) union_object.getType();

        Object xr2_cap = null, jumpout = null, env_cap = null, body_cap = null, rand_cap = null, conseq_cap = null, vr1_cap = null, v_exp_cap = null, k_cap = null, val_of_rator_cap = null, alt_cap = null;

        switch (target) {
            case MAKE_K_MULT_VR2:
                vr1_cap = union_object.get("vr1_cap");
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K_CPS = k_cap;
                RegistersGlobal.Register_VAL_APPLY_K = (Integer) vr1_cap * (Integer) RegistersGlobal.Register_VAL_APPLY_K;
                RegistersGlobal.Register_PC = () -> {
                    this.apply_k();
                    return null;
                };
                break;

            case MAKE_K_MULT_VR1:
                xr2_cap = union_object.get("xr2_cap");
                env_cap = union_object.get("env_cap");
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K = UnionType.kt_make_k_mult_vr2(RegistersGlobal.Register_VAL_APPLY_K, k_cap);
                RegistersGlobal.Register_EXPR_CAP = xr2_cap;
                RegistersGlobal.Register_ENV = env_cap;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case MAKE_K_SUBR1:
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K_CPS = k_cap;
                RegistersGlobal.Register_VAL_APPLY_K = ((Integer) RegistersGlobal.Register_VAL_APPLY_K - 1);
                RegistersGlobal.Register_PC = () -> {
                    this.apply_k();
                    return null;
                };
                break;

            case MAKE_K_ZERO:
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K_CPS = k_cap;
                RegistersGlobal.Register_VAL_APPLY_K = RegistersGlobal.Register_VAL_APPLY_K.equals(0);
                RegistersGlobal.Register_PC = () -> {
                    this.apply_k();
                    return null;
                };
                break;

            case MAKE_K_IF:
                conseq_cap = union_object.get("conseq_cap");
                alt_cap = union_object.get("alt_cap");
                env_cap = union_object.get("env_cap");
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K = k_cap;
                RegistersGlobal.Register_EXPR_CAP = ((Boolean)conseq_cap) ? (RegistersGlobal.Register_VAL_APPLY_K) : (alt_cap);
                RegistersGlobal.Register_ENV = env_cap;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case MAKE_K_THROW_V_EXP:
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K_CPS = k_cap;
                RegistersGlobal.Register_PC = () -> {
                    this.apply_k();
                    return null;
                };
                break;

            case MAKE_K_THROW_K_EXP:
                v_exp_cap = union_object.get("v_exp_cap");
                env_cap = union_object.get("env_cap");
                RegistersGlobal.Register_K = UnionType.kt_make_k_throw_v_exp(RegistersGlobal.Register_VAL_APPLY_K);
                RegistersGlobal.Register_EXPR_CAP = v_exp_cap;
                RegistersGlobal.Register_ENV = env_cap;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case MAKE_K_LET:
                body_cap = union_object.get("body_cap");
                env_cap = union_object.get("env_cap");
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K = k_cap;
                RegistersGlobal.Register_EXPR_CAP = body_cap;
                RegistersGlobal.Register_ENV = UnionType.envr_extend_env(env_cap, RegistersGlobal.Register_VAL_APPLY_K);
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case MAKE_K_RAND:
                val_of_rator_cap = union_object.get("val_of_rator_cap");
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K_CLOS = k_cap;
                RegistersGlobal.Register_CLOSURE_CPS = val_of_rator_cap;
                RegistersGlobal.Register_VAL_CLOS = RegistersGlobal.Register_VAL_APPLY_K;
                RegistersGlobal.Register_PC = () -> {
                    this.apply_closure();
                    return null;
                };
                break;

            case MAKE_K_RATOR:
                rand_cap = union_object.get("rand_cap");
                env_cap = union_object.get("env_cap");
                k_cap = union_object.get("k_cap");
                RegistersGlobal.Register_K = UnionType.kt_make_k_rand(RegistersGlobal.Register_VAL_APPLY_K, k_cap);
                RegistersGlobal.Register_EXPR_CAP = rand_cap;
                RegistersGlobal.Register_ENV = env_cap;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case EMPTY_K:
                jumpout = union_object.get("jumpout");
                this.executorService.submit((Callable<Void>) jumpout);                break;

        }
        return null;
    }

    public Future<Void> apply_closure() throws Exception {
        UnionType union_object = ((UnionType) RegistersGlobal.Register_CLOSURE_CPS);
        UnionEnums.clos target = (UnionEnums.clos) union_object.getType();

        Object env = null, body = null;

        switch (target) {
            case MAKE_CLOSURE:
                body = union_object.get("body");
                env = union_object.get("env");
                RegistersGlobal.Register_K = RegistersGlobal.Register_K_CLOS;
                RegistersGlobal.Register_EXPR_CAP = body;
                RegistersGlobal.Register_ENV = UnionType.envr_extend_env(env, RegistersGlobal.Register_VAL_CLOS);
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

        }
        return null;
    }

    public Future<Void> apply_env() throws Exception {
        UnionType union_object = ((UnionType) RegistersGlobal.Register_ENV_CPS);
        UnionEnums.envr target = (UnionEnums.envr) union_object.getType();

        Object env_cap = null, val_cap = null;

        switch (target) {
            case EXTEND_ENV:
                env_cap = union_object.get("env_cap");
                val_cap = union_object.get("val_cap");
                if ((Boolean) (RegistersGlobal.Register_VAL_ENV.equals(0))) {
                    RegistersGlobal.Register_K_CPS = RegistersGlobal.Register_K_CAP;
                    RegistersGlobal.Register_VAL_APPLY_K = val_cap;
                    RegistersGlobal.Register_PC = () -> {
                        this.apply_k();
                        return null;
                    };
                } else {
                    RegistersGlobal.Register_ENV_CPS = env_cap;
                    RegistersGlobal.Register_VAL_ENV = ((Integer) RegistersGlobal.Register_VAL_ENV - 1);
                    RegistersGlobal.Register_PC = () -> {
                        this.apply_env();
                        return null;
                    };
                }
                break;

            case EMPTY_ENV:
                throw new Exception("unbound identifier");


        }
        return null;
    }

    public Future<Void> value_of_cps() throws Exception {
        UnionType union_object = ((UnionType) RegistersGlobal.Register_EXPR_CAP);
        UnionEnums.expr target = (UnionEnums.expr) union_object.getType();

        Object k_exp = null, conseq = null, exprr1 = null, xr2 = null, xr1 = null, y = null, rator = null, e = null, v_exp = null, rand = null, x = null, alt = null, body = null, test = null;

        switch (target) {
            case CONST:
                exprr1 = union_object.get("cexp");
                RegistersGlobal.Register_K_CPS = RegistersGlobal.Register_K;
                RegistersGlobal.Register_VAL_APPLY_K = exprr1;
                RegistersGlobal.Register_PC = () -> {
                    this.apply_k();
                    return null;
                };
                break;

            case MULT:
                xr1 = union_object.get("nexpr1");
                xr2 = union_object.get("nexpr2");
                RegistersGlobal.Register_K = UnionType.kt_make_k_mult_vr1(xr2, RegistersGlobal.Register_ENV, RegistersGlobal.Register_K);
                RegistersGlobal.Register_EXPR_CAP = xr1;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case SUBR1:
                x = union_object.get("nexp");
                RegistersGlobal.Register_K = UnionType.kt_make_k_subr1(RegistersGlobal.Register_K);
                RegistersGlobal.Register_EXPR_CAP = x;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case ZERO:
                x = union_object.get("nexp");
                RegistersGlobal.Register_K = UnionType.kt_make_k_zero(RegistersGlobal.Register_K);
                RegistersGlobal.Register_EXPR_CAP = x;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case _IF:
                test = union_object.get("test");
                conseq = union_object.get("conseq");
                alt = union_object.get("alt");
                RegistersGlobal.Register_K = UnionType.kt_make_k_if(conseq, alt, RegistersGlobal.Register_ENV, RegistersGlobal.Register_K);
                RegistersGlobal.Register_EXPR_CAP = test;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case LETCC:
                body = union_object.get("body");
                RegistersGlobal.Register_EXPR_CAP = body;
                RegistersGlobal.Register_ENV = UnionType.envr_extend_env(RegistersGlobal.Register_ENV, RegistersGlobal.Register_K);
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case THROW:
                k_exp = union_object.get("kexp");
                v_exp = union_object.get("vexp");
                RegistersGlobal.Register_K = UnionType.kt_make_k_throw_k_exp(v_exp, RegistersGlobal.Register_ENV);
                RegistersGlobal.Register_EXPR_CAP = k_exp;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case LET:
                e = union_object.get("exp");
                body = union_object.get("body");
                RegistersGlobal.Register_K = UnionType.kt_make_k_let(body, RegistersGlobal.Register_ENV, RegistersGlobal.Register_K);
                RegistersGlobal.Register_EXPR_CAP = e;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

            case VAR:
                y = union_object.get("n");
                RegistersGlobal.Register_K_CAP = RegistersGlobal.Register_K;
                RegistersGlobal.Register_ENV_CPS = RegistersGlobal.Register_ENV;
                RegistersGlobal.Register_VAL_ENV = y;
                RegistersGlobal.Register_PC = () -> {
                    this.apply_env();
                    return null;
                };
                break;

            case _LAMBDA:
                body = union_object.get("body");
                RegistersGlobal.Register_K_CPS = RegistersGlobal.Register_K;
                RegistersGlobal.Register_VAL_APPLY_K = UnionType.clos_make_closure(body, RegistersGlobal.Register_ENV);
                RegistersGlobal.Register_PC = () -> {
                    this.apply_k();
                    return null;
                };
                break;

            case APP:
                rator = union_object.get("rator");
                rand = union_object.get("rand");
                RegistersGlobal.Register_K = UnionType.kt_make_k_rator(rand, RegistersGlobal.Register_ENV, RegistersGlobal.Register_K);
                RegistersGlobal.Register_EXPR_CAP = rator;
                RegistersGlobal.Register_PC = () -> {
                    this.value_of_cps();
                    return null;
                };
                break;

        }
        return null;
    }

    void mount_tram() {
        RegistersGlobal.Register_K = UnionType.kt_empty_k((Callable<Void>) (() -> {
            this.executorService.shutdown();
            return null;
        }));

        trampoline();
    }

    void trampoline() {
        this.executorService.submit(RegistersGlobal.Register_PC);
        System.out.println(RegistersGlobal.Register_PC.toString());
        this.executorService.submit(() -> {
            trampoline();
            return null;
        });
    }

    void waitUntilTrampolineTermination() throws InterruptedException {
        this.executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.NANOSECONDS);
    }
}


public class a {

    private ContinuationDriver continuationDriver;

    public a() {
        this.continuationDriver = new ContinuationDriver(Executors.newSingleThreadExecutor());
    }

    public static void main(String[] args) throws InterruptedException {
        a application = new a();
        RegistersGlobal.Register_ENV = UnionType.envr_empty_env();
        RegistersGlobal.Register_EXPR_CAP = UnionType.expr_let(        UnionType.expr_lambda(        UnionType.expr_lambda(        UnionType.expr_if(        UnionType.expr_zero(        UnionType.expr_var(0)),         UnionType.expr_const(1),         UnionType.expr_mult(        UnionType.expr_var(0),         UnionType.expr_app(        UnionType.expr_app(        UnionType.expr_var(1),         UnionType.expr_var(1)),         UnionType.expr_subr1(        UnionType.expr_var(0))))))),         UnionType.expr_mult(        UnionType.expr_letcc(        UnionType.expr_app(        UnionType.expr_app(        UnionType.expr_var(1),         UnionType.expr_var(1)),         UnionType.expr_throw(        UnionType.expr_var(0),         UnionType.expr_app(        UnionType.expr_app(        UnionType.expr_var(1),         UnionType.expr_var(1)),         UnionType.expr_const(4))))),         UnionType.expr_const(5)));
        RegistersGlobal.Register_PC = () -> {
            application.continuationDriver.value_of_cps();
            return null;
        };
        application.continuationDriver.mount_tram();
        application.continuationDriver.waitUntilTrampolineTermination();
        System.out.printf("Output: %d\n", RegistersGlobal.Register_VAL_APPLY_K);
    }
}
