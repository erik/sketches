use std::fmt::Display;
use std::collections::HashMap;

type FreeVariable = u64;
type VariableName<'a> = &'a str;

/// AST node expression
#[derive(Clone, Debug)]
enum Expression<'a> {
    Number(f64),
    Boolean(bool),

    /// simple variable
    Variable(VariableName<'a>),

    /// fn (x, y, z) -> expr
    Lambda(&'a [VariableName<'a>], &'a Expression<'a>),

    /// let x = expr in expr
    Let(VariableName<'a>, &'a Expression<'a>, &'a Expression<'a>),

    /// some_fn (x, y, z)
    FnCall(&'a Expression<'a>, &'a [ &'a Expression<'a>])
}


#[derive(PartialEq, Clone, Debug)]
enum Type {
    Number,
    Boolean,
    Unbound(FreeVariable),
    // FIXME: can I avoid having this be boxed and vec'd?
    Lambda(Vec<Type>, Box<Type>),
}


type TaggedType<'a> = (Expression<'a>, Type);

type TypeEnv<'a> = HashMap<VariableName<'a>, Type>;

#[derive(Clone, Copy)]
struct VariableGen { current: FreeVariable }
impl VariableGen {
    fn next(&mut self) -> FreeVariable { self.current += 1; self.current }
}

/// Run algorithm W on expr with given type env, yielding the resolved
/// type of the expression, or the type error encountered.
fn w<'a>(expr: &Expression<'a>, env: &mut TypeEnv<'a>, inst: &mut VariableGen) -> Type {
    match *expr {
        // simplest case: literals have immediately defined types.
        Expression::Number(_) => Type::Number,
        Expression::Boolean(_) => Type::Boolean,

        // variables we need to look up inside our type environment
        Expression::Variable(name) => match env.get(name) {
            Some(typ) => typ.clone(),
            None => panic!("undefined variable, {}", name)
        },

        Expression::Lambda(args, body) => {
            // TODO: args -> types. modify the environment
            let body_ty = w(body, env, inst);
            let args_ty = args
                .into_iter()
                .map(|arg| {
                    let ty = inst.next();
                    env.insert(arg, Type::Unbound(ty));
                    Type::Unbound(ty)
                })
                .collect::<Vec<Type>>();

            Type::Lambda(args_ty, Box::new(body_ty))
        },

        Expression::Let(name, expr, body) => {
            let expr_ty = w(expr, env, inst);
            env.insert(name, expr_ty);

            w(body, env, inst)
        },

        // 1. ensure that the callable in this function call is truly callable.
        // 2. ensure that the number and type of args match
        // 3. return type of body of lambda
        Expression::FnCall(callable, args) => {
            // FIXME: this seems broken.
            let mut env_ = env.clone();
            let mut inst_ = inst.clone();

            let type_args = args
                .iter()
                .map(|arg| {
                    w(arg, &mut env_, &mut inst_)
                })
                .collect::<Vec<Type>>();

            match w(callable, env, inst) {
                Type::Lambda(expected, body_type) => {
                    if args.len() != expected.len() {
                        panic!("wrong number of args given, {} for {}", args.len(), expected.len())
                    } else if expected != type_args.as_slice() {
                        panic!("bad types given: {:?} for {:?}", type_args.as_slice(), expected)
                    }

                    *body_type.clone()
                }

                _ => panic!("trying to call non-function")
            }
        }
    }
}

fn main() {
    // FIXME: this is so gnarly.
    // let incr = \x -> 123.0 in incr (123.0) :: Number
    let num = Expression::Number(123.0);
    let incr = Expression::Variable("incr");
    let x = Expression::Variable("x");
    let args = ["x"];
    let args_ = [&num];
    let fn_call = Expression::FnCall(&incr, &args_);
    let lambda = Expression::Lambda(&args, &num);

    // let expr = Expression::Let(
    //     "incr", &lambda,
    //     &fn_call);

    let expr = Expression::Let("y", &num, &x);


    let mut inst = VariableGen {current: 0};
    let mut env = HashMap::new();

    println!("this is the thing: {:?}\n", w(&expr, &mut env, &mut inst))
}
