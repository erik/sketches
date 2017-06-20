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
enum Type<'a> {
    Number,
    Boolean,
    Unbound(FreeVariable),
    Lambda(&'a [Type<'a>], &'a Type<'a>),

}


type TaggedType<'a> = (Expression<'a>, Type<'a>);

type TypeEnv<'a> = HashMap<VariableName<'a>, Type<'a>>;

#[derive(Clone, Copy)]
struct VariableGen { current: FreeVariable }
impl VariableGen {
    fn next(&mut self) -> FreeVariable { self.current += 1; self.current }
}

/// Run algorithm W on expr with given type env, yielding the resolved
/// type of the expression, or the type error encountered.
fn w<'a>(expr: &Expression<'a>, env: &mut TypeEnv<'a>, inst: &mut VariableGen) -> Type<'a> {
    match *expr {
        // simplest case: literals have immediately defined types.
        Expression::Number(_) => Type::Number,
        Expression::Boolean(_) => Type::Boolean,

        // variables we need to look up inside our type environment
        Expression::Variable(name) => Type::Unbound(inst.next()),

        Expression::Lambda(args, body) => Type::Unbound(inst.next()),

        Expression::Let(name, expr, body) => Type::Unbound(inst.next()),

        // 1. ensure that the callable in this function call is truly callable.
        // 2. ensure that the number and type of args match
        // 3. return type of body of lambda
        Expression::FnCall(callable, args) => {
            // FIXME: this seems broken.
            let mut env_ = env.clone();
            let mut inst_ = inst.clone();

            let mut type_args = args
                .iter()
                .map(|arg| {
                    w(arg, &mut env_, &mut inst_)
                })
                .collect::<Vec<Type>>();

            match w(callable, env, inst) {
                Type::Lambda(expected, _) if args.len() != expected.len() => {
                    panic!("wrong number of args given")
                },

                Type::Lambda(expected, body_type) => {
                    if expected != type_args.as_slice() {
                        panic!("bad types given")
                    }

                    body_type.clone()
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
    let args = ["x"];
    let args_ = [&num];
    let fn_call = Expression::FnCall(&incr, &args_);
    let lambda = Expression::Lambda(&args, &num);

    let expr = Expression::Let(
        "incr", &lambda,
        &fn_call);

    let mut inst = VariableGen {current: 0};
    let mut env = HashMap::new();

    println!("this is the thing: {:?}\n", w(&expr, &mut env, &mut inst))
}
