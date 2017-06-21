use std::collections::HashMap;

type FreeVariable = u64;
type VariableName<'a> = &'a str;

/// AST node expression
#[derive(Copy, Clone, Debug, PartialEq)]
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


#[derive(Clone, Debug, PartialEq)]
enum Type {
    Number,
    Boolean,
    Unbound(FreeVariable),
    // FIXME: can I avoid having this be boxed and vec'd?
    Lambda(Vec<Type>, Box<Type>),
}


type TypeEnv<'a> = HashMap<VariableName<'a>, Type>;

/// Bind previously unbound type variables to a specified type.
fn bind_type<'a>(env: &mut TypeEnv<'a>, var: FreeVariable, ty: Type) -> Type {
    println!("substitute unbound({}) for {:?}", var, ty);

    for val in env.values_mut() {
        if *val == Type::Unbound(var) {
            *val = ty.clone();
        }
    }

    ty
}


#[derive(Clone, Copy)]
struct VariableGen { current: FreeVariable }
impl VariableGen {
    fn next(&mut self) -> FreeVariable { self.current += 1; self.current }
}


type UnificationError = &'static str;
type TypeError = &'static str;


/// modifies the environment to unify the given types (or returns false if not possible)
fn unify<'a>(a: Type, b: Type, env: &mut TypeEnv<'a>) -> Result<Type, UnificationError> {
    match (a, b) {
        // Easy. Literals match with themselves
        (Type::Number, Type::Number) => Ok(Type::Number),
        (Type::Boolean, Type::Boolean) => Ok(Type::Boolean),

        (Type::Unbound(n), ty) | (ty, Type::Unbound(n)) => Ok(bind_type(env, n, ty)),

        (Type::Lambda(args1, body1), Type::Lambda(args2, body2)) => {
            if args1.len() != args2.len() {
                return Err("lambdas take differing numbers of arguments")
            }

            let mut unified_args = Vec::with_capacity(args1.len());

            for iter in args1.into_iter().zip(args2.into_iter()) {
                match unify(iter.0, iter.1, env) {
                    Ok(ty) => unified_args.push(ty),
                    err => return err
                }
            }

            match unify(*body1, *body2, env) {
                Ok(body_ty) => Ok(Type::Lambda(unified_args, Box::new(body_ty))),
                err => err
            }
        }

        _ => Err("could not unify types")
    }
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
            // Any parameters or variables defined inside the lambda are scoped
            // only to the lambda.
            let mut child_env = env.clone();

            let args_ty = args
                .into_iter()
                .map(|arg| {
                    let ty = inst.next();
                    child_env.insert(arg, Type::Unbound(ty));
                    Type::Unbound(ty)
                })
                .collect::<Vec<Type>>();

            let body_ty = w(body, &mut child_env, inst);

            Type::Lambda(args_ty, Box::new(body_ty))
        },

        // TODO: need an occurs check here.
        Expression::Let(name, expr, body) => {
            let expr_ty = w(expr, env, inst);
            let mut child_env = env.clone();

            child_env.insert(name, expr_ty);

            w(body, &mut child_env, inst)
        },

        // 1. ensure that the callable in this function call is truly callable.
        // 2. ensure that the number and type of args match
        // 3. return type of body of lambda
        Expression::FnCall(callable, args) => {
            let type_args = args
                .iter()
                .map(|arg| {
                    w(arg, env, inst)
                })
                .collect::<Vec<Type>>();

            // Try to unify the lambda we are calling with the way we are calling it.
            unify(w(callable, env, inst),
                  Type::Lambda(type_args, Box::new(Type::Unbound(inst.next()))),
                  env)
                .map(move |ty|{
                    match ty {
                        Type::Lambda(_, return_type) => *return_type,
                        _ => panic!("BUG: expected unify to return lambda, not {:?}", ty)
                    }
                })
                .or(Err("could not unify lambda types"))
                // FIXME: this should eventually return a result anyway.
                .unwrap()
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;

    fn infer(exp: Expression) -> Type {
        let mut inst = VariableGen { current: 0 };
        let mut env = HashMap::new();

        w(&exp, &mut env, &mut inst)
    }

    #[test]
    #[should_panic]
    fn undefined_variable() {
        let var = Expression::Variable("x");

        // FIXME: until we return a result type this will panic.
        infer(var);
    }

    #[test]
    fn literal_inference() {
        let num = Expression::Number(123.0);
        let boolean = Expression::Boolean(true);

        assert_eq!(infer(num), Type::Number);
        assert_eq!(infer(boolean), Type::Boolean);
    }

    #[test]
    fn lambda_expression_inference() {
        let args = ["x"];
        let num = Expression::Number(123.0);

        // \x -> 123.0
        let lambda = Expression::Lambda(&args, &num);

        assert_eq!(infer(lambda), Type::Lambda(vec![Type::Unbound(1)], Box::new(Type::Number)));

        // \x -> \y -> 123.0
        let lambdalambda = Expression::Lambda(&args, &lambda);
        assert_eq!(infer(lambdalambda),
                   Type::Lambda(vec![Type::Unbound(1)], Box::new(
                       Type::Lambda(vec![Type::Unbound(2)], Box::new(Type::Number)))));
    }

    #[test]
    fn let_inference() {
        let boolean = Expression::Boolean(false);
        let var_x = Expression::Variable("x");
        let var_y = Expression::Variable("y");

        // let x = false in x
        let let_ = Expression::Let("x", &boolean, &var_x);

        assert_eq!(infer(let_), Type::Boolean);

        // let y = (let x = false in x) in y
        let nested_let = Expression::Let("y", &let_, &var_y);
        assert_eq!(infer(nested_let), Type::Boolean);
    }

    #[test]
    fn fn_call_inference() {
        let args = ["x"];
        let num = Expression::Number(123.0);
        let boolean = Expression::Boolean(false);

        // \x -> 123.0
        let lambda = Expression::Lambda(&args, &num);

        let app_args = [&boolean];
        let application = Expression::FnCall(&lambda, &app_args);

        assert_eq!(infer(application), Type::Number);

        // let
        //    x = (\a -> a)
        // in
        //    let y = (\a -> a)
        //    in
        //       x (y)
        // ===> \a -> a
        let var_a = Expression::Variable("a");
        let var_x = Expression::Variable("x");
        let var_y = Expression::Variable("y");
        let args_a = ["a"];
        let args_y = [&var_y];
        let lambda_x = Expression::Lambda(&args_a, &var_a);
        let lambda_y = Expression::Lambda(&args_a, &var_a);

        let fncall = Expression::FnCall(&var_x, &args_y);

        let let_y = Expression::Let("y", &lambda_y, &fncall);
        let let_x = Expression::Let("x", &lambda_x, &let_y);

        assert_eq!(infer(let_x), Type::Lambda(vec![Type::Unbound(3)], Box::new(Type::Unbound(3))))
    }

    #[test]
    fn test_unify() {
        let mut env = HashMap::new();

        assert_eq!(unify(Type::Number, Type::Number, &mut env), Ok(Type::Number));
        assert!(unify(Type::Number, Type::Boolean, &mut env).is_err());
        assert_eq!(unify(Type::Unbound(1), Type::Number, &mut env), Ok(Type::Number));

        assert_eq!(unify(Type::Lambda(vec![Type::Unbound(1), Type::Unbound(2)], Box::new(Type::Number)),
                         Type::Lambda(vec![Type::Unbound(3), Type::Unbound(4)], Box::new(Type::Number)),
                         &mut env),
                   Ok(Type::Lambda(vec![Type::Unbound(3), Type::Unbound(4)], Box::new(Type::Number))));
    }
}

fn main() {
    println!("patience.");
}
