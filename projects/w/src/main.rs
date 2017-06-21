use std::fmt::Display;
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
fn bind_type<'a>(env: &mut TypeEnv<'a>, var: FreeVariable, ty: Type) {
    println!("substitute unbound({}) for {:?}", var, ty);

    for val in env.values_mut() {
        if *val == Type::Unbound(var) {
            *val = ty.clone();
        }
    }
}


#[derive(Clone, Copy)]
struct VariableGen { current: FreeVariable }
impl VariableGen {
    fn next(&mut self) -> FreeVariable { self.current += 1; self.current }
}


/// modifies the environment to unify the given types (or returns false if not possible)
fn unify<'a>(a: Type, b: Type, env: &mut TypeEnv<'a>) -> bool {
    match (a, b) {
        // Easy. Literals match with themselves
        (Type::Number, Type::Number) => true,
        (Type::Boolean, Type::Boolean) => true,

        (Type::Unbound(n), ty) | (ty, Type::Unbound(n)) => {
            bind_type(env, n, ty);
            true
        }

        (Type::Lambda(args1, body1), Type::Lambda(args2, body2)) => {
            if args1.len() != args2.len() { return false }

            for iter in args1.iter().zip(args2.iter()) {
                let (a1, a2) = iter;

                if !unify(a1.clone(), a2.clone(), env) { return false }
            }

            unify(*body1, *body2, env)
        }

        _ => false
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

        // TODO: need an occurs check here.
        Expression::Let(name, expr, body) => {
            let expr_ty = w(expr, env, inst);
            env.insert(name, expr_ty);

            w(body, env, inst)
        },

        // 1. ensure that the callable in this function call is truly callable.
        // 2. ensure that the number and type of args match
        // 3. return type of body of lambda
        // TODO: Need unification algo here.
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

            let return_type = Type::Unbound(inst.next());

            let lambda_ty = w(callable, env, inst);
            let derived_ty = Type::Lambda(type_args, Box::new(return_type.clone()));

            if !unify(lambda_ty.clone(), derived_ty.clone(), env) {
                panic!("could not unify! {:?} for {:?}", lambda_ty, derived_ty)
            }

            return_type
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
    fn literal_inference() {
        let num = Expression::Number(123.0);
        let boolean = Expression::Boolean(true);
        // let var = Expression::Variable("x");

        assert_eq!(infer(num), Type::Number);
        assert_eq!(infer(boolean), Type::Boolean);

        // FIXME: until we return a result type this will panic.
        // assert_eq!(w(&var, &mut env, &mut inst), Type::Unbound(1));
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
                   Type::Lambda(vec![Type::Unbound(2)], Box::new(
                       Type::Lambda(vec![Type::Unbound(1)], Box::new(Type::Number)))));
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

        assert_eq!(infer(application), Type::Number)

    }

    #[test]
    fn test_unify() {
        let mut env = HashMap::new();

        assert!(unify(Type::Number, Type::Number, &mut env));
        assert_eq!(unify(Type::Number, Type::Boolean, &mut env), false);
        assert!(unify(Type::Unbound(1), Type::Number, &mut env));

        assert!(unify(Type::Lambda(vec![Type::Unbound(1), Type::Unbound(2)], Box::new(Type::Number)),
                      Type::Lambda(vec![Type::Unbound(3), Type::Unbound(4)], Box::new(Type::Number)),
                      &mut env));
    }
}

fn main() {
    println!("patience.");
}
