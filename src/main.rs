type FreeVariable = u64;
type VariableName<'a> = &'a str;

/// AST node expression
enum Expression<'a> {
    Number(f64),
    Boolean(bool),

    /// simple variable
    Variable(&'a VariableName<'a>),

    /// fn (x, y, z) -> expr
    Lambda(&'a [VariableName<'a>], &'a Expression<'a>),

    /// let x = expr in expr
    Let(VariableName<'a>, &'a Expression<'a>, &'a Expression<'a>),

    /// some_fn (x, y, z)
    FnCall(&'a Expression<'a>, &'a[Expression<'a>])
}


enum Type<'a> {
    Number,
    Boolean,
    Unbound(FreeVariable),
    Lambda(&'a [Type<'a>], &'a Type<'a>),

}


type TaggedType<'a> = (Expression<'a>, Type<'a>);


fn main() {
    println!("Hello, world!");
}
