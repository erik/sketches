use std::collections::HashMap;

use pest::error::Error;
use pest::Parser;

#[derive(Parser)]
#[grammar = "profile.pest"]
pub struct ProfileParser;

#[derive(Debug, Clone)]
pub enum TagPattern {
    Exists { key: String },
    NotExists { key: String },
    OneOf { key: String, values: Vec<String> },
    NoneOf { key: String, values: Vec<String> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Invalid,
    Bool(bool),
    Number(f32),
    String(String),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Invalid)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Value),
    Ident(String),
    TagPattern(Vec<TagPattern>),
    NamedBlock(NamedBlock),
    WhenBlock(WhenBlock),
}

type Def = (String, Expression);
type Definitions = Vec<Def>;

#[derive(Debug, Clone)]
pub struct NamedBlock {
    defs: Definitions,
    name: String,
    body: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct WhenBlock(Vec<WhenClause>);

#[derive(Debug, Clone)]
pub struct WhenClause {
    condition: Expression,
    value: Expression,
}

#[derive(Debug)]
pub struct Profile {
    name: String,
    global_defs: Definitions,

    node_penalty: Option<NamedBlock>,
    way_penalty: Option<NamedBlock>,
    cost_factor: Option<NamedBlock>,
}

fn parse(source: &str) -> Result<Profile, Error<Rule>> {
    let mut syntax_tree = ProfileParser::parse(Rule::top_level, source)?;
    let root_node = syntax_tree.next().unwrap();

    Ok(parse_profile(root_node.into_inner()))
}

fn parse_profile(mut pairs: pest::iterators::Pairs<Rule>) -> Profile {
    let profile_name = parse_as_str(pairs.next().unwrap());
    let mut defs = Definitions::new();
    let mut node_penalty = None;
    let mut way_penalty = None;
    let mut cost_factor = None;

    for node in pairs {
        match node.as_rule() {
            Rule::define_block => {
                defs = parse_define_block(node.into_inner());
            }

            Rule::named_block => {
                let block = parse_named_block(node.into_inner());
                match block.name.as_str() {
                    "node-penalty" => node_penalty = Some(block),
                    "way-penalty" => way_penalty = Some(block),
                    "cost-factor" => cost_factor = Some(block),
                    name => panic!("unexpected block: {:?}", name),
                }
            }

            _ => panic!("unexpected"),
        }
    }

    Profile {
        name: profile_name.into(),
        global_defs: defs,

        node_penalty,
        way_penalty,
        cost_factor,
    }
}

fn parse_define_block(pairs: pest::iterators::Pairs<Rule>) -> Definitions {
    let mut defs = Definitions::new();

    for node in pairs {
        match node.as_rule() {
            Rule::assignment => {
                let mut assignment_node = node.into_inner();
                let name = parse_as_str(assignment_node.next().unwrap());
                let value = parse_expr(assignment_node.next().unwrap());

                defs.push((name.into(), value));
            }

            rule => panic!("unexpected node: {:?}", rule),
        }
    }

    defs
}

fn parse_expr(pair: pest::iterators::Pair<Rule>) -> Expression {
    use Expression::*;

    match pair.as_rule() {
        Rule::bool => Literal(Value::Bool(pair.as_str() == "true")),
        Rule::number => Literal(Value::Number(pair.as_str().parse().unwrap())),
        Rule::string => Literal(Value::String(parse_as_str(pair).into())),
        Rule::invalid => Literal(Value::Invalid),

        Rule::tag_expr => TagPattern(parse_tag_expr(pair.into_inner())),
        Rule::when_block => WhenBlock(parse_when_block(pair.into_inner())),
        Rule::named_block => NamedBlock(parse_named_block(pair.into_inner())),
        Rule::ident => Ident(parse_as_str(pair).into()),

        rule => panic!("unexpected rule: {:?}", rule),
    }
}

fn parse_when_block(pairs: pest::iterators::Pairs<Rule>) -> WhenBlock {
    let mut clauses = vec![];

    for node in pairs {
        let mut node = node.into_inner();

        let condition = {
            let node = node.next().unwrap();
            match node.as_rule() {
                Rule::else_ => Expression::Literal(Value::Bool(true)),
                _ => parse_expr(node),
            }
        };

        clauses.push(WhenClause {
            condition,
            value: parse_expr(node.next().unwrap()),
        });
    }

    WhenBlock(clauses)
}

fn parse_named_block(mut pairs: pest::iterators::Pairs<Rule>) -> NamedBlock {
    let name = pairs.next().unwrap().as_str().into();
    let mut defs = Definitions::new();
    let mut body = vec![];

    for node in pairs {
        match node.as_rule() {
            Rule::define_block => defs = parse_define_block(node.into_inner()),
            _ => body.push(parse_expr(node)),
        }
    }

    NamedBlock { defs, name, body }
}

fn parse_tag_expr(pairs: pest::iterators::Pairs<Rule>) -> Vec<TagPattern> {
    let mut exprs = vec![];

    for node in pairs {
        let expr = match node.as_rule() {
            Rule::tag_expr_binary_clause => {
                let mut node = node.into_inner();
                let key = parse_as_str(node.next().unwrap());
                let op = node.next().unwrap();
                let patterns = node
                    .next()
                    .unwrap()
                    .into_inner()
                    .into_iter()
                    .map(|x| parse_as_str(x).into());

                match op.as_rule() {
                    Rule::op_eq => TagPattern::OneOf {
                        key: key.into(),
                        values: patterns.collect(),
                    },
                    Rule::op_neq => TagPattern::NoneOf {
                        key: key.into(),
                        values: patterns.collect(),
                    },
                    rule => panic!("unexpected rule: {:?}", rule),
                }
            }

            Rule::tag_expr_exists_clause => {
                let mut node = node.into_inner();
                let key = parse_as_str(node.next().unwrap());

                TagPattern::Exists { key: key.into() }
            }

            Rule::tag_expr_not_exists_clause => {
                let mut node = node.into_inner();
                let key = parse_as_str(node.next().unwrap());

                TagPattern::NotExists { key: key.into() }
            }

            rule => panic!("unexpected: {:?}", rule),
        };

        exprs.push(expr);
    }

    exprs
}

fn parse_as_str<'i>(pair: pest::iterators::Pair<'i, Rule>) -> &'i str {
    match pair.as_rule() {
        Rule::ident => pair.as_str(),
        Rule::string => {
            let s = &pair.as_str();
            &s[1..s.len() - 1]
        }

        r => panic!("unexpected rule: {:?}", r),
    }
}

#[derive(Debug)]
struct Scope {
    scope: HashMap<String, LazyValue>,
}

#[derive(Debug, Clone)]
enum LazyValue {
    Evaluated(Value),
    Unevaluated(Expression),
}

impl Scope {
    fn root() -> Scope {
        Scope {
            scope: HashMap::new(),
        }
    }

    fn from_defs(defs: &Definitions) -> Scope {
        let scope = defs
            .iter()
            .map(|(k, v)| (k.into(), LazyValue::Unevaluated(v.clone())))
            .collect();

        Scope { scope }
    }

    fn get_child(&self, definitions: &Definitions) -> Scope {
        let scope = definitions
            .iter()
            .map(|(k, v)| (k.into(), LazyValue::Unevaluated(v.clone())))
            .collect();

        Scope { scope }
    }

    fn set(&mut self, k: &str, v: Value) {
        self.scope.insert(k.into(), LazyValue::Evaluated(v));
    }

    fn get(&self, k: &str) -> Option<&LazyValue> {
        self.scope.get(k)
    }
}

#[derive(Debug, Clone)]
enum EvalError {
    Lookup(String),
    UnknownBlock(String),
    TagNotSupported,
}

type TagMatcherFn = dyn FnMut(&TagPattern) -> Result<bool, EvalError>;

struct EvalContext<'a> {
    scope: &'a mut Scope,
    parent: Option<&'a EvalContext<'a>>,
    matcher: &'a TagMatcherFn,
}

#[derive(Debug)]
struct ProfileRuntime {
    global_scope: Scope,
}

impl ProfileRuntime {
    fn from(profile: &Profile) -> Result<ProfileRuntime, EvalError> {
        let mut rt = ProfileRuntime {
            global_scope: Scope::root(),
        };
        rt.eval_globals(&profile.global_defs)?;
        Ok(rt)
    }

    fn eval_globals(&mut self, defs: &Definitions) -> Result<(), EvalError> {
        for (name, expr) in defs {
            // TODO: Creating in a loop to avoid lifetime-headaches, can this be avoided?
            let value = self.global_context().eval_expr(expr)?;
            self.global_scope.set(name, value);
        }

        Ok(())
    }

    fn global_context(&mut self) -> EvalContext {
        EvalContext {
            scope: &mut self.global_scope,
            parent: None,
            matcher: &|_| Err(EvalError::TagNotSupported),
        }
    }

    fn with_tag_context<'a>(&'a mut self, matcher: &'a TagMatcherFn) -> EvalContext {
        EvalContext {
            matcher,
            scope: &mut self.global_scope,
            parent: None,
        }
    }
}

impl<'a> EvalContext<'a> {
    // TODO: better name
    fn child(&'a self, scope: &'a mut Scope) -> EvalContext {
        EvalContext {
            scope,
            parent: Some(self),
            matcher: self.matcher,
        }
    }

    fn get_lazy(&self, key: &str) -> Option<LazyValue> {
        self.scope.get(key).cloned().or_else(|| match self.parent {
            Some(parent) => parent.get_lazy(key),
            None => None,
        })
    }

    fn get_and_eval(&mut self, key: &str) -> Result<Value, EvalError> {
        match self.get_lazy(key) {
            None => Err(EvalError::Lookup(key.into())),
            Some(LazyValue::Evaluated(val)) => Ok(val),
            Some(LazyValue::Unevaluated(expr)) => {
                let val = self.eval_expr(&expr)?;

                // TODO: We're only setting variables in the local scope, which
                // means they could need to be recalculated in a sibling.  not
                // optimal
                self.scope.set(key, val.clone());
                Ok(val)
            }
        }
    }

    // TODO: way too much cloning here
    fn eval_expr(&mut self, expr: &Expression) -> Result<Value, EvalError> {
        use Expression::*;
        let value = match expr {
            Literal(v) => v.clone(),
            Ident(name) => self.get_and_eval(name)?,

            NamedBlock(block) => self.eval_named_block(block)?,
            TagPattern(_) => todo!(),
            WhenBlock(block) => self.eval_when_block(block)?,
        };

        Ok(value)
    }

    fn eval_when_block(&mut self, block: &WhenBlock) -> Result<Value, EvalError> {
        for clause in block.0.iter() {
            let cond = self.eval_expr(&clause.condition)?;
            if cond.is_truthy() {
                let value = self.eval_expr(&clause.value)?;
                return Ok(value);
            }
        }

        println!("WARN: no matching when clause, using INVALID");
        Ok(Value::Invalid)
    }

    fn eval_named_block_inner(&mut self, block: &NamedBlock) -> Result<Value, EvalError> {
        match block.name.as_str() {
            "any?" | "none?" => {
                let mut any = false;
                let invert = block.name.as_str() == "none?";

                for expr in &block.body {
                    let val = self.eval_expr(expr)?;
                    if val.is_truthy() {
                        any = true;
                        break;
                    }
                }

                let val = Value::Bool(if invert { !any } else { any });
                Ok(val)
            }

            "all?" => {
                for expr in &block.body {
                    let val = self.eval_expr(expr)?;
                    if !val.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            }

            "eq?" => {
                if block.body.is_empty() {
                    return Ok(Value::Bool(false));
                }

                let mut prev = self.eval_expr(&block.body[0])?;
                for expr in &block.body[1..] {
                    let val = self.eval_expr(expr)?;
                    if val != prev {
                        return Ok(Value::Bool(false));
                    }
                    prev = val;
                }

                Ok(Value::Bool(true))
            }

            "sum" => {
                let mut acc = 0.0_f32;

                for expr in &block.body {
                    match self.eval_expr(expr)? {
                        Value::Invalid => return Ok(Value::Invalid),
                        Value::Number(x) => acc += x,
                        val => panic!("todo: expected num, got {:?}", val),
                    }
                }

                Ok(Value::Number(acc))
            }

            name => Err(EvalError::UnknownBlock(name.into())),
        }
    }

    fn eval_named_block(&mut self, block: &NamedBlock) -> Result<Value, EvalError> {
        let mut scope = Scope::from_defs(&block.defs);
        let mut child_ctx = self.child(&mut scope);

        child_ctx.eval_named_block_inner(block)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pest::Parser;

    #[test]
    fn parse_the_kitchen_sink() {
        let input = r#"
// a
profile "kitchen sink" {
    define {
        k = "value" // b
        k = "a \"quoted\" value"
        k = true
        k = 123
        k = value
        k = [k; !k; k=a; k!=a|b; k="a|b"|c]
        k = [k !k k=a k!=a|b k="a|b"|c]
    }

    any? { true // split
           false; when {
               [highway=path] => true
               else           => when { true => false; false => true }
           }
    }
}
"#;

        let parsed = ProfileParser::parse(Rule::top_level, &input).expect("parse okay");

        for pair in parsed {
            println!("---> {:?}", pair);
        }
    }

    #[test]
    fn parse_to_ast() {
        let input = r#"
// Simple test profile
profile "test" {
    define {
        dismount?   = [bicycle=dismount]
        is-wet?     = false
        valley-mode = when {
            eq? { hills; 2 } => 80
            else             => 0
        }
        base-cost   = 123
        this        = true
        that        = "the other"
        bar         = [highway=path; access; access!=private]
    }

    node-penalty {
        when {
            [bicycle=no|private] => invalid
            [bicycle=dismount]   => 2.0
            else                 => 0
        }
    }

    way-penalty {
        when {
            [route=ferry] => invalid
            else          => 0
        }
    }

    cost-factor {
        when {
            unpaved? => 1.2
            else     => 0
        }
    }
}
"#;

        let profile = parse(input).expect("parse success");
        assert_eq!(profile.name, "test");
    }

    #[test]
    fn evaluate_globals() {
        let input = r#"
profile "test" {
    define {
        a = 1
        b = a
        c = false
        d = any? { c; false }
        e = any? { c; false; b }
        f = when {
           c            => "c"
           any? { d e } => "d or e"
        }
        g = sum { a; 2 }
        h = sum { invalid; a; 2 }
    }
}
"#;
        let profile = parse(input).expect("parse success");
        let mut runtime = ProfileRuntime::from(&profile).expect("create runtime");

        let expected = vec![
            ("b", Value::Number(1.0)),
            ("e", Value::Bool(true)),
            ("f", Value::String("d or e".into())),
            ("g", Value::Number(3.0)),
            ("h", Value::Invalid),
        ];

        for (key, val) in expected.into_iter() {
            let resolved = runtime
                .global_context()
                .eval_expr(&Expression::Ident(key.into()))
                .expect("lookup");

            assert_eq!(resolved, val);
        }
    }
}
