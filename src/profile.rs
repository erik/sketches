use std::collections::HashMap;

use pest::error::Error;
use pest::Parser;

use crate::tags::{EmptyTagSource, TagSource};

#[derive(Parser)]
#[grammar = "profile.pest"]
pub struct ProfileParser;

#[derive(Debug, Clone)]
pub enum TagPattern {
    Exists(String),
    NotExists(String),
    OneOf(String, Vec<String>),
    NoneOf(String, Vec<String>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Invalid,
    Bool(bool),
    Number(f32),
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

// TODO: switch to smartstring
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
    constant_defs: Definitions,

    node_penalty: Option<NamedBlock>,
    way_penalty: Option<NamedBlock>,
    cost_factor: Option<NamedBlock>,
}

impl Profile {
    pub fn parse(source: &str) -> Result<Profile, Error<Rule>> {
        let mut syntax_tree = ProfileParser::parse(Rule::top_level, source)?;
        let root_node = syntax_tree.next().unwrap();

        Ok(parse_profile(root_node.into_inner()))
    }
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
                let mut block = parse_named_block(node.into_inner());
                // TODO: hacky
                let block_name = block.name;
                block.name = "sum".into();
                match block_name.as_str() {
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
        constant_defs: defs,

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
                    Rule::op_eq => TagPattern::OneOf(key.into(), patterns.collect()),
                    Rule::op_neq => TagPattern::NoneOf(key.into(), patterns.collect()),
                    rule => panic!("unexpected rule: {:?}", rule),
                }
            }

            Rule::tag_expr_exists_clause => {
                let mut node = node.into_inner();
                let key = parse_as_str(node.next().unwrap());

                TagPattern::Exists(key.into())
            }

            Rule::tag_expr_not_exists_clause => {
                let mut node = node.into_inner();
                let key = parse_as_str(node.next().unwrap());

                TagPattern::NotExists(key.into())
            }

            rule => panic!("unexpected: {:?}", rule),
        };

        exprs.push(expr);
    }

    exprs
}

fn parse_as_str(pair: pest::iterators::Pair<Rule>) -> &str {
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
struct Scope<'a> {
    scope: HashMap<String, LazyValue<'a>>,
    parent: Option<&'a Scope<'a>>,
}

#[derive(Debug, Clone)]
enum LazyValue<'a> {
    Evaluated(Value),
    Unevaluated(&'a Expression),
}

impl<'a> Scope<'a> {
    fn empty() -> Scope<'a> {
        Scope {
            scope: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(scope: &'a Scope) -> Scope<'a> {
        Scope {
            scope: HashMap::new(),
            parent: Some(scope),
        }
    }

    fn get_child(&'a self, definitions: &'a Definitions) -> Scope<'a> {
        let scope = definitions
            .iter()
            .map(|(k, v)| (k.into(), LazyValue::Unevaluated(v)))
            .collect();

        Scope {
            scope,
            parent: Some(self),
        }
    }

    fn set(&mut self, k: &str, v: Value) {
        self.scope.insert(k.into(), LazyValue::Evaluated(v));
    }

    fn get(&'a self, k: &str) -> Option<&'a LazyValue> {
        self.scope
            .get(k)
            .or_else(|| self.parent.and_then(|scope| scope.get(k)))
    }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    Lookup(String),
    UnknownBlock(String),
    TagNotSupported,
}

pub struct EvalContext<'a, T>
where
    T: TagSource,
{
    scope: Scope<'a>,
    source: Option<&'a T>,
}

impl<T: TagSource> EvalContext<'_, T> {
    fn root() -> EvalContext<'static, T> {
        EvalContext {
            scope: Scope::empty(),
            source: None,
        }
    }
}

#[derive(Debug)]
pub struct ProfileRuntime {
    constant_scope: Scope<'static>,

    way_penalty: Option<NamedBlock>,
}

impl ProfileRuntime {
    pub fn from(profile: Profile) -> Result<ProfileRuntime, EvalError> {
        let mut rt = ProfileRuntime {
            constant_scope: Scope::empty(),
            way_penalty: profile.way_penalty,
        };
        rt.eval_constants(&profile.constant_defs)?;
        Ok(rt)
    }

    fn eval_constants(&mut self, defs: &Definitions) -> Result<(), EvalError> {
        let mut context = EvalContext::<EmptyTagSource>::root();

        for (ident, expr) in defs {
            let value = context.eval_expr(expr)?;
            context.scope.set(ident, value);
        }

        self.constant_scope = context.scope;
        Ok(())
    }

    pub fn score_way<T: TagSource>(&self, tags: &T) -> Result<f32, EvalError> {
        match &self.way_penalty {
            None => Ok(0.0),
            Some(block) => {
                match self.with_tag_source(tags).eval_named_block(&block)? {
                    Value::Number(score) => Ok(score),
                    // TODO: Formally specify this somehow. Result<Option<f32>>?
                    Value::Invalid => Ok(100_000_000.0),
                    // TODO: recover, don't panic
                    _ => panic!("score_way returned a non-number"),
                }
            }
        }
    }

    pub fn without_tag_source(&self) -> EvalContext<'_, EmptyTagSource> {
        EvalContext {
            scope: Scope::with_parent(&self.constant_scope),
            source: None,
        }
    }

    pub fn with_tag_source<'a, T: TagSource>(&'a self, source: &'a T) -> EvalContext<'a, T> {
        EvalContext {
            scope: Scope::with_parent(&self.constant_scope),
            source: Some(source),
        }
    }
}

impl<'a, T: TagSource> EvalContext<'a, T> {
    fn child_context(&'a self, scope: Scope<'a>) -> EvalContext<T> {
        EvalContext {
            scope,
            source: self.source,
        }
    }

    fn get_and_eval(&mut self, key: &str) -> Result<Value, EvalError> {
        let lazy_val = self
            .scope
            .get(key)
            .ok_or_else(|| EvalError::Lookup(key.into()))?;

        let value = match *lazy_val {
            LazyValue::Evaluated(val) => val,
            LazyValue::Unevaluated(expr) => {
                // TODO: avoid the clone, this branch seems to be expensive.
                let expr = expr.clone();
                let val = self.eval_expr(&expr)?;

                // TODO: We're only setting variables in the local scope, which
                // means they could need to be recalculated in a sibling.  not
                // optimal

                self.scope.set(key, val.clone());
                val
            }
        };

        Ok(value)
    }

    fn eval_expr(&mut self, expr: &Expression) -> Result<Value, EvalError> {
        use Expression::*;

        // TODO: Avoid cloning Value in each branch
        let value = match expr {
            Literal(val) => *val,
            Ident(name) => self.get_and_eval(name)?,

            NamedBlock(block) => self.eval_named_block(block)?,
            TagPattern(patterns) => self.eval_tag_patterns(patterns)?,
            WhenBlock(block) => self.eval_when_block(block)?,
        };

        Ok(value)
    }

    fn eval_tag_patterns(&mut self, patterns: &[TagPattern]) -> Result<Value, EvalError> {
        let source = match self.source {
            Some(s) => s,
            None => return Err(EvalError::TagNotSupported),
        };

        for pattern in patterns {
            use TagPattern::*;
            let matches = match pattern {
                Exists(key) => source.has_tag(key),
                NotExists(key) => !source.has_tag(key),
                OneOf(key, values) => source
                    .get_tag(key)
                    .map(|val| values.iter().any(|v| *v == val))
                    .unwrap_or(false),
                NoneOf(key, values) => source
                    .get_tag(key)
                    .map(|val| !values.iter().any(|v| *v == val))
                    .unwrap_or(true),
            };

            if !matches {
                return Ok(Value::Bool(false));
            }
        }

        Ok(Value::Bool(true))
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
        // TODO: avoid doing string matches
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
        // Avoid creating unnecessary children if there are no
        // definitions to pollute the scope
        if block.defs.is_empty() {
            self.eval_named_block_inner(block)
        } else {
            let scope = self.scope.get_child(&block.defs);
            self.child_context(scope).eval_named_block_inner(block)
        }
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

        let profile = Profile::parse(input).expect("parse success");
        assert_eq!(profile.name, "test");
    }

    #[test]
    fn evaluate_constants() {
        let input = r#"
profile "test" {
    define {
        a = 1
        b = a
        c = false
        d = any? { c; false }
        e = any? { c; false; b }
        f = sum { a; 2 }
        g = sum { invalid; a; 2 }
    }
}
"#;
        let profile = Profile::parse(input).expect("parse success");
        let runtime = ProfileRuntime::from(profile).expect("create runtime");

        let expected = vec![
            ("b", Value::Number(1.0)),
            ("e", Value::Bool(true)),
            ("f", Value::Number(3.0)),
            ("g", Value::Invalid),
        ];

        for (key, val) in expected.into_iter() {
            let resolved = runtime
                .without_tag_source()
                .eval_expr(&Expression::Ident(key.into()))
                .expect("lookup");

            assert_eq!(resolved, val);
        }
    }
}
