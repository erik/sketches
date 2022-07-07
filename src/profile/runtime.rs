use std::collections::HashMap;
use std::hash::Hash;

use crate::tags::{CompactString, EmptyTagSource, TagDict, TagDictId, TagSource, UNKNOWN_TAG_ID};

use super::parse::{Definitions, Expression, Profile, TagPattern, Value};

#[derive(Debug)]
pub struct NestedScope<K, V>(Vec<HashMap<K, V>>);

impl<K: Eq + Hash, V> NestedScope<K, V> {
    pub fn empty() -> Self {
        NestedScope(vec![HashMap::new()])
    }

    pub fn push(&mut self) {
        self.0.push(HashMap::new())
    }

    pub fn pop(&mut self) -> HashMap<K, V> {
        self.0.pop().expect("scope stack is empty!")
    }

    pub fn set(&mut self, k: K, v: V) {
        self.0
            .last_mut()
            .expect("scope stack is empty")
            .insert(k, v);
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.0.iter().rev().find_map(|map| map.get(k))
    }
}

#[derive(Debug, Clone)]
enum BlockTy {
    All,
    Any,
    None,
    Return,
    Sum,
}

#[derive(Debug, Clone)]
enum Arity {
    Unary,
    Variadic,
}

impl BlockTy {
    const fn arity(&self) -> Arity {
        match self {
            BlockTy::Any => Arity::Variadic,
            BlockTy::All => Arity::Variadic,
            BlockTy::None => Arity::Variadic,
            BlockTy::Sum => Arity::Variadic,
            BlockTy::Return => Arity::Unary,
        }
    }
}

#[derive(Debug, Clone)]
struct WhenBlockClause(Expr, Expr);

#[derive(Debug, Clone)]
enum Expr {
    Literal(Value),
    LookupConstant(u8),
    LookupOrCompute(u16, Box<Expr>),
    Block(BlockTy, Vec<Expr>),
    When(Vec<WhenBlockClause>),
    TagPattern(Vec<TagPattern<TagDictId>>),
}

type VariableId = u16;

struct VariableMapping {
    // ident -> id
    ids: NestedScope<CompactString, VariableId>,
    // ident -> definition
    defs: NestedScope<CompactString, Expr>,

    next_id: VariableId,
}

impl VariableMapping {
    fn new() -> Self {
        Self {
            ids: NestedScope::empty(),
            defs: NestedScope::empty(),
            next_id: VariableId::default(),
        }
    }

    fn clear(&mut self) -> Self {
        std::mem::replace(self, Self::new())
    }

    fn push(&mut self) {
        self.ids.push();
        self.defs.push();
    }

    fn pop(&mut self) {
        self.ids.pop();
        self.defs.pop();
    }

    fn add_variable(&mut self, ident: &CompactString) -> VariableId {
        let new_id = self.next_id;
        self.ids.set(ident.clone(), new_id);
        self.next_id += 1;

        new_id
    }

    fn get_or_assign_id(&mut self, ident: &CompactString) -> VariableId {
        match self.ids.get(ident) {
            Some(&id) => id,
            None => self.add_variable(ident),
        }
    }

    fn add_definition(&mut self, ident: &CompactString, expr: Expr) {
        self.defs.set(ident.clone(), expr);
    }

    fn get_definition(&self, ident: &CompactString) -> Option<&Expr> {
        self.defs.get(ident)
    }
}

// TODO: needs a better name, it's not really a builder, but a compiler
struct Builder<'a> {
    constants: HashMap<CompactString, u8>,
    tag_dict: &'a TagDict<CompactString>,
    variables: VariableMapping,
}

impl<'a> Builder<'a> {
    fn new(constants: &Definitions, tag_dict: &'a TagDict<CompactString>) -> Self {
        Self {
            tag_dict,
            variables: VariableMapping::new(),
            constants: Self::build_const_map(constants),
        }
    }

    fn compact_tag(&self, key: &CompactString) -> TagDictId {
        self.tag_dict.to_compact(key).unwrap_or(UNKNOWN_TAG_ID)
    }

    fn build_const_map(defs: &Definitions) -> HashMap<CompactString, u8> {
        if defs.len() >= (u8::MAX as usize) {
            panic!("Too many constants defined")
        }

        defs.iter()
            .enumerate()
            .map(|(id, (ident, _))| (ident.clone(), id as u8))
            .collect()
    }

    fn lower(&mut self, expr: &Expression) -> Result<Expr, CompileError> {
        match expr {
            Expression::Literal(val) => Ok(Expr::Literal(*val)),

            Expression::Ident(ident) => {
                if let Some(def) = self.variables.get_definition(ident) {
                    let def = Box::new(def.clone());
                    let var_id = self.variables.get_or_assign_id(ident);

                    Ok(Expr::LookupOrCompute(var_id, def))
                } else if let Some(const_id) = self.constants.get(ident) {
                    Ok(Expr::LookupConstant(*const_id))
                } else {
                    Err(CompileError::UnknownIdent(ident.as_str().into()))
                }
            }

            Expression::TagPattern(patterns) => {
                use TagPattern::*;
                let patterns = patterns
                    .iter()
                    .map(|pat| match pat {
                        Exists(k) => Exists(self.compact_tag(k)),
                        NotExists(k) => NotExists(self.compact_tag(k)),
                        OneOf(k, vs) => OneOf(
                            self.compact_tag(k),
                            vs.iter().map(|v| self.compact_tag(v)).collect(),
                        ),
                        NoneOf(k, vs) => NoneOf(
                            self.compact_tag(k),
                            vs.iter().map(|v| self.compact_tag(v)).collect(),
                        ),
                    })
                    .collect();

                Ok(Expr::TagPattern(patterns))
            }

            Expression::Block(block) => {
                self.variables.push();

                for (ident, expr) in &block.defs {
                    let expr = self.lower(expr)?;
                    self.variables.add_definition(ident, expr);
                }

                let ty = match block.name.as_str() {
                    "any?" => BlockTy::Any,
                    "all?" => BlockTy::All,
                    "none?" => BlockTy::None,
                    "sum" => BlockTy::Sum,
                    "return!" => BlockTy::Return,
                    other => return Err(CompileError::UnknownBlockTy(other.into())),
                };

                let arg_range = match ty.arity() {
                    Arity::Unary => 1..=1,
                    Arity::Variadic => (1..=usize::MAX),
                };

                if !arg_range.contains(&block.body.len()) {
                    panic!("invalid number of arguments given");
                }

                let mut body = vec![];
                for expr in &block.body {
                    body.push(self.lower(expr)?);
                }

                self.variables.pop();

                Ok(Expr::Block(ty, body))
            }

            Expression::WhenBlock(exprs) => {
                let mut clauses = vec![];
                for clause in &exprs.0 {
                    let cond = self.lower(&clause.condition)?;
                    let value = self.lower(&clause.value)?;

                    clauses.push(WhenBlockClause(cond, value))
                }

                Ok(Expr::When(clauses))
            }
        }
    }

    fn build(&mut self, expr: &Expression) -> Result<RunnableExpr, CompileError> {
        let expr = self.lower(expr)?;
        let variables = self.variables.clear();

        Ok(RunnableExpr { expr, variables })
    }
}

struct RunnableExpr {
    expr: Expr,
    variables: VariableMapping,
}

#[derive(Debug)]
pub enum CompileError {
    UnknownBlockTy(String),
    UnknownIdent(String),
    ConstEval(RuntimeError),
}

#[derive(Debug)]
pub enum RuntimeError {
    Internal(String),
    // TODO: could be compile time if we add a type-checking pass.
    TypeError { have: String, expected: String },
    WhenFallthrough,
    // TODO: Not an error, reconsider naming
    EarlyReturn(Value),
}

pub struct Runtime {
    constants: Vec<Value>,

    way_penalty: Option<RunnableExpr>,
    node_penalty: Option<RunnableExpr>,
    cost_factor: Option<RunnableExpr>,
}

impl Runtime {
    pub fn from(
        profile: &Profile,
        tag_dict: &TagDict<CompactString>,
    ) -> Result<Self, CompileError> {
        let mut builder = Builder::new(&profile.constant_defs, tag_dict);

        Ok(Self {
            constants: Self::evaluate_constants(&mut builder, &profile.constant_defs)?,

            way_penalty: profile
                .way_penalty
                .as_ref()
                .map(|expr| builder.build(expr))
                .transpose()?,
            node_penalty: profile
                .node_penalty
                .as_ref()
                .map(|expr| builder.build(expr))
                .transpose()?,
            cost_factor: profile
                .cost_factor
                .as_ref()
                .map(|expr| builder.build(expr))
                .transpose()?,
        })
    }

    fn evaluate_constants(
        builder: &mut Builder,
        defs: &Definitions,
    ) -> Result<Vec<Value>, CompileError> {
        let mut consts = vec![];

        for (_, def) in defs {
            let runnable = builder.build(def)?;

            let mut context = EvalContext::constant(&consts);
            let value = context
                .evaluate(&runnable.expr)
                .map_err(CompileError::ConstEval)?;

            consts.push(value);
        }

        Ok(consts)
    }

    pub fn score<T>(
        &self,
        source_node_tags: &T,
        target_node_tags: &T,
        way_tags: &T,
    ) -> Result<EdgeScore, RuntimeError>
    where
        T: TagSource<TagDictId, TagDictId>,
    {
        let mut penalty = 0.0;
        let mut cost_factor = 1.0;

        if let Some(expr) = &self.node_penalty {
            penalty += self.evaluate_expression(expr, source_node_tags)?;
            penalty += self.evaluate_expression(expr, target_node_tags)?;
        }

        if let Some(expr) = &self.way_penalty {
            penalty += self.evaluate_expression(expr, way_tags)?;
        }

        if let Some(expr) = &self.cost_factor {
            cost_factor += self.evaluate_expression(expr, way_tags)?;
        }

        Ok(EdgeScore {
            penalty,
            cost_factor,
        })
    }

    fn evaluate_expression<T>(
        &self,
        expr: &RunnableExpr,
        tag_source: &T,
    ) -> Result<f32, RuntimeError>
    where
        T: TagSource<TagDictId, TagDictId>,
    {
        let mut context = EvalContext::with_tag_source(
            &self.constants,
            expr.variables.next_id as usize,
            tag_source,
        );

        let val = match context.evaluate(&expr.expr) {
            Err(RuntimeError::EarlyReturn(val)) => Ok(val),
            res => res,
        }?;

        match val {
            Value::Number(score) => Ok(score),
            // TODO: Formally specify this somehow. Result<Option<f32>>?
            // TODO: Can easily overflow.
            Value::Invalid => Ok(500_000.0),
            // TODO: recover, don't panic
            val => Err(RuntimeError::TypeError {
                have: format!("{:?}", val),
                expected: "number|invalid".into(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct EdgeScore {
    pub penalty: f32,
    pub cost_factor: f32,
}

struct EvalContext<'a, T> {
    constants: &'a [Value],
    variables: Vec<Option<Value>>,
    tag_source: Option<&'a T>,
}

impl<'a> EvalContext<'a, EmptyTagSource> {
    fn constant(constants: &'a [Value]) -> EvalContext<'a, EmptyTagSource> {
        EvalContext {
            constants,
            variables: vec![],
            tag_source: None,
        }
    }
}

impl<'a, T> EvalContext<'a, T>
where
    T: TagSource<TagDictId, TagDictId>,
{
    fn with_tag_source(
        constants: &'a [Value],
        num_variables: usize,
        tag_source: &'a T,
    ) -> EvalContext<'a, T> {
        EvalContext {
            constants,
            variables: vec![None; num_variables],
            tag_source: Some(tag_source),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(val) => Ok(*val),

            Expr::LookupConstant(id) => {
                self.constants.get(*id as usize).cloned().ok_or_else(|| {
                    RuntimeError::Internal(format!("bad constant reference: {:?}", id))
                })
            }

            Expr::LookupOrCompute(id, def) => match self.variables[*id as usize] {
                Some(val) => Ok(val),
                None => {
                    let val = self.evaluate(def)?;
                    self.variables[*id as usize] = Some(val);

                    Ok(val)
                }
            },

            Expr::Block(ty, body) => self.evaluate_block(ty, body),
            Expr::When(clauses) => self.evaluate_when(clauses),
            Expr::TagPattern(patterns) => self.evaluate_tag_patterns(patterns),
        }
    }

    fn evaluate_block(&mut self, ty: &BlockTy, body: &[Expr]) -> Result<Value, RuntimeError> {
        match ty {
            BlockTy::Any => {
                for expr in body {
                    if self.evaluate(expr)?.is_truthy() {
                        return Ok(Value::Bool(true));
                    }
                }

                Ok(Value::Bool(false))
            }

            BlockTy::All => {
                for expr in body {
                    if !self.evaluate(expr)?.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            }

            BlockTy::None => {
                for expr in body {
                    if self.evaluate(expr)?.is_truthy() {
                        return Ok(Value::Bool(false));
                    }
                }

                Ok(Value::Bool(true))
            }

            BlockTy::Return => {
                // TODO: confirm block arity
                let val = self.evaluate(&body[0])?;
                Err(RuntimeError::EarlyReturn(val))
            }

            BlockTy::Sum => {
                let mut acc = 0.0;
                for expr in body {
                    match self.evaluate(expr)? {
                        Value::Invalid => return Ok(Value::Invalid),
                        Value::Number(n) => acc += n,
                        other => {
                            return Err(RuntimeError::TypeError {
                                have: format!("{:?}", other),
                                expected: "invalid|number".into(),
                            })
                        }
                    }
                }

                Ok(Value::Number(acc))
            }
        }
    }

    fn evaluate_when(&mut self, clauses: &[WhenBlockClause]) -> Result<Value, RuntimeError> {
        for clause in clauses {
            let condition = self.evaluate(&clause.0)?;
            if condition.is_truthy() {
                let value = self.evaluate(&clause.1)?;
                return Ok(value);
            }
        }

        Err(RuntimeError::WhenFallthrough)
    }

    fn evaluate_tag_patterns(
        &mut self,
        patterns: &[TagPattern<TagDictId>],
    ) -> Result<Value, RuntimeError> {
        let tag_source = self
            .tag_source
            .unwrap_or_else(|| panic!("no tags supported here"));

        for pattern in patterns {
            use TagPattern::*;
            let matches = match pattern {
                Exists(key) => tag_source.has_tag(key),

                NotExists(key) => !tag_source.has_tag(key),

                OneOf(key, values) => tag_source
                    .get_tag(key)
                    .map(|val| values.contains(val))
                    .unwrap_or(false),

                NoneOf(key, values) => tag_source
                    .get_tag(key)
                    .map(|val| !values.contains(val))
                    .unwrap_or(false),
            };

            if !matches {
                return Ok(Value::Bool(false));
            }
        }

        Ok(Value::Bool(true))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::tags::TagDict;

    fn common_tags() -> Vec<&'static str> {
        vec![
            "highway", "surface", "asphalt", "unpaved", "bicycle", "yes", "no", "access", "private",
        ]
    }

    #[test]
    fn build_full_runtime() {
        let input = include_str!("../../profiles/cxb.mint");
        let profile = Profile::parse(input).expect("parse success");

        let mut tag_dict = TagDict::new();
        for &tag in &common_tags() {
            tag_dict.insert(tag.into());
        }

        Runtime::from(&profile, &tag_dict).expect("create runtime");
    }

    #[test]
    fn evaluate_constants_for_runtime() {
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

        let mut tag_dict = TagDict::new();
        for &tag in &common_tags() {
            tag_dict.insert(tag.into());
        }

        let runtime = Runtime::from(&profile, &tag_dict).expect("create runtime");

        let expected = vec![
            Value::Number(1.0),
            Value::Number(1.0),
            Value::Bool(false),
            Value::Bool(false),
            Value::Bool(true),
            Value::Number(3.0),
            Value::Invalid,
        ];

        assert_eq!(expected, runtime.constants);
    }
}
