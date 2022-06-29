use std::collections::HashMap;

use crate::{
    profile::{Expression, TagPattern, Value},
    tags::{CompactString, TagDict, TagDictId, TagSource, UNKNOWN_TAG_ID},
};

use super::NestedScope;

#[derive(Debug, Clone)]
enum BlockTy {
    Any,
    All,
    None,
    Sum,
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

struct Builder<'a> {
    constants: HashMap<CompactString, u8>,
    assignments: NestedScope<CompactString, u16>,
    defs: NestedScope<CompactString, Expr>,
    tag_dict: &'a TagDict<CompactString>,
    num_ids: u16,
}

impl<'a> Builder<'a> {
    fn new(tag_dict: &'a TagDict<CompactString>) -> Self {
        Self {
            tag_dict,
            constants: HashMap::new(),
            assignments: NestedScope::empty(),
            defs: NestedScope::empty(),
            num_ids: 0,
        }
    }

    fn compact_tag(&self, key: &CompactString) -> TagDictId {
        self.tag_dict.to_compact(key).unwrap_or(UNKNOWN_TAG_ID)
    }

    fn add_constant(&mut self, ident: CompactString) -> u8 {
        let new_id = self.constants.len() as u8;
        self.constants.insert(ident, new_id);
        new_id
    }

    fn add_variable(&mut self, ident: CompactString) -> u16 {
        let new_id = self.num_ids + 1;
        self.assignments.set(ident, new_id);
        self.num_ids += 1;
        new_id
    }

    fn lower(&mut self, expr: &Expression) -> Expr {
        match expr {
            Expression::Literal(val) => Expr::Literal(*val),

            Expression::Ident(ident) => {
                if let Some(def) = self.defs.get(ident) {
                    let def = Box::new(def.clone());
                    let var_id = match self.assignments.get(ident) {
                        Some(id) => *id,
                        None => self.add_variable(ident.clone()),
                    };
                    Expr::LookupOrCompute(var_id, def.clone())
                } else if let Some(const_id) = self.constants.get(ident) {
                    Expr::LookupConstant(*const_id)
                } else {
                    panic!("undefined var or const: {:?}", ident);
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

                Expr::TagPattern(patterns)
            }

            Expression::NamedBlock(block) => {
                self.assignments.push();
                self.defs.push();

                for (ident, expr) in &block.defs {
                    let expr = self.lower(expr);
                    self.defs.set(ident.clone(), expr);
                }

                let ty = match block.name.as_str() {
                    "any?" => BlockTy::Any,
                    "all?" => BlockTy::All,
                    "none?" => BlockTy::None,
                    "sum" => BlockTy::Sum,
                    other => panic!("unknown block type: {:?}", other),
                };

                let body = block.body.iter().map(|expr| self.lower(expr)).collect();

                self.defs.pop();
                self.assignments.pop();

                Expr::Block(ty, body)
            }

            Expression::WhenBlock(clauses) => Expr::When(
                clauses
                    .0
                    .iter()
                    .map(|clause| {
                        let cond = self.lower(&clause.condition);
                        let value = self.lower(&clause.value);

                        WhenBlockClause(cond, value)
                    })
                    .collect(),
            ),
        }
    }

    fn build(&mut self, expr: &Expression) -> LoweredExpression {
        let expr = self.lower(expr);

        LoweredExpression {
            expr,
            num_variables: self.num_ids,
        }
    }
}

struct LoweredExpression {
    expr: Expr,
    num_variables: u16,
}

struct Runtime {
    constants: Vec<Value>,
}

impl Runtime {}

struct EvalContext<'a, T> {
    constants: &'a [Value],
    variables: Vec<Option<Value>>,
    tag_source: Option<&'a T>,
}

impl<'a, T> EvalContext<'a, T>
where
    T: TagSource<TagDictId, TagDictId>,
{
    fn evaluate(&mut self, expr: &Expr) -> Result<Value, ()> {
        match expr {
            Expr::Literal(val) => Ok(*val),

            Expr::LookupConstant(id) => self
                .constants
                .get(*id as usize)
                .cloned()
                .ok_or_else(|| panic!("bug: bad const lookup")),

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

    fn evaluate_block(&mut self, ty: &BlockTy, body: &[Expr]) -> Result<Value, ()> {
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

            BlockTy::Sum => {
                let mut acc = 0.0;
                for expr in body {
                    match self.evaluate(expr)? {
                        Value::Invalid => return Ok(Value::Invalid),
                        Value::Number(n) => acc += n,
                        other => panic!("Unexpected type from sum block: {:?}", other),
                    }
                }

                Ok(Value::Number(acc))
            }
        }
    }

    fn evaluate_when(&mut self, clauses: &[WhenBlockClause]) -> Result<Value, ()> {
        for clause in clauses {
            let condition = self.evaluate(&clause.0)?;
            if condition.is_truthy() {
                let value = self.evaluate(&clause.1)?;
                return Ok(value);
            }
        }

        panic!("Fallthrough -> no else block for when");
    }

    fn evaluate_tag_patterns(&mut self, patterns: &[TagPattern<TagDictId>]) -> Result<Value, ()> {
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
    use super::super::*;
    use super::*;

    use crate::tags::TagDict;

    fn common_tags() -> Vec<&'static str> {
        vec![
            "highway", "surface", "asphalt", "unpaved", "bicycle", "yes", "no", "access", "private",
        ]
    }

    #[test]
    fn runtime_stmt_builder() {
        let input = include_str!("../../profiles/cxb.mint");
        let profile = Profile::parse(input).expect("parse success");

        let mut tag_dict = TagDict::new();
        for &tag in &common_tags() {
            tag_dict.insert(tag.into());
        }

        let mut builder = Builder::new(&tag_dict);

        for (ident, _) in &profile.constant_defs {
            builder.add_constant(ident.clone());
        }

        let expr = Expression::NamedBlock(profile.way_penalty.unwrap().clone());

        let stmt = builder.build(&expr);

        println!("evaluated to: {:#?}", stmt.expr);
    }
}
