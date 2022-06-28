use std::collections::HashMap;

use crate::tags::{CompactString, EmptyTagSource, TagSource};

use super::{Definitions, Expression, NamedBlock, Profile, TagPattern, Value, WhenBlock};

#[derive(Debug)]
struct Scope<'a> {
    // TODO: don't clone key, store ref
    scope: HashMap<CompactString, LazyValue<'a>>,
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
            .map(|(k, v)| (k.clone(), LazyValue::Unevaluated(v)))
            .collect();

        Scope {
            scope,
            parent: Some(self),
        }
    }

    fn set(&mut self, k: CompactString, v: Value) {
        self.scope.insert(k, LazyValue::Evaluated(v));
    }

    fn get(&'a self, k: &CompactString) -> Option<&'a LazyValue> {
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

pub struct EvalContext<'a, T> {
    scope: Scope<'a>,
    source: Option<&'a T>,
}

impl<T: TagSource<CompactString, CompactString>> EvalContext<'_, T> {
    fn root() -> EvalContext<'static, T> {
        EvalContext {
            scope: Scope::empty(),
            source: None,
        }
    }
}

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
            context.scope.set(ident.clone(), value);
        }

        self.constant_scope = context.scope;
        Ok(())
    }

    pub fn score_way<T>(&self, tags: &T) -> Result<f32, EvalError>
    where
        T: TagSource<CompactString, CompactString>,
    {
        match &self.way_penalty {
            None => Ok(0.0),
            Some(block) => {
                match self.with_tag_source(tags).eval_named_block(block)? {
                    Value::Number(score) => Ok(score),
                    // TODO: Formally specify this somehow. Result<Option<f32>>?
                    // TODO: Can easily overflow.
                    Value::Invalid => Ok(500_000.0),
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

    pub fn with_tag_source<'a, T>(&'a self, source: &'a T) -> EvalContext<'a, T> {
        EvalContext {
            scope: Scope::with_parent(&self.constant_scope),
            source: Some(source),
        }
    }
}

impl<'a, T: TagSource<CompactString, CompactString>> EvalContext<'a, T> {
    fn child_context(&'a self, scope: Scope<'a>) -> EvalContext<T> {
        EvalContext {
            scope,
            source: self.source,
        }
    }

    fn get_and_eval(&mut self, key: &CompactString) -> Result<Value, EvalError> {
        let lazy_val = self
            .scope
            .get(key)
            .ok_or_else(|| EvalError::Lookup(key.as_str().into()))?;

        let value = match *lazy_val {
            LazyValue::Evaluated(val) => val,
            LazyValue::Unevaluated(expr) => {
                // TODO: avoid the clone, this branch seems to be expensive.
                let expr = expr.clone();
                let val = self.eval_expr(&expr)?;

                // TODO: We're only setting variables in the local scope, which
                // means they could need to be recalculated in a sibling.  not
                // optimal

                // TODO: avoid clone
                self.scope.set(key.clone(), val);
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
        let tag_source = self.source.ok_or(EvalError::TagNotSupported)?;

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
mod tests {
    use super::*;

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
