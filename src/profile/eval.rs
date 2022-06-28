use std::collections::HashMap;

use crate::tags::{CompactString, EmptyTagSource, TagSource};

use super::{Definitions, Expression, NamedBlock, Profile, TagPattern, Value, WhenBlock};

#[derive(Debug)]
struct Scope(Vec<HashMap<CompactString, Value>>);

impl Scope {
    fn empty() -> Scope {
        Scope(vec![HashMap::new()])
    }

    fn push(&mut self) {
        self.0.push(HashMap::new())
    }

    fn pop(&mut self) -> HashMap<CompactString, Value> {
        self.0.pop().expect("scope stack is empty!")
    }

    fn set(&mut self, k: CompactString, v: Value) {
        self.0
            .last_mut()
            .expect("scope stack is empty")
            .insert(k, v);
    }

    fn get(&self, k: &CompactString) -> Option<&Value> {
        self.0.iter().rev().find_map(|map| map.get(k))
    }
}

#[derive(Debug, Clone)]
pub enum EvalError {
    Lookup(String),
    UnknownBlock(String),
    TagNotSupported,
}

pub struct EvalContext<'a, T> {
    constants: Option<&'a HashMap<CompactString, Value>>,
    variables: Scope,
    source: Option<&'a T>,
}

impl<T: TagSource<CompactString, CompactString>> EvalContext<'_, T> {
    fn empty() -> EvalContext<'static, T> {
        EvalContext {
            constants: None,
            variables: Scope::empty(),
            source: None,
        }
    }
}

pub struct ProfileRuntime {
    constants: HashMap<CompactString, Value>,

    way_penalty: Option<NamedBlock>,
}

impl ProfileRuntime {
    pub fn from(profile: Profile) -> Result<ProfileRuntime, EvalError> {
        let mut rt = ProfileRuntime {
            constants: HashMap::new(),
            way_penalty: profile.way_penalty,
        };
        rt.eval_constants(&profile.constant_defs)?;
        Ok(rt)
    }

    fn eval_constants(&mut self, defs: &Definitions) -> Result<(), EvalError> {
        let mut context = EvalContext::<EmptyTagSource>::empty();

        for (ident, expr) in defs {
            let value = context.eval_expr(expr)?;
            context.variables.set(ident.clone(), value);
        }

        self.constants = context.variables.pop();
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
            constants: Some(&self.constants),
            variables: Scope::empty(),
            source: None,
        }
    }

    pub fn with_tag_source<'a, T>(&'a self, source: &'a T) -> EvalContext<'a, T> {
        EvalContext {
            constants: Some(&self.constants),
            variables: Scope::empty(),
            source: Some(source),
        }
    }
}

impl<'a, T: TagSource<CompactString, CompactString>> EvalContext<'a, T> {
    fn get_and_eval(&mut self, key: &CompactString) -> Result<Value, EvalError> {
        self.variables
            .get(key)
            .cloned()
            .or_else(|| self.constants?.get(key).cloned())
            .ok_or_else(|| EvalError::Lookup(key.as_str().into()))
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
            self.variables.push();

            for (ident, expr) in &block.defs {
                let val = self.eval_expr(expr)?;
                self.variables.set(ident.clone(), val);
            }

            let result = self.eval_named_block_inner(block);

            self.variables.pop();
            result
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
