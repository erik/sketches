use pest::error::Error;
use pest::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "profile.pest"]
pub struct ProfileParser;

#[derive(Debug)]
pub enum TagPattern {
    Exists { key: String },
    NotExists { key: String },
    OneOf { key: String, values: Vec<String> },
    NoneOf { key: String, values: Vec<String> },
}

#[derive(Debug)]
pub enum Expression {
    Bool(bool),
    Ident(String),
    Invalid,
    Number(f32),
    String(String),
    TagPattern(Vec<TagPattern>),
    Block(Block),
}

#[derive(Debug)]
pub struct WhenClause {
    condition: Expression,
    value: Expression,
}

#[derive(Debug)]
pub struct Scope(HashMap<String, Expression>);

#[derive(Debug)]
pub struct NamedBlock {
    pub name: String,
    pub body: Vec<Expression>,
}

#[derive(Debug)]
pub enum Block {
    Named(NamedBlock),
    When(Vec<WhenClause>),
}

#[derive(Debug)]
pub struct Profile {
    name: String,
    globals: Scope,

    way_cost: NamedBlock,
    node_cost: NamedBlock,
}

struct ProfileBuilder {
    name: Option<String>,
    globals: Scope,
}

fn parse(source: &str) -> Result<Profile, Error<Rule>> {
    let syntax_tree = ProfileParser::parse(Rule::top_level, source)?;
    for pair in syntax_tree {
        match pair.as_rule() {
            Rule::profile => {
                let mut pair = pair.into_inner();
                let name = parse_str(pair.next().unwrap());
                let body = parse_block(pair.next().unwrap());
                println!("name = {:?}. now => {:?}", name, body);
            }

            Rule::EOI => break,
            rule => panic!("unexpected: {:?}", rule),
        }
    }

    todo!()
}

fn parse_block(pair: pest::iterators::Pair<Rule>) -> Vec<Expression> {
    vec![]
}

fn parse_str<'i>(pair: pest::iterators::Pair<'i, Rule>) -> &'i str {
    match pair.as_rule() {
        Rule::string => {
            let s = &pair.as_str();
            &s[1..s.len() - 1]
        }

        r => panic!("unexpected rule: {:?}", r),
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
    }

    any? { true; // split
           false }
    when {
        [highway=path] => 1
        else           => when { true => false; false => true }
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
        base-cost = 123
    }
}
"#;

        parse(input).expect("foo");
    }
}
