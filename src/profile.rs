#[derive(Parser)]
#[grammar = "profile.pest"]
pub struct ProfileParser;

pub enum TagPatternNode {
    Exists { key: String },
    NotExists { key: String },
    OneOf { key: String, values: Vec<String> },
    NoneOf { key: String, values: Vec<String> },
}

pub enum AstNode {
    Invalid,
    Number(f32),
    String(String),
    Bool(bool),
    Ident(String),
    TagPattern(Vec<TagPatternNode>),

    Assignment {
        // TODO: should this be an ident?
        ident: String,
        value: Box<AstNode>,
    },
    Block {
        name: String,
        body: Box<Vec<AstNode>>,
    },
    WhenBlock {
        clauses: Vec<(Box<AstNode>, Box<AstNode>)>,
        _else: Box<AstNode>,
    },
}

#[cfg(test)]
mod test {
    use super::*;
    use pest::Parser;

    #[test]
    fn kitchen_sink_parse() {
        let input = r#"
// a
key = "value" // b
key = "a \"quoted\" value"
true
123
invalid
value
[highway; !highway; highway=path; highway!=tunnel|bridge; highway="s p" | a | "|c e"]
any? { true; // split
        false }
when {
  [highway=path] => 1
  else           => when { true => false; false => true }
}
"#;

        let parsed = ProfileParser::parse(Rule::top_level, &input).expect("parse okay");

        for pair in parsed {
            println!("---> {:?}", pair);
        }
    }
}
