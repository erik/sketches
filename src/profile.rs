use pest::error::Error;
use pest::Parser;

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
    NamedBlock(NamedBlock),
    WhenBlock(WhenBlock),
}

type Def = (String, Expression);
type Definitions = Vec<Def>;

#[derive(Debug)]
pub struct NamedBlock {
    defs: Definitions,
    name: String,
    body: Vec<Expression>,
}

#[derive(Debug)]
pub struct WhenBlock(Vec<WhenClause>);

#[derive(Debug)]
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

    if root_node.as_rule() != Rule::profile {
        panic!("unexpected: {:?}", root_node.as_rule());
    }

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

        node_penalty: node_penalty,
        way_penalty: way_penalty,
        cost_factor: cost_factor,
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
    match pair.as_rule() {
        Rule::bool => Expression::Bool(pair.as_str() == "true"),
        Rule::number => Expression::Number(pair.as_str().parse().unwrap()),
        Rule::string => Expression::String(parse_as_str(pair).into()),
        Rule::tag_expr => Expression::TagPattern(parse_tag_expr(pair.into_inner())),
        Rule::when_block => Expression::WhenBlock(parse_when_block(pair.into_inner())),
        Rule::named_block => Expression::NamedBlock(parse_named_block(pair.into_inner())),
        Rule::ident => Expression::Ident(parse_as_str(pair).into()),
        Rule::invalid => Expression::Invalid,
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
                Rule::else_ => Expression::Bool(true),
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
        Rule::ident => &pair.as_str(),
        Rule::string => {
            let s = &pair.as_str();
            &s[1..s.len() - 1]
        }

        r => panic!("unexpected rule: {:?}", r),
    }
}

struct EvaluationContext<'a> {
    profile: &'a Profile,
}

impl<'a> EvaluationContext<'a> {
    fn score_node(&self) -> f32 {
        0.0
    }
    fn score_way(&self) -> f32 {
        0.0
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
}
