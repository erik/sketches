use pest::error::Error;
use pest::Parser;

#[derive(Parser)]
#[grammar = "profile/grammar.pest"]
struct Grammar;

#[derive(Debug, Clone)]
pub enum TagPattern<T> {
    Exists(T),
    NotExists(T),
    OneOf(T, Vec<T>),
    NoneOf(T, Vec<T>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Invalid,
    Bool(bool),
    Number(f32),
}

impl Value {
    pub(super) fn is_truthy(&self) -> bool {
        !matches!(self, Value::Bool(false) | Value::Invalid)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Value),
    Ident(String),
    TagPattern(Vec<TagPattern<String>>),
    Block(Block),
    WhenBlock(WhenBlock),
}

pub(super) type Def = (String, Expression);
pub(super) type Definitions = Vec<Def>;

#[derive(Debug, Clone)]
pub struct Block {
    pub defs: Definitions,
    pub name: String,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct WhenBlock(pub Vec<WhenClause>);

#[derive(Debug, Clone)]
pub struct WhenClause {
    pub condition: Expression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ProfileParser {
    pub name: String,
    pub constant_defs: Definitions,

    pub node_penalty: Option<Expression>,
    pub way_penalty: Option<Expression>,
    pub cost_factor: Option<Expression>,
}

impl ProfileParser {
    pub fn parse(source: &str) -> Result<ProfileParser, Error<Rule>> {
        let mut syntax_tree = Grammar::parse(Rule::top_level, source)?;
        let root_node = syntax_tree.next().unwrap();

        Ok(parse_profile(root_node.into_inner()))
    }
}

fn parse_profile(mut pairs: pest::iterators::Pairs<Rule>) -> ProfileParser {
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
                let block = parse_block(node.into_inner());
                // TODO: still a bit hacky here...
                let block_expr = Expression::Block(Block {
                    body: block.body,
                    name: "return!".into(),
                    defs: block.defs,
                });

                match block.name.as_str() {
                    "node-penalty" => node_penalty = Some(block_expr),
                    "way-penalty" => way_penalty = Some(block_expr),
                    "cost-factor" => cost_factor = Some(block_expr),
                    name => panic!("unexpected block: {:?}", name),
                }
            }

            _ => panic!("unexpected"),
        }
    }

    ProfileParser {
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
        Rule::named_block => Block(parse_block(pair.into_inner())),
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

fn parse_block(mut pairs: pest::iterators::Pairs<Rule>) -> Block {
    let name = pairs.next().unwrap().as_str().into();
    let mut defs = Definitions::new();
    let mut body = vec![];

    for node in pairs {
        match node.as_rule() {
            Rule::define_block => defs = parse_define_block(node.into_inner()),
            _ => body.push(parse_expr(node)),
        }
    }

    Block { defs, name, body }
}

fn parse_tag_expr(pairs: pest::iterators::Pairs<Rule>) -> Vec<TagPattern<String>> {
    let mut exprs = vec![];

    for node in pairs {
        let expr = match node.as_rule() {
            Rule::tag_expr_binary_clause => {
                let mut node = node.into_inner();
                let key = parse_as_str(node.next().unwrap());
                let op = node.next().unwrap();
                let patterns = node
                    .into_iter()
                    .map(|x| parse_as_str(x).into())
                    .collect::<Vec<_>>();

                match op.as_rule() {
                    Rule::op_eq => TagPattern::OneOf(key.into(), patterns),
                    Rule::op_neq => TagPattern::NoneOf(key.into(), patterns),
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

        let parsed = Grammar::parse(Rule::top_level, &input).expect("parse okay");

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

        let profile = ProfileParser::parse(input).expect("parse success");
        assert_eq!(profile.name, "test");
    }
}
