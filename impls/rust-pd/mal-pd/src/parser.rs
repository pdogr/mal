use crate::{ast::*, tokens::*, utils::escape};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::anychar,
    combinator::{complete, map, map_res, opt, recognize, value},
    multi::{many0, many1},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::borrow::Borrow;

#[allow(unused_macros)]
macro_rules! tag {
    ($i:ident::$b:ident($s: ident)) => {
        move |input: MalTokens<'a>| {
            let (_input, r) = take1(input)?;
            if let Some($i::$b($s)) = r.0.get(0) {
                return Ok((_input, $s.borrow()));
            }
            return Err(nom::Err::Error(nom::error::Error {
                input: _input,
                code: nom::error::ErrorKind::Tag,
            }));
        }
    };
    ($i:ident::$b:ident) => {
        tag(MalTokens(&[$i::$b]))
    };
}

#[allow(unused_macros)]
macro_rules! keyword {
    ($k: literal) => {
        complete(tag($k))
    };
}

fn take1<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalTokens<'a>> {
    take(1usize)(i)
}

fn parse_keyword<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalAtom> {
    map_res(tag!(MalToken::Sequence(s)), move |s: &str| {
        alt::<_, _, nom::error::Error<&str>, _>((
            value(MalAtom::Nil, keyword!("nil")),
            value(MalAtom::Bool(true), keyword!("true")),
            value(MalAtom::Bool(false), keyword!("false")),
        ))(s)
        .map(|(_, r)| r)
    })(i)
}

fn parse_literal<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalLiteral> {
    alt((
        map(tag!(MalToken::QuotedSequence(s)), |s: &str| {
            MalLiteral::Str(escape(s).into())
        }),
        map_res(tag!(MalToken::Sequence(s)), move |s: &str| {
            complete::<_, _, nom::error::Error<&str>, _>(alt((
                map(nom::character::complete::i64, MalLiteral::Int),
                map(nom::number::complete::double, MalLiteral::Float),
            )))(s)
            .map(|(_, r)| r)
        }),
    ))(i)
}

fn parse_symbol<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalSymbol> {
    map(tag!(MalToken::Sequence(s)), MalSymbol::new)(i)
}

fn parse_hashkey<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalAtom> {
    map_res(tag!(MalToken::Sequence(s)), move |s: &str| {
        preceded::<_, _, _, nom::error::Error<&str>, _, _>(tag(":"), recognize(many1(anychar)))(s)
            .map(|(_, r)| MalAtom::HashKey(r.into()))
    })(i)
}
fn parse_atom<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalAtom> {
    alt((
        parse_keyword,
        map(parse_literal, MalAtom::Literal),
        parse_hashkey,
        map(parse_symbol, MalAtom::Symbol),
    ))(i)
}

fn parse_list<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalList> {
    map(opt(many0(parse_type)), |ws| {
        MalList::new(ws.unwrap_or_default())
    })(i)
}

fn parse_vector<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalVec> {
    map(opt(many0(parse_type)), |ws| {
        MalVec::new(ws.unwrap_or_default())
    })(i)
}

fn parse_hashmap<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalHashMap> {
    map_res(opt(many0(parse_type)), |ws: Option<Vec<MalType>>| {
        let ws = ws.unwrap_or_default();
        if ws.len() & 1 == 1 {
            Err(nom::Err::Error(nom::error::Error {
                input: ws,
                code: nom::error::ErrorKind::Count,
            }))
        } else {
            let mut vec = Vec::new();
            for kv in ws.chunks_exact(2) {
                let (k, v) = kv.split_at(1);
                let k = match k.get(0).unwrap() {
                    MalType::Atom(a) => match &a {
                        MalAtom::Literal(_) | MalAtom::HashKey(_) => a,
                        _ => {
                            return Err(nom::Err::Error(nom::error::Error {
                                input: ws,
                                code: nom::error::ErrorKind::Tag,
                            }));
                        }
                    },
                    _ => {
                        return Err(nom::Err::Error(nom::error::Error {
                            input: ws,
                            code: nom::error::ErrorKind::Tag,
                        }));
                    }
                };
                let v = v.get(0).unwrap();
                vec.push((k.clone(), v.clone()));
            }
            Ok(MalHashMap::new(vec))
        }
    })(i)
}

pub fn parse_type<'a>(i: MalTokens<'a>) -> IResult<MalTokens<'a>, MalType> {
    alt((
        map(
            delimited(
                tag!(MalToken::LeftParen),
                parse_list,
                tag!(MalToken::RightParen),
            ),
            MalType::List,
        ),
        map(
            delimited(
                tag!(MalToken::LeftSquare),
                parse_vector,
                tag!(MalToken::RightSquare),
            ),
            MalType::Vector,
        ),
        map(
            delimited(
                tag!(MalToken::LeftBrace),
                parse_hashmap,
                tag!(MalToken::RightBrace),
            ),
            MalType::HashMap,
        ),
        map(preceded(tag!(MalToken::Quote), parse_type), |q| {
            MalType::Quoted(Box::new(q))
        }),
        map(preceded(tag!(MalToken::QuasiQuote), parse_type), |q| {
            MalType::QuasiQuoted(Box::new(q))
        }),
        map(preceded(tag!(MalToken::Unquote), parse_type), |q| {
            MalType::Unquote(Box::new(q))
        }),
        map(preceded(tag!(MalToken::Deref), parse_atom), |q| {
            MalType::Deref(q)
        }),
        map(
            preceded(tag!(MalToken::WithMeta), tuple((parse_type, parse_type))),
            |(a, b)| MalType::WithMeta(Box::new(a), Box::new(b)),
        ),
        map(preceded(tag!(MalToken::SpliceUnquote), parse_type), |q| {
            MalType::SpliceUnquote(Box::new(q))
        }),
        map(parse_atom, MalType::Atom),
    ))(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::MalLexer, tokens::MalTokens};
    use nom::*;

    #[allow(unused_macros)]
    macro_rules! check {
        ($fn: ident,$lit: literal,$expected: literal) => {{
            let t = MalLexer::lex($lit).unwrap_or_default();
            println!("{:?}", t);
            let t = MalTokens(t.as_slice());
            let (_, r) = $fn(t).finish().unwrap();
            assert_eq!($expected, format!("{}", r));
        }};
    }

    #[test]
    fn parse_int0() {
        check!(parse_literal, "123", "123");
    }

    #[test]
    fn parse_str0() {
        check!(parse_literal, "\"123\"", "\"123\"");
    }

    #[test]
    fn parse_sym0() {
        check!(parse_symbol, "abc", "abc");
    }

    #[test]
    fn parse_str1() {
        check!(parse_literal, "\"\\\\\"", "\"\\\\\"");
    }

    #[test]
    fn parse_atom0() {
        check!(parse_atom, "doge", "doge");
    }

    #[test]
    fn parse_type0() {
        check!(parse_type, "12345", "12345");
    }

    #[test]
    fn parse_list0() {
        check!(parse_list, "123 45 67     89    ", "(123 45 67 89)");
    }

    #[test]
    fn parse_list1() {
        check!(
            parse_list,
            "123 ( 4 5     ok is    it   working   ( 6      7 (        8 9 ) ) ) 67     89    ",
            "(123 (4 5 ok is it working (6 7 (8 9))) 67 89)"
        );
    }

    #[test]
    fn parse0() {
        check!(parse_type, "( + 2 (  * 3 4  ) ) \n\n\n\n", "(+ 2 (* 3 4))");
    }

    #[test]
    fn parse1() {
        check!(parse_type, "(    \"1231\")", "(\"1231\")");
    }

    #[test]
    fn parse2() {
        check!(parse_type, "(   )", "()");
    }
    #[test]
    fn parse3() {
        check!(parse_type, "(nil)", "(nil)");
    }

    #[test]
    fn parse5() {
        check!(parse_type, "\"\"", "\"\"");
    }

    #[test]
    fn parse6() {
        check!(parse_type, "\"abc (with parens)\"", "\"abc (with parens)\"");
    }
}
