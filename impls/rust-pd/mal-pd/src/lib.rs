#![feature(box_into_inner)]
extern crate rustyline;
pub use rustyline::{error::ReadlineError, Editor};
extern crate nom;

pub mod ast;
pub mod env;
pub mod eval;
pub mod lexer;
pub(crate) mod parser;
pub mod tokens;
pub(crate) mod utils;
use ast::*;
pub use env::MalEnv;
pub use eval::eval;
pub use lexer::MalLexer;
pub use nom::Finish;
pub use parser::parse_type as parse;
use tokens::*;
type Result<R> = std::result::Result<R, Box<dyn std::error::Error>>;

#[allow(unused_macros)]
macro_rules! binary_op {
    ($map: ident, $sym: literal, $op: tt) => {{
        $map.insert(
            MalSymbol::new($sym),
            MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
                let a1 = &args[0];
                let a2 = &args[1];
                if let (MalType::Int(i), MalType::Int(j)) = (a1, a2) {
                    return Ok(MalType::Int((i $op j)));
                }
                Err(format!("could not convert args to int64").into())
            }))),
        );
    }};
}

#[allow(unused_macros)]
macro_rules! binary_cmp {
    ($map: ident, $sym: literal, $op: tt) => {{
        $map.insert(
            MalSymbol::new($sym),
            MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
                let a1 = &args[0];
                let a2 = &args[1];
                if let (MalType::Int(i), MalType::Int(j)) = (a1, a2) {
                    return Ok(MalType::Bool((i $op j)));
                }
                Err(format!("could not convert args to int64").into())
            }))),
        );
    }};
}

pub fn print(mt: &MalType, print_readonly: bool) -> String {
    match mt {
        MalType::Str(s) => match print_readonly {
            true => format!("\"{}\"", utils::escape(s)),
            false => s.to_string(),
        },
        MalType::Nil => format!("nil"),
        MalType::Bool(b) => format!("{}", b),
        MalType::Int(i) => format!("{}", i),
        MalType::Float(fs) => format!("{}", fs),
        MalType::Keyword(k) => format!("{}", k),
        MalType::Symbol(s) => format!("{}", s),
        MalType::HashKey(k) => format!(":{}", k),
        MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
            let result = l
                .iter()
                .map(|mt| print(mt, print_readonly))
                .collect::<Vec<_>>()
                .join(" ");
            match mt {
                MalType::List(_) => format!("({})", result),
                MalType::Vector(_) => format!("[{}]", result),
                _ => unreachable!(),
            }
        }
        MalType::HashMap(MalHashMap(h)) => {
            let result = h
                .iter()
                .map(|(k, v)| format!("{} {}", print(k, print_readonly), print(v, print_readonly)))
                .collect::<Vec<_>>()
                .join(" ");
            format!("{{{}}}", result)
        }
        MalType::Quoted(q) => format!("({} {})", MalToken::Quote, print(q, print_readonly)),
        MalType::QuasiQuoted(q) => {
            format!("({} {})", MalToken::QuasiQuote, print(q, print_readonly))
        }
        MalType::Unquote(q) => format!("({} {})", MalToken::Unquote, print(q, print_readonly)),
        MalType::Deref(a) => format!("({} {})", MalToken::Deref, print(a, print_readonly)),
        MalType::WithMeta(a, b) => format!(
            "({} {} {})",
            MalToken::WithMeta,
            print(b, print_readonly),
            print(a, print_readonly)
        ),
        MalType::SpliceUnquote(s) => {
            format!("({} {})", MalToken::SpliceUnquote, print(s, print_readonly))
        }
        MalType::Func(_func) => format!("#<function>"),
    }
}

pub fn make_env() -> std::rc::Rc<MalEnv> {
    let env = MalEnv::new();
    binary_op!(env, "+", +);
    binary_op!(env, "-", -);
    binary_op!(env, "*", *);
    binary_op!(env, "/", /);
    env.insert(
        MalSymbol::new("pr-str"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, true))
                .collect::<Vec<_>>()
                .join("");
            Ok(MalType::Str(result.into()))
        }))),
    );
    env.insert(
        MalSymbol::new("str"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, false))
                .collect::<Vec<_>>()
                .join("");
            Ok(MalType::Str(result.into()))
        }))),
    );
    env.insert(
        MalSymbol::new("prn"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, true))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", result);
            Ok(MalType::Nil)
        }))),
    );
    env.insert(
        MalSymbol::new("println"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            let result = args
                .iter()
                .map(|arg| print(arg, false))
                .collect::<Vec<_>>()
                .join(" ");
            println!("{}", result);
            Ok(MalType::Nil)
        }))),
    );
    env.insert(
        MalSymbol::new("list"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            Ok(MalType::List(MalList::new(args)))
        }))),
    );
    env.insert(
        MalSymbol::new("list?"),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            if let MalType::List(_) = &args[0] {
                return Ok(MalType::Bool(true));
            }
            return Ok(MalType::Bool(false));
        }))),
    );
    env.insert(
        MalSymbol::new("empty?"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    if l.len() == 0 {
                        return Ok(MalType::Bool(true));
                    }
                    return Ok(MalType::Bool(false));
                }
                _ => Ok(MalType::Bool(false)),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("count"),
        MalType::Func(Box::new(MalFunc::from_closure(
            |args: Vec<MalType>| match &args[0] {
                MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                    Ok(MalType::Int(l.len() as i64))
                }
                _ => Ok(MalType::Int(0)),
            },
        ))),
    );
    env.insert(
        MalSymbol::new("="),
        MalType::Func(Box::new(MalFunc::from_closure(|args: Vec<MalType>| {
            Ok(MalType::Bool(args[0] == args[1]))
        }))),
    );
    binary_cmp!(env, "<", <);
    binary_cmp!(env, "<=", <=);
    binary_cmp!(env, ">", >);
    binary_cmp!(env, ">=", >=);
    return env;
}
