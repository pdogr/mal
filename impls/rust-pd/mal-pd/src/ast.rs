use crate::{tokens::MalToken, Result};
use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalType {
    List(MalList),
    Atom(MalAtom),
    Vector(MalVec),
    HashMap(MalHashMap),
    Quoted(Box<MalType>),
    QuasiQuoted(Box<MalType>),
    Unquote(Box<MalType>),
    Deref(MalAtom),
    WithMeta(Box<MalType>, Box<MalType>),
    SpliceUnquote(Box<MalType>),
    Func(fn(Vec<MalType>) -> Result<MalType>),
}

impl Display for MalType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MalType::List(l) => write!(f, "{}", l),
            MalType::Atom(a) => write!(f, "{}", a),
            MalType::Vector(v) => write!(f, "{}", v),
            MalType::HashMap(h) => write!(f, "{}", h),
            MalType::Quoted(q) => write!(f, "({} {})", MalToken::Quote, q),
            MalType::QuasiQuoted(q) => write!(f, "({} {})", MalToken::QuasiQuote, q),
            MalType::Unquote(q) => write!(f, "({} {})", MalToken::Unquote, q),
            MalType::Deref(a) => write!(f, "({} {})", MalToken::Deref, a),
            MalType::WithMeta(a, b) => write!(f, "({} {} {})", MalToken::WithMeta, b, a),
            MalType::SpliceUnquote(s) => write!(f, "({} {})", MalToken::SpliceUnquote, s),
            MalType::Func(_) => write!(f, "func "),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalAtom {
    Nil,
    Bool(bool),
    Literal(MalLiteral),
    Keyword(&'static str),
    Symbol(MalSymbol),
    HashKey(Rc<str>),
}

impl Display for MalAtom {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MalAtom::Nil => write!(f, "nil"),
            MalAtom::Bool(b) => write!(f, "{}", b),
            MalAtom::Literal(l) => write!(f, "{}", l),
            MalAtom::Keyword(k) => write!(f, "{}", k),
            MalAtom::Symbol(s) => write!(f, "{}", s),
            MalAtom::HashKey(k) => write!(f, ":{}", k),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MalLiteral {
    Int(i64),
    Float(f64),
    Str(Rc<str>),
}

impl Display for MalLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MalLiteral::Int(i) => write!(f, "{}", i),
            MalLiteral::Float(fs) => write!(f, "{}", fs),
            MalLiteral::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl PartialEq for MalLiteral {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => *l0 == *r0,
            (Self::Float(l0), Self::Float(r0)) => *l0 == *r0,
            (Self::Str(l0), Self::Str(r0)) => *l0 == *r0,
            _ => false,
        }
    }
}
impl Eq for MalLiteral {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MalSymbol {
    ident: Rc<str>,
}

impl Display for MalSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl MalSymbol {
    pub fn new(ident: &str) -> Self {
        Self {
            ident: ident.into(),
        }
    }

    pub fn strcmp(&self, o: &str) -> bool {
        self.ident.as_ref() == o
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalList(pub Vec<MalType>);
impl MalList {
    pub fn new(types: Vec<MalType>) -> Self {
        Self(types)
    }
}

impl Display for MalList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        match self.0.len() {
            0 => {}
            1 => {
                unsafe {
                    write!(f, "{}", self.0.get_unchecked(0))?;
                };
            }
            _ => {
                unsafe {
                    write!(f, "{}", self.0.get_unchecked(0))?;
                };
                for i in 1..self.0.len() {
                    unsafe {
                        write!(f, " {}", self.0.get_unchecked(i))?;
                    };
                }
            }
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalVec(pub Vec<MalType>);
impl MalVec {
    pub fn new(types: Vec<MalType>) -> Self {
        Self(types)
    }
}

impl Display for MalVec {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        match self.0.len() {
            0 => {}
            1 => {
                unsafe {
                    write!(f, "{}", self.0.get_unchecked(0))?;
                };
            }
            _ => {
                unsafe {
                    write!(f, "{}", self.0.get_unchecked(0))?;
                };
                for i in 1..self.0.len() {
                    unsafe {
                        write!(f, " {}", self.0.get_unchecked(i))?;
                    };
                }
            }
        }
        write!(f, "]")
    }
}

// MalAtom -> MalType, MalAtom => HashKey | Literal::Str
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalHashMap(pub Vec<(MalAtom, MalType)>);

impl MalHashMap {
    pub fn new(v: Vec<(MalAtom, MalType)>) -> Self {
        Self(v)
    }
}

impl Display for MalHashMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        match self.0.len() {
            0 => {}
            1 => {
                unsafe {
                    let (k, v) = self.0.get_unchecked(0);
                    write!(f, "{} {}", k, v)?;
                };
            }
            _ => {
                unsafe {
                    let (k, v) = self.0.get_unchecked(0);
                    write!(f, "{} {}", k, v)?;
                };
                for i in 1..self.0.len() {
                    unsafe {
                        let (k, v) = self.0.get_unchecked(i);
                        write!(f, " {} {}", k, v)?;
                    };
                }
            }
        }
        write!(f, "}}")
    }
}
