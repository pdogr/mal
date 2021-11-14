use crate::tokens::MalToken;
use std::{
    borrow::Cow,
    fmt::{Display, Formatter},
};
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalType<'a> {
    List(MalList<'a>),
    Atom(MalAtom<'a>),
    Vector(MalVec<'a>),
    HashMap(MalHashMap<'a>),
    Quoted(Box<MalType<'a>>),
    QuasiQuoted(Box<MalType<'a>>),
    Unquote(Box<MalType<'a>>),
    Deref(MalAtom<'a>),
    WithMeta(Box<MalType<'a>>, Box<MalType<'a>>),
    SpliceUnquote(Box<MalType<'a>>),
}

impl<'a> Display for MalType<'a> {
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalAtom<'a> {
    Nil,
    Bool(bool),
    Literal(MalLiteral<'a>),
    Keyword(&'static str),
    Symbol(MalSymbol<'a>),
    HashKey(Cow<'a, str>),
}

impl<'a> Display for MalAtom<'a> {
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
pub enum MalLiteral<'a> {
    Int(i64),
    Float(f64),
    Str(Cow<'a, str>),
}

impl<'a> Display for MalLiteral<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            MalLiteral::Int(i) => write!(f, "{}", i),
            MalLiteral::Float(fs) => write!(f, "{}", fs),
            MalLiteral::Str(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl<'a> PartialEq for MalLiteral<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => *l0 == *r0,
            (Self::Float(l0), Self::Float(r0)) => *l0 == *r0,
            (Self::Str(l0), Self::Str(r0)) => *l0 == *r0,
            _ => false,
        }
    }
}
impl<'a> Eq for MalLiteral<'a> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalSymbol<'a> {
    ident: Cow<'a, str>,
}

impl<'a> Display for MalSymbol<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl<'a> MalSymbol<'a> {
    pub fn new<'b>(ident: &'b str) -> Self
    where
        'b: 'a,
    {
        Self {
            ident: Cow::Borrowed(ident),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalList<'a>(Vec<MalType<'a>>);
impl<'a> MalList<'a> {
    pub fn new(types: Vec<MalType<'a>>) -> Self {
        Self(types)
    }
}

impl<'a> Display for MalList<'a> {
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
pub struct MalVec<'a>(Vec<MalType<'a>>);
impl<'a> MalVec<'a> {
    pub fn new(types: Vec<MalType<'a>>) -> Self {
        Self(types)
    }
}

impl<'a> Display for MalVec<'a> {
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
pub struct MalHashMap<'a>(Vec<(MalAtom<'a>, MalType<'a>)>);

impl<'a> MalHashMap<'a> {
    pub fn new(v: Vec<(MalAtom<'a>, MalType<'a>)>) -> Self {
        Self(v)
    }
}

impl<'a> Display for MalHashMap<'a> {
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
