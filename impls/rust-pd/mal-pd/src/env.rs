use crate::ast::{MalAtom, MalLiteral, MalSymbol, MalType};
use std::collections::HashMap;

#[allow(unused_macros)]
macro_rules! binary_func {
    ($a: ident, $op: tt) => { {
        let a1 = &$a[0];
        let a2 = &$a[1];

            if let (
                MalType::Atom(MalAtom::Literal(MalLiteral::Int(i))),
                MalType::Atom(MalAtom::Literal(MalLiteral::Int(j))),
            ) = (a1, a2)
            {
                return Ok(MalType::Atom(MalAtom::Literal(MalLiteral::Int(i $op j))));
            }
            Err(format!("could not use op").into())
    }
};
}

pub struct MalEnv<'a>(HashMap<MalSymbol<'a>, MalType<'a>>);

impl<'a> MalEnv<'a> {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(
            MalSymbol::new("+"),
            MalType::Func(|args: Vec<MalType<'a>>| binary_func!(args,+)),
        );
        map.insert(
            MalSymbol::new("-"),
            MalType::Func(|args: Vec<MalType<'a>>| binary_func!(args,-)),
        );
        map.insert(
            MalSymbol::new("*"),
            MalType::Func(|args: Vec<MalType<'a>>| binary_func!(args,*)),
        );
        map.insert(
            MalSymbol::new("/"),
            MalType::Func(|args: Vec<MalType<'a>>| binary_func!(args,/)),
        );
        Self(map)
    }
    pub fn get(&self, ms: &MalSymbol<'a>) -> Option<&MalType<'a>> {
        self.0.get(ms)
    }
}
