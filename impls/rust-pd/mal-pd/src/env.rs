use crate::{
    ast::{MalAtom, MalLiteral, MalSymbol, MalType},
    Result,
};
use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MalEnv {
    map: RefCell<HashMap<MalSymbol, MalType>>,
    outer: Option<Rc<MalEnv>>,
}

impl MalEnv {
    pub fn new() -> Rc<Self> {
        let mut map = HashMap::new();
        map.insert(
            MalSymbol::new("+"),
            MalType::Func(|args: Vec<MalType>| binary_func!(args,+)),
        );
        map.insert(
            MalSymbol::new("-"),
            MalType::Func(|args: Vec<MalType>| binary_func!(args,-)),
        );
        map.insert(
            MalSymbol::new("*"),
            MalType::Func(|args: Vec<MalType>| binary_func!(args,*)),
        );
        map.insert(
            MalSymbol::new("/"),
            MalType::Func(|args: Vec<MalType>| binary_func!(args,/)),
        );
        Rc::new(Self {
            map: RefCell::new(map),
            outer: None,
        })
    }
    pub fn detach(outer: &Rc<MalEnv>) -> Rc<Self> {
        Rc::new(Self {
            map: RefCell::new(HashMap::new()),
            outer: Some(outer.clone()),
        })
    }

    pub fn borrow(&self) -> Ref<'_, HashMap<MalSymbol, MalType>> {
        Ref::map(self.map.borrow(), |m| m)
    }

    pub fn has(&self, ms: &MalSymbol) -> bool {
        self.map.borrow().contains_key(ms)
    }

    pub fn find(env: &Rc<MalEnv>, ms: &MalSymbol) -> Option<Rc<MalEnv>> {
        if env.has(ms) {
            return Some(env.clone());
        }
        if env.outer.is_some() {
            return MalEnv::find(env.outer.as_ref().unwrap(), ms);
        }
        None
    }

    pub fn set(&self, k: &MalType, v: MalType) -> Result<()> {
        if let MalType::Atom(MalAtom::Symbol(ms)) = k {
            self.map.borrow_mut().insert(ms.clone(), v);
            return Ok(());
        }
        Err(format!("unable to set on non symbol, found {}", k).into())
    }
}
