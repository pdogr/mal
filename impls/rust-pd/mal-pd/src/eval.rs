use crate::{
    ast::{MalAtom, MalHashMap, MalList, MalType, MalVec},
    env::MalEnv,
    Result,
};
pub fn eval_ast<'a>(mt: MalType<'a>, env: &'a MalEnv<'a>) -> Result<MalType<'a>> {
    if let MalType::Atom(MalAtom::Symbol(ref s)) = mt {
        if let Some(mt) = env.get(&s) {
            return Ok(mt.clone());
        } else {
            return Err("Symbol not found in env".into());
        };
    } else if let MalType::List(l) = mt {
        let mut v: Vec<MalType> = Vec::new();
        for item in l.0 {
            let res = eval(item, env)?;
            v.push(res);
        }
        return Ok(MalType::List(MalList::new(v)));
    } else if let MalType::Vector(l) = mt {
        let mut v: Vec<MalType> = Vec::new();
        for item in l.0 {
            let res = eval(item, env)?;
            v.push(res);
        }
        return Ok(MalType::Vector(MalVec::new(v)));
    } else if let MalType::HashMap(h) = mt {
        let mut hm: Vec<(MalAtom, MalType)> = Vec::new();
        for (k, v) in h.0 {
            let v = eval(v, env)?;
            hm.push((k, v))
        }
        return Ok(MalType::HashMap(MalHashMap::new(hm)));
    }
    return Ok(mt);
}

pub fn eval<'a>(mt: MalType<'a>, env: &'a MalEnv<'a>) -> Result<MalType<'a>> {
    if let MalType::List(l) = &mt {
        match l.0.len() {
            0 => return Ok(mt),
            _ => {
                if let MalType::List(l) = eval_ast(mt, env)? {
                    let f = l.0[0].clone();
                    if let MalType::Func(f) = f {
                        return f(l.0[1..].to_vec());
                    } else {
                        return Err(format!("expected function as first argument").into());
                    }
                }
                return Err(format!("expected function and args in a list").into());
            }
        };
    }
    eval_ast(mt, env)
}
