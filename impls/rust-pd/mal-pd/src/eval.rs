use crate::{
    ast::{MalAtom, MalHashMap, MalList, MalType, MalVec},
    env::MalEnv,
    Result,
};
use std::rc::Rc;
pub fn eval_ast(mt: MalType, env: &Rc<MalEnv>) -> Result<MalType> {
    if let MalType::Atom(MalAtom::Symbol(s)) = mt {
        if let Some(env) = MalEnv::find(env, &s) {
            return Ok(env.as_ref().borrow().get(&s).unwrap().clone());
        }
        return Err(format!("symbol {} not found", s).into());
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
    Ok(mt)
}

pub fn eval(mt: MalType, env: &Rc<MalEnv>) -> Result<MalType> {
    if let MalType::List(l) = &mt {
        match l.0.len() {
            0 => return Ok(mt),
            _ => {
                if let MalType::Atom(MalAtom::Symbol(ref s)) = l.0[0] {
                    if s.strcmp("def!") {
                        let k = &l.0[1];
                        let v = eval(l.0[2].clone(), env)?;
                        env.set(k, v.clone())?;
                        return Ok(v);
                    }
                    if s.strcmp("let*") {
                        let new_env = MalEnv::detach(env);
                        let (a1, a2) = (l.0[1].clone(), l.0[2].clone());
                        match a1 {
                            MalType::List(MalList(l)) | MalType::Vector(MalVec(l)) => {
                                if l.len() & 1 != 0 {
                                    return Err("even amount of bindings expected"
                                        .to_string()
                                        .into());
                                }
                                for kv in l.chunks(2) {
                                    let (k, v) = (&kv[0], &kv[1]);
                                    let v = eval(v.clone(), &new_env)?;
                                    new_env.set(k, v)?;
                                }
                            }
                            _ => {
                                return Err("Expected list or vector".to_string().into());
                            }
                        }
                        return eval(a2, &new_env);
                    }
                }
                if let MalType::List(l) = eval_ast(mt, env)? {
                    let f = l.0[0].clone();
                    if let MalType::Func(f) = f {
                        return f(l.0[1..].to_vec());
                    } else {
                        return Err("expected function as first argument".to_string().into());
                    }
                }
                return Err("expected function and args in a list".to_string().into());
            }
        };
    }
    eval_ast(mt, env)
}
