extern crate mal;
use mal::eval;
use mal::make_env;
use mal::parse;
use mal::print;
use mal::tokens::MalTokens;
use mal::Editor;
use mal::Finish;
use mal::MalEnv;
use mal::MalLexer;
use mal::ReadlineError;
use std::rc::Rc;

fn rep(line: &str, env: &Rc<MalEnv>) -> Result<(), Box<dyn std::error::Error>> {
    let t = MalLexer::lex(line)?;
    let t = MalTokens(t.as_slice());
    match parse(t).finish() {
        Ok((_, ast)) => match eval(ast, env.clone()) {
            Ok(res) => println!("{}", print(&res, true)),
            Err(e) => {
                println!("Error in eval {}", e)
            }
        },
        Err(e) => {
            println!("unbalanced {:?}", e);
        }
    };
    Ok(())
}

const NOT_DEFINITION: &str = "(def! not (fn* (a) (if a false true)))";
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut rl = Editor::<()>::new();
    let env = make_env();
    rep(NOT_DEFINITION, &env)?;
    loop {
        let r = rl.readline("user> ");
        match r {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if !line.is_empty() {
                    rep(&line, &env)?;
                }
            }
            Err(ReadlineError::Interrupted) => {
                continue;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(e) => {
                println!("{}", e);
                break;
            }
        }
    }
    Ok(())
}
