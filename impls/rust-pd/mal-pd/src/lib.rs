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
pub use env::MalEnv;
pub use eval::eval;
pub use lexer::MalLexer;
pub use nom::Finish;
pub use parser::parse_type as parse;

type Result<R> = std::result::Result<R, Box<dyn std::error::Error>>;
