extern crate rustyline;
pub use rustyline::error::ReadlineError;
pub use rustyline::Editor;
extern crate nom;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
