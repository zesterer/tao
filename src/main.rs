use rustyline::Editor;
use std::{env, fs, io::Read};
use tao::{eval_expr, run_module, Loader, Src, Ident, ErrorCode, Error, Span};

struct SimpleLoader(String);

impl Loader for SimpleLoader {
    fn load(&mut self, path: &[Ident]) -> Result<Src, Error> {
        if path.len() == 0 {
            Ok(Src {
                name: "repl".to_string(),
                code: self.0.clone(),
            })
        } else {
            Err(Error::new(ErrorCode::NoSuchSrc, Span::none(), format!("No such source: {:?}", path)))
        }
    }
}

fn main() {
    if let Some(filename) = env::args().nth(1) {
        let src = fs::read_to_string(&filename)
            .unwrap_or_else(|err| panic!("Could not read file '{}': {:?}", filename, err));

        run_module(SimpleLoader(src));
    } else {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline("\n>> ");
            match line {
                Ok(line) => {
                    rl.add_history_entry(&line);
                    eval_expr(SimpleLoader(line));
                }
                Err(_) => break,
            }
        }
    }
}
