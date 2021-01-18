use rustyline::Editor;
use std::{env, fs, io::Read, path::PathBuf};
use tao::{eval_expr, run_module, Loader, Src, Ident, ErrorCode, Error, Span};

struct SimpleLoader(String, String);

impl Loader for SimpleLoader {
    fn load(&mut self, path: &[Ident]) -> Result<Src, Error> {
        if path.len() == 0 {
            Ok(Src {
                name: self.1.clone(),
                code: self.0.clone(),
            })
        } else {
            Err(Error::new(ErrorCode::NoSuchSrc, Span::none(), format!("No such source: {:?}", path)))
        }
    }
}

fn main() {
    if let Some(filename) = env::args().nth(1) {
        let path = PathBuf::from(filename.to_string());
        let src = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("Could not read file '{}': {:?}", filename, err));

        run_module(SimpleLoader(src, path.file_name().unwrap_or_default().to_os_string().into_string().unwrap()));
    } else {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline("\n>> ");
            match line {
                Ok(line) => {
                    rl.add_history_entry(&line);
                    eval_expr(SimpleLoader(line, "repl".to_string()));
                }
                Err(_) => break,
            }
        }
    }
}
