use rustyline::Editor;
use std::{env, fs::File, io::Read};
use tao::{run_expr, run_module};

fn main() {
    if let Some(filename) = env::args().nth(1) {
        let mut src = String::new();
        File::open(&filename)
            .and_then(|mut file| file.read_to_string(&mut src))
            .unwrap_or_else(|err| panic!("Could not read file '{}': {:?}", filename, err));

        match run_module(&src) {
            Ok(val) => println!("{}", val),
            Err(errs) => errs
                .iter()
                .for_each(|err| print!("{}", err.in_source(&src))),
        };
    } else {
        let mut rl = Editor::<()>::new();

        loop {
            let line = rl.readline("\n>> ");
            match line {
                Ok(line) => {
                    rl.add_history_entry(&line);

                    match run_expr(&line) {
                        Ok((ty, val)) => println!("{} of {}", val, ty),
                        Err(errs) => errs
                            .iter()
                            .for_each(|err| print!("{}", err.in_source(&line))),
                    };
                }
                Err(_) => break,
            }
        }
    }
}
