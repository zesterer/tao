use tao::{Options, SrcId, compile};
use tao_vm::{exec, Env};
use structopt::StructOpt;
use std::{fs, path::PathBuf};
use rand::prelude::*;

#[derive(Clone, Debug, StructOpt)]
pub struct Args {
    #[structopt(flatten)]
    pub options: Options,
    /// Specify the file to run
    #[structopt(name = "FILE", parse(from_os_str))]
    pub file: PathBuf,
}

#[derive(Default)]
pub struct Stdio;

impl Env for Stdio {
    fn input(&mut self) -> String {
        use std::io::{stdin, stdout, Write};

        let mut s = String::new();
        print!("> ");
        stdout().flush().expect("IO error");
        stdin().read_line(&mut s).expect("IO error");
        s
    }

    fn print(&mut self, s: String) {
        println!("{}", s);
    }

    fn rand(&mut self, max: i64) -> i64 {
        thread_rng().gen_range(0..max)
    }
}

fn main() {
    let args = Args::from_args();
    let src = fs::read_to_string(&args.file)
        .expect("Failed to read file");
    let src_id = SrcId::from_path(args.file);
    let prog = compile(
        src,
        src_id,
        args.options,
        std::io::stdout(),
        |src| fs::read_to_string(src.to_path()).ok(),
        |parent, rel| {
            let mut path = parent.to_path();
            path.pop();
            path.push(rel);
            Some(SrcId::from_path(path.canonicalize().ok()?))
        },
    );

    if let Some(prog) = prog {
        exec(&prog, &mut Stdio::default());
    }
}
