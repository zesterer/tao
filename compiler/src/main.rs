use tao::{Options, SrcId, run};
use structopt::StructOpt;
use std::{fs, path::PathBuf};

#[derive(Clone, Debug, StructOpt)]
pub struct Args {
    #[structopt(flatten)]
    pub options: Options,
    /// Specify the file to run
    #[structopt(name = "FILE", parse(from_os_str))]
    pub file: PathBuf,
}

fn main() {
    let args = Args::from_args();
    let src = fs::read_to_string(&args.file)
        .expect("Failed to read file");
    let src_id = SrcId::from_path(args.file);
    run(src, src_id, args.options, std::io::stdout());
}
