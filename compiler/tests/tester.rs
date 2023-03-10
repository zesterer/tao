macro_rules! test {
    ($x:ident) => {
        #[test]
        fn $x() {
            test_configs(stringify!($x))
        }
    };
}

test!(math);
test!(lists);
test!(records);

use std::fs;
use tao::{run, OptMode, Options, SrcId};

fn test_configs(name: &str) {
    fn test_config(name: &str, options: Options) {
        let path = format!("tests/{}.tao", name);
        let src = fs::read_to_string(&path).unwrap();
        let src_id = SrcId::from_path(&path);

        let mut input = String::new();
        let mut expected = String::new();

        #[derive(Debug)]
        enum State {
            Start,
            Input,
            Output,
        }

        let mut state = State::Start;
        for line in src.lines().chain(std::iter::once("# >>>> END")) {
            match &state {
                State::Start | State::Output => {
                    if line.trim() == "# >>>> INPUT" || line.trim() == "# >>>> END" {
                        if let State::Output = &state {
                            let mut output = Vec::new();
                            run(
                                input.clone(),
                                src_id,
                                options.clone(),
                                &mut output,
                                |src| fs::read_to_string(src.to_path()).ok(),
                                |parent, rel| {
                                    let mut path = parent.to_path();
                                    path.pop();
                                    path.push(rel);
                                    let path = path.canonicalize().ok()?;
                                    Some(SrcId::from_path(path))
                                },
                            );
                            let output = String::from_utf8(output).unwrap();
                            if output.trim() != expected.trim() {
                                panic!(
                                    "\n\n \
                                ========[ EXPECTED OUTPUT ]========\n\n\
                                {}\n \
                                ========[ FOUND OUTPUT ]========\n\n\
                                {}\n",
                                    expected, output
                                );
                            }

                            input.clear();
                            expected.clear();
                        }
                        state = State::Input;
                    } else {
                        expected += line;
                        expected += "\n";
                    }
                }
                State::Input => {
                    if line.trim() == "# >>>> OUTPUT" {
                        state = State::Output;
                    } else {
                        input += line;
                        input += "\n";
                    }
                }
            }
        }
    }

    let mut options = Options {
        debug: Vec::new(),
        opt: OptMode::None,
    };
    options.opt = OptMode::None;
    test_config(name, options.clone());
    options.opt = OptMode::Fast;
    test_config(name, options.clone());
}
