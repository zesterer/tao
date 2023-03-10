use include_dir::{include_dir, Dir};
use rand::prelude::*;
use std::path::{Component, PathBuf};
use tao::{compile, OptMode, Options, SrcId};
use tao_vm::{exec, Env, Program};
use wasm_bindgen::prelude::*;

static LIB_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../lib");

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn prompt(s: String) -> String;
    fn alert(s: String);
}

struct WebEnv(rand::rngs::SmallRng);

impl Env for WebEnv {
    fn input(&mut self) -> String {
        prompt("Provide input to program".to_string())
    }
    fn print(&mut self, s: String) {
        let win = web_sys::window().unwrap();
        let doc = win.document().unwrap();
        let output = doc.get_element_by_id("output").unwrap();
        let mut output_buf = output.text_content().unwrap();
        output_buf += &s;
        output_buf += "\n";
        output.set_text_content(Some(&output_buf));
    }
    fn rand(&mut self, max: i64) -> i64 {
        self.0.gen_range(0..max)
    }
}

#[wasm_bindgen]
pub fn tao_init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn run(src: &str, mode: &str, optimisation: &str) {
    let mut stderr = Vec::<u8>::new();
    let debug = match mode {
        "tokens" | "ast" | "hir" | "call_graph" | "mir" | "bytecode" => vec![mode.to_string()],
        _ => Vec::new(),
    };
    let prog = compile(
        src.to_string(),
        SrcId::from_path("main.tao"),
        Options {
            debug: debug.clone(),
            opt: match optimisation {
                "fast" => OptMode::Fast,
                "size" => OptMode::Size,
                _ => OptMode::None,
            },
        },
        &mut stderr,
        |src_id| {
            std::str::from_utf8(LIB_DIR.get_file(src_id.to_path())?.contents())
                .map(|s| s.to_string())
                .ok()
        },
        |parent, rel| {
            let mut path = parent.to_path();
            path.pop();
            path.push(rel);
            let mut new_path = PathBuf::new();
            for c in path.components() {
                match c {
                    Component::Prefix(_) | Component::RootDir | Component::CurDir => {}
                    Component::ParentDir => {
                        new_path.pop();
                    }
                    Component::Normal(p) => new_path.push(p),
                }
            }
            Some(SrcId::from_path(new_path))
        },
    );

    let output = std::str::from_utf8(&strip_ansi_escapes::strip(stderr).unwrap())
        .map(|s| s.to_string())
        .unwrap_or_else(|e| format!("Demo error: {}", e));

    let mut env = WebEnv(rand::SeedableRng::seed_from_u64(42));

    env.print(output);

    if debug.is_empty() {
        if let Some(prog) = prog {
            env.print(format!("Compilation succeeded."));
            exec(&prog, &mut env);
        }
    }
}
