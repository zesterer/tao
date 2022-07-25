use wasm_bindgen::prelude::*;
use include_dir::{include_dir, Dir};
use tao::{compile, SrcId, Options};
use tao_vm::{Program, Env, exec};
use std::path::{PathBuf, Component};

static LIB_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../lib");

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn prompt(s: String) -> String;
    fn alert(s: String);
}

#[derive(Default)]
struct WebEnv;

impl Env for WebEnv {
    fn input(&mut self) -> String { prompt("Provide input to program".to_string()) }
    fn print(&mut self, s: String) {
        let win = web_sys::window().unwrap();
        let doc = win.document().unwrap();
        let output = doc.get_element_by_id("output").unwrap();
        let mut output_buf = output
            .text_content()
            .unwrap();
        output_buf += &s;
        output_buf += "\n";
        output.set_text_content(Some(&output_buf));
    }
}

#[wasm_bindgen]
pub fn tao_init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn run(src: &str) {
    let mut stderr = Vec::<u8>::new();
    let prog = compile(
        src.to_string(),
        SrcId::from_path("main.tao"),
        Options::default(),
        &mut stderr,
        |src_id| std::str::from_utf8(LIB_DIR
            .get_file(src_id.to_path())?
            .contents())
            .map(|s| s.to_string())
            .ok(),
        |parent, rel| {
            let mut path = parent.to_path();
            path.pop();
            path.push(rel);
            let mut new_path = PathBuf::new();
            for c in path.components() {
                match c {
                    Component::Prefix(_) | Component::RootDir | Component::CurDir => {},
                    Component::ParentDir => { new_path.pop(); },
                    Component::Normal(p) => new_path.push(p),
                }
            }
            Some(SrcId::from_path(new_path))
        },
    );

    let output = std::str::from_utf8(&strip_ansi_escapes::strip(stderr).unwrap())
        .map(|s| s.to_string())
        .unwrap_or_else(|e| format!("Demo error: {}", e));

    let mut env = WebEnv::default();

    env.print(output);

    if let Some(prog) = prog {
        env.print(format!("Compilation succeeded."));
        exec(&prog, &mut env);
    }
}
