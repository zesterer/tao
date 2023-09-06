use std::path::{self, PathBuf};
use yew::{html, Component, Context, Html, InputEvent, Event};
use yew_ansi::Ansi;
use web_sys::{HtmlTextAreaElement, HtmlSelectElement, HtmlElement, HtmlIFrameElement, window};
use wasm_bindgen::{JsCast, UnwrapThrowExt};
use include_dir::{include_dir, Dir};
use rand::prelude::*;
use tao::{compile, SrcId, Options, OptMode};
use tao_vm::{Env, exec};

const TEXT_CODE_PLACEHOLDER: &str = "\
Write code here, or choose an example from the menu above.

Click 'Run' to compile and run the code.

'Mode' allows switching between various compiler output modes.

'Optimization' changes the optimization level and strategy of the compiler.
";
const TEXT_PANEL: &str = "Tao is a functional programming language with generalised algebraic effects and typeclasses";
const URL_GITHUB: &str = "https://www.github.com/zesterer/tao";

static LIB_DIR: Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/../lib");

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

macro_rules! log { ($($arg:tt)*) => { gloo::console::log!(format!($($arg)*)) }; }

// Define the possible messages which can be sent to the component
pub enum Msg {
    SetSrc(String),
    SwitchExample(usize),
    SetMode(String),
    SetOpt(String),
    Run,
}

pub struct App {
    src: String,
    mode: String,
    opt: String,
    output: String,
    examples: Vec<(&'static str, &'static str)>,
    rng: rand::rngs::SmallRng,
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self {
            src: String::new(),
            mode: "".to_string(),
            opt: "none".to_string(),
            output: String::new(),
            examples: vec![
                ("hello", include_str!("../../examples/hello.tao")),
                ("input", include_str!("../../examples/input.tao")),
                ("mutate", include_str!("../../examples/mutate.tao")),
                ("polymorphic_effects", include_str!("../../examples/polymorphic_effects.tao")),
                ("adventure", include_str!("../../examples/adventure.tao")),
                ("brainfuck", include_str!("../../examples/brainfuck.tao")),
                ("calc", include_str!("../../examples/calc.tao")),
                ("quickcheck", include_str!("../../examples/quickcheck.tao")),
            ],
            rng: rand::SeedableRng::seed_from_u64(42),
        }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::SetSrc(src) => {
                self.src = src;
                true
            },
            Msg::SwitchExample(idx) => {
                let (name, src) = self.examples.get(idx).unwrap();
                self.src = src.to_string();

                window()
                    .unwrap_throw()
                    .document()
                    .unwrap_throw()
                    .get_element_by_id("editor")
                    .unwrap_throw()
                    .dyn_into::<HtmlTextAreaElement>()
                    .unwrap_throw()
                    .set_value(&self.src);

                log!("Set example to {name}.");
                true
            },
            Msg::SetMode(mode) => {
                self.mode = mode;
                false
            },
            Msg::SetOpt(opt) => {
                self.opt = opt;
                false
            },
            Msg::Run => {
                log!("Running...");
                let compiled = self.run();

                let document = window()
                    .expect_throw("failed to get window")
                    .document()
                    .expect_throw("failed to get document");

                let is_graph = compiled && self.mode == "call_graph";

                if is_graph {
                    let url = format!("https://dreampuf.github.io/GraphvizOnline/#{}", urlencoding::encode(&self.output));
                    // Switch out the iframe
                    let iframe_holder = document
                        .get_elements_by_class_name("iframe-holder")
                        .item(0)
                        .expect_throw("failed to get iframe-holder");
                    iframe_holder.set_inner_html(&format!("<iframe class=\"split call_graph\" src=\"{url}\"/>"));
                }

                // Toggle between graph and text output
                for (class, set) in [("call_graph", false), ("output", true)] {
                    document
                        .get_elements_by_class_name(class)
                        .item(0)
                        .expect_throw("failed to get element")
                        .dyn_into::<HtmlElement>()
                        .expect_throw("failed to get downcast element")
                        .style()
                        .set_property("display", if is_graph ^ set { "block" } else { "none" })
                        .expect_throw("failed to set display property");
                }

                true
            },
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <div>
                <div class="panel">
                    <button class="run" id="run_button" onclick={ctx.link().callback(|_| Msg::Run)}>
                        { "Run" }
                    </button>

                    <span>{ "Example:" }</span>
                    <select name="examples" required=false onchange={ctx.link().callback(|e: Event| {
                        let tgt = e.target().unwrap_throw().dyn_into::<HtmlSelectElement>().unwrap_throw();
                        Msg::SwitchExample(tgt.selected_index() as usize)
                    })} id="examples">
                        { for self.examples.iter().enumerate().map(|(idx, (e, _))| html! {
                            <option value = { idx.to_string() }>{ e }</option>
                        }) }
                    </select>
                    <span>{ "Mode:" }</span>
                    <select name="mode" id="mode" onchange={ctx.link().callback(|e: Event| {
                        let tgt = e.target().unwrap_throw().dyn_into::<HtmlSelectElement>().unwrap_throw();
                        Msg::SetMode(tgt.value())
                    })}>
                        <option value = "exec" selected=true>{ "Execute" }</option>
                        <option value = "ast">{ "AST" }</option>
                        <option value = "hir">{ "HIR" }</option>
                        <option value = "call_graph">{ "Call Graph" }</option>
                        <option value = "mir">{ "MIR" }</option>
                        <option value = "bytecode">{ "Bytecode" }</option>
                    </select>
                    <span>{ "Optimization:" }</span>
                    <select name="optimization" id="optimization" onchange={ctx.link().callback(|e: Event| {
                        let tgt = e.target().unwrap_throw().dyn_into::<HtmlSelectElement>().unwrap_throw();
                        Msg::SetOpt(tgt.value())
                    })}>
                        <option value = "none" selected=true>{ "None" }</option>
                        <option value = "fast">{ "Fast" }</option>
                        <option value = "size">{ "Size" }</option>
                    </select>

                    <span>{ TEXT_PANEL }<a href={ URL_GITHUB }>{ "Github" }<i class="fa fa-github"></i></a></span>
                </div>

                <div class="splitter">
                    <aside class="left">
                        // Input
                        <textarea class="split" id="editor" autofocus=true oninput={ctx.link().callback(|e: InputEvent| {
                            let tgt = e.target().unwrap_throw();
                            let tgt = tgt.dyn_into::<HtmlTextAreaElement>().unwrap_throw();
                            Msg::SetSrc(tgt.value().into())
                        })} rows="5" cols="60" name="text" placeholder={TEXT_CODE_PLACEHOLDER}>{ &self.output }</textarea>
                    </aside>
                    <aside class="right">
                        // Output
                        <Ansi class="split output" text={ self.output.clone() }/>
                        <div class="iframe-holder"></div>
                    </aside>
                </div>
            </div>
        }
    }
}

impl App {
    pub fn run(&mut self) -> bool {
        self.output.clear();

        let mut stderr = Vec::<u8>::new();

        let debug = match self.mode.as_str() {
            m @ ("tokens" | "ast" | "hir" | "call_graph" | "mir" | "bytecode") => vec![m.to_string()],
            _ => Vec::new(),
        };

        let demo_src_id = SrcId::from_path("<demo>");

        let prog = compile(
            self.src.to_string(),
            demo_src_id,
            Options {
                debug: debug.clone(),
                opt: match self.opt.as_str() {
                    "fast" => OptMode::Fast,
                    "size" => OptMode::Size,
                    _ => OptMode::None,
                },
            },
            &mut stderr,
            |src_id| std::str::from_utf8(LIB_DIR
                .get_file(src_id.to_path())?
                .contents())
                .map(|s| s.to_string())
                .ok(),
            |parent, rel| {
                let rel = PathBuf::from(rel);

                // Special-case the demo src because we're piggy-backing off the imports from the repo examples
                let rel = if parent == demo_src_id {
                    rel.strip_prefix("../lib").ok()?.to_path_buf()
                } else {
                    rel
                };

                let mut new_path = parent.to_path();
                new_path.pop();

                for c in rel.components() {
                    match c {
                        path::Component::Prefix(_) | path::Component::RootDir | path::Component::CurDir => {},
                        path::Component::ParentDir => { new_path.pop(); },
                        path::Component::Normal(p) => new_path.push(p),
                    }
                }

                Some(SrcId::from_path(new_path))
            },
        );

        let output = std::str::from_utf8(&stderr)
            .map(|s| s.to_string())
            .unwrap_or_else(|e| format!("Demo error: {}", e));

        self.print(output);

        if debug.is_empty() {
            if let Some(prog) = &prog {
                exec(prog, self);
            }
        }

        prog.is_some()
    }
}

impl Env for App {
    fn input(&mut self) -> String {
        window()
            .unwrap_throw()
            .prompt_with_message("Provide input to program")
            .unwrap_or_default()
            .unwrap_or_default()
    }
    fn print(&mut self, s: String) {
        self.output += &s;
        self.output += "\n";
    }
    fn rand(&mut self, max: i64) -> i64 { self.rng.gen_range(0..max) }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
