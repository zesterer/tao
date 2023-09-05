use js_sys::Date;
use yew::{html, Component, Context, Html, InputEvent};
use web_sys::HtmlTextAreaElement;
use wasm_bindgen::{JsCast, UnwrapThrowExt};

macro_rules! log { ($($arg:tt)*) => { gloo::console::log!(format!($($arg)*)) }; }

// Define the possible messages which can be sent to the component
pub enum Msg {
    SetText(String),
    Build,
}

pub struct App {
    text: String,
}

impl Component for App {
    type Message = Msg;
    type Properties = ();

    fn create(_ctx: &Context<Self>) -> Self {
        Self { text: String::new() }
    }

    fn update(&mut self, _ctx: &Context<Self>, msg: Self::Message) -> bool {
        match msg {
            Msg::SetText(text) => {
                self.text = text;
                true
            },
            Msg::Build => {
                log!("Building {}!", &self.text);
                true
            },
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
            <div>
                <div class="panel">
                    // A button to send the Increment message
                    <button class="button" onclick={ctx.link().callback(|_| Msg::Build)}>
                        { "build" }
                    </button>

                </div>

                <textarea oninput={ctx.link().callback(|e: InputEvent| {
                    let tgt = e.target().unwrap_throw();
                    let tgt = tgt.dyn_into::<HtmlTextAreaElement>().unwrap_throw();
                    Msg::SetText(tgt.value().into())
                })} rows="5" cols="60" name="text" placeholder="Enter text"></textarea>

                // Display the current value of the counter
                <p class="counter">
                    { &self.text }
                </p>

                // Display the current date and time the page was rendered
                <p class="footer">
                    { "Rendered: " }
                    { String::from(Date::new_0().to_string()) }
                </p>
            </div>
        }
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
