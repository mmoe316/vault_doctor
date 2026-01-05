import lustre
import lustre/element.{text}
import lustre/element/html.{a, div, h1, p}
import lustre/attribute.{class, href}

// --- MODEL ---

pub type Model {
  Model
}

pub fn init(_flags) -> Model {
  Model
}

// --- UPDATE ---

pub type Msg {
  NoOp
}

pub fn update(model: Model, _msg: Msg) -> Model {
  model
}

// --- VIEW ---

pub fn view(_model: Model) -> element.Element(Msg) {
  div([class("container")], [
    h1([], [text("Vault Doctor")]),
    p([], [text("Is your Obsidian Vault slow, crashing, or bloated?")]),
    p([], [text("Diagnose and fix it in seconds.")]),
    
    div([class("cta-section")], [
      a([href("https://github.com/yourusername/vault_doctor/releases"), class("download-btn")], [text("Download CLI Tool")]),
      
      // The "Buy Me a Coffee" Button
      a([href("https://buymeacoffee.com/mmoore"), class("coffee-btn")], [
        text("☕ Buy me a coffee")
      ])
    ])
  ])
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)
  Nil
}
