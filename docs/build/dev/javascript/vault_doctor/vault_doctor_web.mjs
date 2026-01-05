import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import { class$, href } from "../lustre/lustre/attribute.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import { text } from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import { a, div, h1, p } from "../lustre/lustre/element/html.mjs";
import { Ok, toList, CustomType as $CustomType, makeError } from "./gleam.mjs";

const FILEPATH = "src/vault_doctor_web.gleam";

export class Model extends $CustomType {}
export const Model$Model = () => new Model();
export const Model$isModel = (value) => value instanceof Model;

export class NoOp extends $CustomType {}
export const Msg$NoOp = () => new NoOp();
export const Msg$isNoOp = (value) => value instanceof NoOp;

export function init(_) {
  return new Model();
}

export function update(model, _) {
  return model;
}

export function view(_) {
  return div(
    toList([class$("container")]),
    toList([
      h1(toList([]), toList([text("Vault Doctor")])),
      p(
        toList([]),
        toList([text("Is your Obsidian Vault slow, crashing, or bloated?")]),
      ),
      p(toList([]), toList([text("Diagnose and fix it in seconds.")])),
      div(
        toList([class$("cta-section")]),
        toList([
          a(
            toList([
              href("https://github.com/yourusername/vault_doctor/releases"),
              class$("download-btn"),
            ]),
            toList([text("Download CLI Tool")]),
          ),
          a(
            toList([
              href("https://buymeacoffee.com/mmoore"),
              class$("coffee-btn"),
            ]),
            toList([text("☕ Buy me a coffee")]),
          ),
        ]),
      ),
    ]),
  );
}

export function main() {
  let app = $lustre.simple(init, update, view);
  let $ = $lustre.start(app, "#app", undefined);
  if (!($ instanceof Ok)) {
    throw makeError(
      "let_assert",
      FILEPATH,
      "vault_doctor_web",
      47,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $, start: 989, end: 1038, pattern_start: 1000, pattern_end: 1005 }
    )
  }
  return undefined;
}
