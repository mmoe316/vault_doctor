import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $simplifile from "../simplifile/simplifile.mjs";
import { Ok, toList } from "./gleam.mjs";

function get_all_files(dir) {
  let $ = $simplifile.read_directory(dir);
  if ($ instanceof Ok) {
    let contents = $[0];
    return $list.flat_map(
      contents,
      (item) => {
        let path = (dir + "/") + item;
        let $1 = $simplifile.is_directory(path);
        if ($1 instanceof Ok) {
          let $2 = $1[0];
          if ($2) {
            return get_all_files(path);
          } else {
            return toList([path]);
          }
        } else {
          return toList([]);
        }
      },
    );
  } else {
    return toList([]);
  }
}

export function main() {
  $io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  $io.println("Vault Doctor - v1.0.0");
  $io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
  $io.println("Scanning vault structure...");
  let vault_path = ".";
  let files = get_all_files(vault_path);
  let md_files = $list.filter(
    files,
    (f) => { return $string.ends_with(f, ".md"); },
  );
  let attachments = $list.filter(
    files,
    (f) => {
      return (!$string.ends_with(f, ".md") && !$string.contains(f, "/.git/")) && !$string.contains(
        f,
        "/node_modules/",
      );
    },
  );
  $io.println(("Found " + $int.to_string($list.length(md_files))) + " notes.");
  $io.println(
    ("Found " + $int.to_string($list.length(attachments))) + " attachments.",
  );
  $io.println("\nAnalyzing links (this is a mock implementation)...");
  let orphans = attachments;
  $io.println(
    ("Found " + $int.to_string($list.length(orphans))) + " potential orphans.",
  );
  $io.println("\n[!] This tool is free and open source.");
  return $io.println("[!] Visit https://mdmoore.net/vault-doctor for more.");
}
