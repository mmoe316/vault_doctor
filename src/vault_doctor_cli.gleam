import gleam/io
import simplifile
import gleam/list
import gleam/int
import gleam/string
import gleam/result

pub fn main() {
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("Vault Doctor - v1.0.0")
  io.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
  io.println("Scanning vault structure...")
  
  let vault_path = "."
  let files = get_all_files(vault_path)
  
  let md_files = list.filter(files, fn(f) { string.ends_with(f, ".md") })
  let attachments = list.filter(files, fn(f) { 
    !string.ends_with(f, ".md") && !string.contains(f, "/.git/") && !string.contains(f, "/node_modules/")
  })
  
  io.println("Found " <> int.to_string(list.length(md_files)) <> " notes.")
  io.println("Found " <> int.to_string(list.length(attachments)) <> " attachments.")
  
  io.println("\nAnalyzing links (this is a mock implementation)...")
  
  // TODO: Actual link parsing logic here
  // For now, we just list the attachments as potential orphans to demonstrate structure
  
  let orphans = attachments 
  
  io.println("Found " <> int.to_string(list.length(orphans)) <> " potential orphans.")
  
  io.println("\n[!] This tool is free and open source.")
  io.println("[!] Visit https://mdmoore.net/vault-doctor for more.")
}

fn get_all_files(dir: String) -> List(String) {
  case simplifile.read_directory(dir) {
    Ok(contents) -> {
      list.flat_map(contents, fn(item) {
        let path = dir <> "/" <> item
        case simplifile.is_directory(path) {
          Ok(True) -> get_all_files(path)
          Ok(False) -> [path]
          _ -> []
        }
      })
    }
    Error(_) -> []
  }
}