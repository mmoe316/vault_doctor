# Vault Doctor 🩺

**Fix your slow, crashing, or bloated Obsidian Vault.**

## The Problem
Obsidian vaults grow over time. Large images, broken links, and orphaned attachments can slow down your experience or even crash the mobile app.

## The Solution
Vault Doctor is a free, open-source CLI tool that:
- 🧹 Identifies orphaned files (files not linked in any note)
- 🔗 Finds broken links
- 📊 Reports on large files wasting space

## Quick Start (CLI)

1. **Download** the latest binary from [Releases](https://github.com/mmoe316/vault_doctor/releases).
2. **Run** it inside your Obsidian Vault:
   ```bash
   ./vault_doctor
   ```
3. **Read** the generated report `00_Vault_Health_Report.md`.

## Development

### Prerequisites
- [Gleam](https://gleam.run/)
- Node.js (for the website)

### Running the CLI
```bash
gleam build --target javascript
node run_cli.js
```

### Running the Website
The website is a static Lustre application.
```bash
python3 -m http.server
# Open http://localhost:8000
```

## Support
This tool is free. If it saved your vault, consider [buying me a coffee](https://buymeacoffee.com/mmoore).