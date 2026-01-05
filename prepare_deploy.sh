#!/bin/bash

# 1. Clean and Create docs folder
rm -rf docs
mkdir -p docs

# 2. Copy the Entry Point
cp index.html docs/index.html

# 3. Copy the Compiled Code
# We need to preserve the path structure because the imports are relative
# e.g. import ... from "./build/dev/..."
mkdir -p docs/build/dev/javascript
cp -r build/dev/javascript/* docs/build/dev/javascript/

# 4. (Optional) Add a .nojekyll file to tell GitHub not to ignore files starting with _ or .
touch docs/.nojekyll

echo "✅ Website ready in 'docs/' folder."
echo "👉 Commit this folder and set GitHub Pages source to '/docs'."
