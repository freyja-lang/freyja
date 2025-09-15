#!/bin/bash

set -e

echo "Building Freyja LSP and VSCode Extension..."

# Build the LSP server
echo "Building LSP server..."
cd server
odin build . -out:freyja-lsp
echo "✓ LSP server built: freyja-lsp"
cd ..

# Build the VSCode extension
echo "Building VSCode extension..."
cd vscode

# Install dependencies if needed
if [ ! -d "node_modules" ]; then
    echo "Installing npm dependencies..."
    npm install
fi

# Compile TypeScript
echo "Compiling TypeScript..."
npm run compile

echo "✓ VSCode extension compiled"

# Optional: Package the extension
if command -v vsce &> /dev/null; then
    echo "Packaging extension..."
    vsce package --no-dependencies
    echo "✓ Extension packaged as .vsix"
else
    echo "Note: Install vsce to package the extension: npm install -g vsce"
fi

cd ..

echo ""
echo "Build complete!"
echo "To use the extension:"
echo "1. Open the vscode folder in VSCode"
echo "2. Press F5 to test in a new VSCode window"
echo "3. Or install the .vsix file via Extensions > ... > Install from VSIX"