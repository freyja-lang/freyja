#!/bin/bash

# Build the Freyja Language Server
odin build . -out:freyja-lsp

if [ $? -eq 0 ]; then
    echo "Build successful: freyja-lsp"
else
    echo "Build failed"
    exit 1
fi