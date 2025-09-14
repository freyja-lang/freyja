#!/bin/bash

# Build the FEA demo
echo "Building FEA demo..."
odin build . -out:fea_demo

if [ $? -eq 0 ]; then
    echo "Build successful!"
    echo "Running demo..."
    ./fea_demo
else
    echo "Build failed!"
    exit 1
fi