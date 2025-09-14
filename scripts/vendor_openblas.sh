#!/bin/bash

# OpenBLAS vendoring script for Freyja
# Downloads pre-compiled OpenBLAS libraries for multiple platforms

set -e

OPENBLAS_VERSION="0.3.24"
VENDOR_DIR="vendor/linalg"

echo "==================================="
echo "Freyja OpenBLAS Vendoring Script"
echo "Version: $OPENBLAS_VERSION"
echo "==================================="

# Function to download and extract OpenBLAS
download_openblas() {
    local platform=$1
    local url=$2
    local filename=$3
    local extract_file=$4
    local target_file=$5
    
    echo ""
    echo "Downloading OpenBLAS for $platform..."
    echo "URL: $url"
    
    local platform_dir="$VENDOR_DIR/$platform"
    local temp_dir="/tmp/openblas_${platform}_$$"
    
    mkdir -p "$temp_dir"
    cd "$temp_dir"
    
    # Download
    if command -v wget &> /dev/null; then
        wget -q --show-progress "$url" -O "$filename"
    else
        curl -L --progress-bar "$url" -o "$filename"
    fi
    
    # Extract based on file type
    case "$filename" in
        *.zip)
            unzip -q "$filename"
            ;;
        *.tar.gz|*.tgz)
            tar -xzf "$filename"
            ;;
        *.tar.xz)
            tar -xJf "$filename"
            ;;
    esac
    
    # Find and copy the library
    if [ -f "$extract_file" ]; then
        cp "$extract_file" "$OLDPWD/$platform_dir/$target_file"
        echo "✓ Installed to $platform_dir/$target_file"
    else
        echo "✗ Could not find $extract_file"
        echo "  Available files:"
        find . -name "*.a" -o -name "*.lib" | head -10
    fi
    
    cd "$OLDPWD"
    rm -rf "$temp_dir"
}

# Linux x86_64
echo "1. Linux x64"
download_openblas \
    "linux-x64" \
    "https://github.com/OpenMathLib/OpenBLAS/releases/download/v${OPENBLAS_VERSION}/OpenBLAS-${OPENBLAS_VERSION}-x64.tar.gz" \
    "OpenBLAS-${OPENBLAS_VERSION}-x64.tar.gz" \
    "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.a" \
    "libopenblas.a"

# macOS x86_64 (Intel)
echo "2. Darwin x64 (Intel Mac)"
# Note: We'll use conda-forge builds as they're reliable
download_openblas \
    "darwin-x64" \
    "https://anaconda.org/conda-forge/openblas/0.3.24/download/osx-64/libopenblas-0.3.24-openmp_h48a4ad5_0.tar.bz2" \
    "openblas-darwin-x64.tar.bz2" \
    "lib/libopenblas.a" \
    "libopenblas.a"

# macOS ARM64 (Apple Silicon)
echo "3. Darwin ARM64 (Apple Silicon)"
download_openblas \
    "darwin-arm64" \
    "https://anaconda.org/conda-forge/openblas/0.3.24/download/osx-arm64/libopenblas-0.3.24-openmp_hfe08b51_0.tar.bz2" \
    "openblas-darwin-arm64.tar.bz2" \
    "lib/libopenblas.a" \
    "libopenblas.a"

# Windows x64
echo "4. Windows x64"
download_openblas \
    "windows-x64" \
    "https://github.com/OpenMathLib/OpenBLAS/releases/download/v${OPENBLAS_VERSION}/OpenBLAS-${OPENBLAS_VERSION}-x64.zip" \
    "OpenBLAS-${OPENBLAS_VERSION}-x64.zip" \
    "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.lib" \
    "openblas.lib"

# WASM32 would need to be compiled from source
echo ""
echo "5. WASM32"
echo "Note: WASM32 build requires compilation from source with Emscripten"
echo "To build: "
echo "  git clone https://github.com/OpenMathLib/OpenBLAS.git"
echo "  cd OpenBLAS"
echo "  make CC=emcc FC=emfc AR=emar HOSTCC=gcc TARGET=GENERIC BINARY=32 ONLY_CBLAS=1"

echo ""
echo "==================================="
echo "Download complete!"
echo "==================================="
echo ""
echo "Library sizes:"
du -sh $VENDOR_DIR/*/* 2>/dev/null || true