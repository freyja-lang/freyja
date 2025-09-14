#!/bin/bash

# Simplified OpenBLAS vendoring script
# Downloads from GitHub releases

set -e

VENDOR_DIR="vendor/linalg"
OPENBLAS_VERSION="0.3.24"
BASE_URL="https://github.com/OpenMathLib/OpenBLAS/releases/download/v${OPENBLAS_VERSION}"

echo "==================================="
echo "Freyja OpenBLAS Vendor Setup"
echo "==================================="
echo ""
echo "This will download OpenBLAS ${OPENBLAS_VERSION} binaries."
echo "Note: Some platforms may need manual setup."
echo ""

# Create directories
mkdir -p $VENDOR_DIR/{linux-x64,darwin-x64,darwin-arm64,windows-x64,wasm32}

# Function to download with progress
download_file() {
    local url=$1
    local output=$2
    echo "Downloading: $url"
    echo "To: $output"
    if command -v curl &> /dev/null; then
        curl -L --progress-bar "$url" -o "$output" || echo "Failed to download $url"
    else
        wget --show-progress "$url" -O "$output" || echo "Failed to download $url"
    fi
    echo ""
}

# Linux x64 - Download and extract
echo "1. Setting up Linux x64..."
LINUX_FILE="OpenBLAS-${OPENBLAS_VERSION}-x64.tar.gz"
LINUX_URL="${BASE_URL}/${LINUX_FILE}"
TEMP_DIR=$(mktemp -d)

download_file "$LINUX_URL" "$TEMP_DIR/$LINUX_FILE"

if [ -f "$TEMP_DIR/$LINUX_FILE" ]; then
    echo "Extracting Linux x64 library..."
    cd "$TEMP_DIR"
    tar -xzf "$LINUX_FILE"
    if [ -f "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.a" ]; then
        cp "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.a" "$OLDPWD/$VENDOR_DIR/linux-x64/"
        echo "✓ Linux x64 library installed"
    else
        echo "✗ Could not find libopenblas.a in archive"
    fi
    cd "$OLDPWD"
fi
rm -rf "$TEMP_DIR"

# Windows x64 - Download and extract
echo "2. Setting up Windows x64..."
WIN_FILE="OpenBLAS-${OPENBLAS_VERSION}-x64.zip"
WIN_URL="${BASE_URL}/${WIN_FILE}"
TEMP_DIR=$(mktemp -d)

download_file "$WIN_URL" "$TEMP_DIR/$WIN_FILE"

if [ -f "$TEMP_DIR/$WIN_FILE" ]; then
    echo "Extracting Windows x64 library..."
    cd "$TEMP_DIR"
    unzip -q "$WIN_FILE"
    if [ -f "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.lib" ]; then
        cp "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.lib" "$OLDPWD/$VENDOR_DIR/windows-x64/openblas.lib"
        echo "✓ Windows x64 library installed"
    elif [ -f "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.a" ]; then
        # Sometimes the Windows package has .a files
        cp "OpenBLAS-${OPENBLAS_VERSION}-x64/lib/libopenblas.a" "$OLDPWD/$VENDOR_DIR/windows-x64/libopenblas.a"
        echo "✓ Windows x64 library installed (static)"
    else
        echo "✗ Could not find Windows library in archive"
    fi
    cd "$OLDPWD"
fi
rm -rf "$TEMP_DIR"

echo ""
echo "3. Darwin (macOS) platforms"
echo "Note: For macOS, we recommend using the system Accelerate framework"
echo "      or installing OpenBLAS via Homebrew:"
echo ""
echo "  brew install openblas"
echo "  cp /opt/homebrew/opt/openblas/lib/libopenblas.a vendor/linalg/darwin-arm64/ # Apple Silicon"
echo "  cp /usr/local/opt/openblas/lib/libopenblas.a vendor/linalg/darwin-x64/       # Intel"
echo ""

echo "4. WASM32"
echo "Note: WASM requires compilation with Emscripten. See documentation."
echo ""

# Create placeholder files with instructions
cat > "$VENDOR_DIR/darwin-x64/README.txt" << EOF
To add OpenBLAS for Intel Mac:

Option 1: Use Homebrew
  brew install openblas
  cp /usr/local/opt/openblas/lib/libopenblas.a ./libopenblas.a

Option 2: Use system Accelerate framework (recommended)
  The build system will detect and use Accelerate.framework automatically
EOF

cat > "$VENDOR_DIR/darwin-arm64/README.txt" << EOF
To add OpenBLAS for Apple Silicon Mac:

Option 1: Use Homebrew
  brew install openblas
  cp /opt/homebrew/opt/openblas/lib/libopenblas.a ./libopenblas.a

Option 2: Use system Accelerate framework (recommended)
  The build system will detect and use Accelerate.framework automatically
EOF

cat > "$VENDOR_DIR/wasm32/README.txt" << EOF
To build OpenBLAS for WebAssembly:

1. Install Emscripten
2. Clone OpenBLAS:
   git clone https://github.com/OpenMathLib/OpenBLAS.git
3. Build:
   cd OpenBLAS
   make CC=emcc FC=emfc AR=emar HOSTCC=gcc TARGET=GENERIC BINARY=32 ONLY_CBLAS=1 NO_SHARED=1
4. Copy the static library:
   cp libopenblas.a /path/to/freyja/vendor/linalg/wasm32/
EOF

echo "==================================="
echo "Setup Summary:"
echo "==================================="
echo ""
ls -la $VENDOR_DIR/*/