#!/bin/bash

# Freyja vendoring setup - practical approach
# Uses system package managers where possible

set -e

VENDOR_DIR="vendor/linalg"

echo "==================================="
echo "Freyja Linear Algebra Setup"
echo "==================================="
echo ""

# Detect OS
OS="unknown"
ARCH=$(uname -m)

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    OS="linux"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    OS="darwin"
elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]]; then
    OS="windows"
fi

echo "Detected: $OS-$ARCH"
echo ""

# Create directories
mkdir -p $VENDOR_DIR/{linux-x64,darwin-x64,darwin-arm64,windows-x64}

case "$OS" in
    linux)
        echo "Setting up for Linux..."
        
        # Check if OpenBLAS is installed
        if pkg-config --exists openblas 2>/dev/null; then
            echo "OpenBLAS found via pkg-config"
            OPENBLAS_LIB=$(pkg-config --variable=libdir openblas)
            echo "Library path: $OPENBLAS_LIB"
            
            # Try to find the static library
            if [ -f "$OPENBLAS_LIB/libopenblas.a" ]; then
                echo "Copying static library..."
                cp "$OPENBLAS_LIB/libopenblas.a" "$VENDOR_DIR/linux-x64/"
                echo "✓ Copied to vendor/linalg/linux-x64/"
            else
                echo "Static library not found. You may need to:"
                echo "  sudo apt-get install libopenblas-dev  # Debian/Ubuntu"
                echo "  sudo yum install openblas-devel        # RHEL/CentOS"
                echo "  sudo pacman -S openblas                # Arch"
            fi
        else
            echo "OpenBLAS not found. Installing..."
            
            # Try to install based on distro
            if command -v apt-get &> /dev/null; then
                echo "Using apt-get..."
                sudo apt-get update
                sudo apt-get install -y libopenblas-dev
                
                # Find and copy the library
                OPENBLAS_LIB="/usr/lib/x86_64-linux-gnu/libopenblas.a"
                if [ -f "$OPENBLAS_LIB" ]; then
                    cp "$OPENBLAS_LIB" "$VENDOR_DIR/linux-x64/"
                    echo "✓ Installed and copied to vendor/linalg/linux-x64/"
                fi
            elif command -v yum &> /dev/null; then
                echo "Using yum..."
                sudo yum install -y openblas-devel
            elif command -v pacman &> /dev/null; then
                echo "Using pacman..."
                sudo pacman -S --noconfirm openblas
            else
                echo "Could not detect package manager."
                echo "Please install OpenBLAS manually."
            fi
        fi
        ;;
        
    darwin)
        echo "Setting up for macOS..."
        
        if [[ "$ARCH" == "arm64" ]]; then
            PLATFORM="darwin-arm64"
            BREW_PREFIX="/opt/homebrew"
        else
            PLATFORM="darwin-x64"
            BREW_PREFIX="/usr/local"
        fi
        
        # Check for Homebrew OpenBLAS
        if [ -f "$BREW_PREFIX/opt/openblas/lib/libopenblas.a" ]; then
            echo "Found OpenBLAS via Homebrew"
            cp "$BREW_PREFIX/opt/openblas/lib/libopenblas.a" "$VENDOR_DIR/$PLATFORM/"
            echo "✓ Copied to vendor/linalg/$PLATFORM/"
        else
            echo "OpenBLAS not found. Options:"
            echo "1. Install via Homebrew (recommended):"
            echo "   brew install openblas"
            echo ""
            echo "2. Use Apple's Accelerate framework (built-in):"
            echo "   No installation needed - we'll detect it at build time"
            
            # Create a marker file for Accelerate
            echo "ACCELERATE" > "$VENDOR_DIR/$PLATFORM/USE_ACCELERATE"
        fi
        ;;
        
    windows)
        echo "Setting up for Windows..."
        echo "Downloading pre-built OpenBLAS..."
        
        # Download Windows binaries
        WIN_URL="https://github.com/OpenMathLib/OpenBLAS/releases/download/v0.3.30/OpenBLAS-0.3.30-x64.zip"
        TEMP_DIR=$(mktemp -d)
        
        echo "Downloading from: $WIN_URL"
        curl -L --progress-bar "$WIN_URL" -o "$TEMP_DIR/openblas.zip"
        
        cd "$TEMP_DIR"
        unzip -q openblas.zip
        
        # Find the library files
        find . -name "*.lib" -o -name "*.a" | while read lib; do
            echo "Found: $lib"
            cp "$lib" "$OLDPWD/$VENDOR_DIR/windows-x64/"
        done
        
        cd "$OLDPWD"
        rm -rf "$TEMP_DIR"
        ;;
        
    *)
        echo "Unknown OS: $OS"
        echo "Please set up OpenBLAS manually."
        ;;
esac

echo ""
echo "==================================="
echo "Setup Complete"
echo "==================================="
echo ""
echo "Vendor directory contents:"
ls -la $VENDOR_DIR/*/ 2>/dev/null || echo "No libraries installed yet"

echo ""
echo "To use in your Freyja build:"
echo "  freyja build --blas=vendor"
echo ""