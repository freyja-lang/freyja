package freyja

import "core:os"
import "core:fmt"
import "core:strings"
import "core:path/filepath"

Build_Config :: struct {
    blas_backend: Blas_Backend,
    link_mode:    Link_Mode,
    output_dir:   string,
    verbose:      bool,
}

Blas_Backend :: enum {
    Auto,      // Detect best available
    Vendor,    // Use vendored library
    System,    // Use system-installed library
    Accelerate,// macOS Accelerate framework
    None,      // No BLAS (compile will fail if needed)
}

Link_Mode :: enum {
    Static,
    Dynamic,
}

Platform :: struct {
    os:   string,
    arch: string,
}

// Detect current platform
detect_platform :: proc() -> Platform {
    platform: Platform
    
    when ODIN_OS == .Linux {
        platform.os = "linux"
    } else when ODIN_OS == .Darwin {
        platform.os = "darwin"
    } else when ODIN_OS == .Windows {
        platform.os = "windows"
    }
    
    when ODIN_ARCH == .amd64 {
        platform.arch = "x64"
    } else when ODIN_ARCH == .arm64 {
        platform.arch = "arm64"
    }
    
    return platform
}

// Get platform-specific vendor directory
get_vendor_dir :: proc(platform: Platform) -> string {
    return fmt.tprintf("vendor/linalg/%s-%s", platform.os, platform.arch)
}

// Find BLAS library
find_blas_library :: proc(config: Build_Config) -> (path: string, ok: bool) {
    platform := detect_platform()
    
    switch config.blas_backend {
    case .Vendor:
        return find_vendored_blas(platform)
    case .System:
        return find_system_blas(platform)
    case .Accelerate:
        if platform.os == "darwin" {
            return "-framework Accelerate", true
        }
        return "", false
    case .Auto:
        // Try vendor first, then system
        if path, ok := find_vendored_blas(platform); ok {
            return path, true
        }
        if path, ok := find_system_blas(platform); ok {
            return path, true
        }
        // macOS: try Accelerate
        if platform.os == "darwin" {
            return "-framework Accelerate", true
        }
    case .None:
        return "", false
    }
    
    return "", false
}

// Find vendored BLAS library
find_vendored_blas :: proc(platform: Platform) -> (path: string, ok: bool) {
    vendor_dir := get_vendor_dir(platform)
    
    // Check for various library names
    lib_names := []string{
        "libopenblas.a",
        "openblas.lib",
        "libopenblas.lib",
    }
    
    for name in lib_names {
        lib_path := filepath.join([]string{vendor_dir, name})
        if os.exists(lib_path) {
            if verbose_logging {
                fmt.printf("Found vendored BLAS: %s\n", lib_path)
            }
            return lib_path, true
        }
    }
    
    // Check for USE_ACCELERATE marker (macOS)
    if platform.os == "darwin" {
        marker := filepath.join([]string{vendor_dir, "USE_ACCELERATE"})
        if os.exists(marker) {
            return "-framework Accelerate", true
        }
    }
    
    return "", false
}

// Find system BLAS library
find_system_blas :: proc(platform: Platform) -> (path: string, ok: bool) {
    // Linux: Check standard locations
    if platform.os == "linux" {
        search_paths := []string{
            "/usr/lib/x86_64-linux-gnu/libopenblas.a",
            "/usr/lib/x86_64-linux-gnu/libopenblas.so",
            "/usr/lib64/libopenblas.a",
            "/usr/lib64/libopenblas.so",
            "/usr/lib/libopenblas.a",
            "/usr/lib/libopenblas.so",
        }
        
        for path in search_paths {
            if os.exists(path) {
                return path, true
            }
        }
        
        // Try pkg-config
        // Note: In real implementation, would shell out to pkg-config
        return "", false
    }
    
    // macOS: Check Homebrew locations
    if platform.os == "darwin" {
        homebrew_paths := []string{
            "/opt/homebrew/opt/openblas/lib/libopenblas.a",  // ARM64
            "/usr/local/opt/openblas/lib/libopenblas.a",     // x64
        }
        
        for path in homebrew_paths {
            if os.exists(path) {
                return path, true
            }
        }
    }
    
    // Windows: Check common locations
    if platform.os == "windows" {
        // TODO: Add Windows search paths
    }
    
    return "", false
}

// Generate linker flags for BLAS
get_blas_link_flags :: proc(config: Build_Config) -> string {
    blas_lib, ok := find_blas_library(config)
    if !ok {
        if config.blas_backend != .None {
            fmt.eprintln("Warning: BLAS library not found")
        }
        return ""
    }
    
    // Handle framework flag (macOS Accelerate)
    if strings.has_prefix(blas_lib, "-framework") {
        return blas_lib
    }
    
    // Regular library path
    platform := detect_platform()
    if platform.os == "windows" {
        return fmt.tprintf("/DEFAULTLIB:%s", blas_lib)
    } else {
        // For static libraries, just return the path
        if strings.has_suffix(blas_lib, ".a") {
            return blas_lib
        }
        // For shared libraries, use -l flag
        return fmt.tprintf("-L%s -lopenblas", filepath.dir(blas_lib))
    }
}

// Global verbose flag for debugging
verbose_logging := false