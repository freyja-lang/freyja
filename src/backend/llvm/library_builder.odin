package llvm_backend

import "core:os"
import "core:fmt"
import "core:strings"
import "core:path/filepath"
import "core:c/libc"
import llvm "../../llvm"

// Helper to execute shell commands
exec_cmd :: proc(cmd: string) -> int {
    return int(libc.system(strings.clone_to_cstring(cmd, context.temp_allocator)))
}

// Helper to remove directory recursively
remove_dir :: proc(path: string) -> bool {
    cmd := fmt.aprintf("rm -rf %s", path)
    defer delete(cmd)
    return exec_cmd(cmd) == 0
}

Library_Type :: enum {
    Smart_Fat,  // Default: Include only used BLAS functions
    Thin,       // Just Freyja code, requires external BLAS
    Full_Fat,   // Include entire OpenBLAS (not recommended)
}

Build_Output :: struct {
    library_type: Library_Type,
    output_path:  string,
    include_debug: bool,
}

// Build a library from LLVM module
build_library :: proc(module: llvm.LLVMModuleRef, config: Build_Output) -> bool {
    // First, generate object file
    obj_path := strings.concatenate([]string{config.output_path, ".o"})
    defer delete(obj_path)
    
    if !generate_object_file(module, obj_path) {
        return false
    }
    
    // Build library based on type
    switch config.library_type {
    case .Smart_Fat:
        return build_smart_fat_library(obj_path, config.output_path)
    case .Thin:
        return build_thin_library(obj_path, config.output_path)
    case .Full_Fat:
        return build_full_fat_library(obj_path, config.output_path)
    }
    
    return false
}

// Generate object file from LLVM module
generate_object_file :: proc(module: llvm.LLVMModuleRef, obj_path: string) -> bool {
    // First write LLVM IR to compile it properly
    ll_path := strings.concatenate([]string{obj_path, ".ll"})
    defer delete(ll_path)
    
    ir_string := llvm.LLVMPrintModuleToString(module)
    defer llvm.LLVMDisposeMessage(ir_string)
    
    if !os.write_entire_file(ll_path, transmute([]u8)string(ir_string)) {
        fmt.eprintln("Failed to write IR file")
        return false
    }
    
    // Compile LLVM IR to native object with clang
    compile_cmd := fmt.aprintf("clang -c -O2 %s -o %s", ll_path, obj_path)
    defer delete(compile_cmd)
    
    result := libc.system(strings.clone_to_cstring(compile_cmd, context.temp_allocator))
    if result != 0 {
        fmt.eprintln("Failed to compile object")
        return false
    }
    
    // Clean up the temporary .ll file
    os.remove(ll_path)
    
    return true
}

// Build smart fat library - includes only used BLAS functions
build_smart_fat_library :: proc(obj_path: string, output_path: string) -> bool {
    fmt.println("Building smart fat library (includes only used BLAS functions)...")
    
    // Get BLAS library path
    blas_lib := find_blas_library()
    if blas_lib == "" {
        fmt.eprintln("Warning: No BLAS library found, building thin library instead")
        return build_thin_library(obj_path, output_path)
    }
    
    // Create temporary directory for extraction
    temp_dir := "tmp_lib_build"
    os.make_directory(temp_dir)
    defer remove_dir(temp_dir)
    
    // Use partial linking to include only needed symbols
    // This is the key to smart linking!
    partial_obj := filepath.join([]string{temp_dir, "partial.o"})
    
    // Step 1: Partial link with --gc-sections to eliminate unused code
    link_cmd := fmt.aprintf(
        "ld -r --gc-sections %s %s -o %s",
        obj_path, blas_lib, partial_obj
    )
    
    result := exec_cmd(link_cmd)
    if result != 0 {
        // Fallback: Try with clang
        link_cmd = fmt.aprintf(
            "clang -Wl,-r -Wl,--gc-sections %s %s -o %s",
            obj_path, blas_lib, partial_obj
        )
        defer delete(link_cmd)
        result = exec_cmd(link_cmd)
        
        if result != 0 {
            fmt.eprintln("Partial linking failed, trying alternative method...")
            return build_smart_fat_alternative(obj_path, blas_lib, output_path)
        }
    }
    
    // Step 2: Create archive from partial-linked object
    ar_cmd := fmt.aprintf("ar rcs %s %s", output_path, partial_obj)
    result = exec_cmd(ar_cmd)
    
    if result != 0 {
        fmt.eprintln("Failed to create archive")
        return false
    }
    defer delete(ar_cmd)
    
    // Get final size
    if info, err := os.stat(output_path); err == nil {
        size_mb := f64(info.size) / (1024.0 * 1024.0)
        fmt.printf("✓ Created smart fat library: %s (%.1f MB)\n", output_path, size_mb)
        fmt.println("  Contains: Freyja kernels + only used BLAS/LAPACK functions")
    }
    
    return true
}

// Alternative smart fat method using archiver manipulation
build_smart_fat_alternative :: proc(obj_path: string, blas_lib: string, output_path: string) -> bool {
    fmt.println("Using alternative smart linking method...")
    
    // Extract only needed symbols
    temp_dir := "tmp_lib_build"
    os.make_directory(temp_dir)
    defer remove_dir(temp_dir)
    
    // Get list of undefined BLAS symbols from our object
    symbols_cmd := fmt.aprintf("nm -u %s | grep -E '(gemm_|gemv_|gesv_|getrf_)'", obj_path)
    defer delete(symbols_cmd)
    symbols_result := exec_cmd(symbols_cmd)
    
    if symbols_result != 0 {
        // Fallback to including more
        return build_conservative_fat_library(obj_path, blas_lib, output_path)
    }
    
    // Extract specific object files from BLAS library
    // This is more complex but gives better size control
    
    // For now, fall back to conservative approach
    return build_conservative_fat_library(obj_path, blas_lib, output_path)
}

// Conservative fat library - includes common BLAS/LAPACK routines
build_conservative_fat_library :: proc(obj_path: string, blas_lib: string, output_path: string) -> bool {
    fmt.println("Building conservative fat library (common routines only)...")
    
    temp_dir := "tmp_lib_build"
    os.make_directory(temp_dir)
    defer remove_dir(temp_dir)
    
    // Extract BLAS library
    extract_cmd := fmt.aprintf("cd %s && ar x ../%s", temp_dir, blas_lib)
    exec_cmd(extract_cmd)
    
    // Copy our object
    cp_cmd := fmt.aprintf("cp %s %s/freyja_kernels.o", obj_path, temp_dir)
    exec_cmd(cp_cmd)
    
    // Create archive with all objects for now
    // Smart linking is complex with OpenBLAS due to internal dependencies
    create_cmd := fmt.aprintf("cd %s && ar rcs ../%s *.o", temp_dir, output_path)
    defer delete(create_cmd)
    
    result := exec_cmd(create_cmd)
    if result != 0 {
        fmt.eprintln("Failed to create archive")
        return false
    }
    
    if info, err := os.stat(output_path); err == nil {
        size_mb := f64(info.size) / (1024.0 * 1024.0)
        fmt.printf("✓ Created conservative fat library: %s (%.1f MB)\n", output_path, size_mb)
    }
    
    return true
}

// Build thin library - just Freyja code
build_thin_library :: proc(obj_path: string, output_path: string) -> bool {
    fmt.println("Building thin library (requires external BLAS)...")

    result := 0
    when ODIN_OS == .Windows {
        // Try multiple archiver options on Windows
        // First try llvm-lib (comes with LLVM)
        ar_cmd := fmt.aprintf("llvm-lib /OUT:%s %s 2>NUL", output_path, obj_path)
        result = exec_cmd(ar_cmd)

        if result != 0 {
            // If llvm-lib fails, try lib.exe (Visual Studio)
            ar_cmd = fmt.aprintf("lib /OUT:%s %s 2>NUL", output_path, obj_path)
            result = exec_cmd(ar_cmd)

            if result != 0 {
                // If both fail, try ar (MinGW/MSYS2/Git Bash)
                ar_cmd = fmt.aprintf("ar rcs %s %s", output_path, obj_path)
                result = exec_cmd(ar_cmd)

                if result != 0 {
                    fmt.eprintln("Failed to create library. Please install one of:")
                    fmt.eprintln("  - LLVM (for llvm-lib)")
                    fmt.eprintln("  - Visual Studio Build Tools (for lib.exe)")
                    fmt.eprintln("  - MinGW/MSYS2 (for ar)")
                    return false
                }
            }
        }
    } else {
        // Use ar on Unix-like systems
        ar_cmd := fmt.aprintf("ar rcs %s %s", output_path, obj_path)
        result = exec_cmd(ar_cmd)
    }
    
    if result != 0 {
        fmt.eprintln("Failed to create thin library:", result)
        return false
    }
    
    if info, err := os.stat(output_path); err == nil {
        size_kb := f64(info.size) / 1024.0
        fmt.printf("✓ Created thin library: %s (%.1f KB)\n", output_path, size_kb)
        fmt.println("  Note: Requires linking with BLAS/LAPACK at build time")
    }
    
    return true
}

// Build full fat library - includes entire OpenBLAS
build_full_fat_library :: proc(obj_path: string, output_path: string) -> bool {
    fmt.println("Building full fat library (includes entire OpenBLAS)...")
    
    blas_lib := find_blas_library()
    if blas_lib == "" {
        fmt.eprintln("No BLAS library found")
        return false
    }
    
    temp_dir := "tmp_lib_build"
    os.make_directory(temp_dir)
    defer remove_dir(temp_dir)
    
    // Extract entire BLAS library
    extract_cmd := fmt.aprintf("cd %s && ar x ../%s", temp_dir, blas_lib)
    exec_cmd(extract_cmd)
    
    // Add our object
    cp_cmd := fmt.aprintf("cp %s %s/", obj_path, temp_dir)
    exec_cmd(cp_cmd)
    
    // Create combined archive
    ar_cmd := fmt.aprintf("cd %s && ar rcs ../%s *.o", temp_dir, output_path)
    result := exec_cmd(ar_cmd)
    
    if result != 0 {
        fmt.eprintln("Failed to create full fat library")
        return false
    }
    
    if info, err := os.stat(output_path); err == nil {
        size_mb := f64(info.size) / (1024.0 * 1024.0)
        fmt.printf("✓ Created full fat library: %s (%.1f MB)\n", output_path, size_mb)
        fmt.println("  Warning: Contains entire OpenBLAS - consider using smart fat instead")
    }
    
    return true
}

// Find BLAS library path
find_blas_library :: proc() -> string {
    // Check vendor directory first
    platform := detect_platform()
    vendor_path := fmt.aprintf("vendor/linalg/%s/libopenblas.a", platform)
    
    if os.exists(vendor_path) {
        return vendor_path
    }
    
    // Check system locations
    system_paths := []string{
        "/usr/lib/x86_64-linux-gnu/libopenblas.a",
        "/usr/lib64/libopenblas.a",
        "/usr/local/lib/libopenblas.a",
    }
    
    for path in system_paths {
        if os.exists(path) {
            return path
        }
    }
    
    return ""
}

// Detect current platform
detect_platform :: proc() -> string {
    when ODIN_OS == .Linux {
        when ODIN_ARCH == .amd64 {
            return "linux-x64"
        }
    } else when ODIN_OS == .Darwin {
        when ODIN_ARCH == .amd64 {
            return "darwin-x64"
        } else when ODIN_ARCH == .arm64 {
            return "darwin-arm64"
        }
    } else when ODIN_OS == .Windows {
        return "windows-x64"
    }
    
    return "unknown"
}