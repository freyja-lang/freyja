package llvm_backend

import "core:os"
import "core:fmt"
import "core:strings"
import "core:path/filepath"
import "core:c/libc"

// Get the BLAS library path for linking
get_blas_library_path :: proc() -> string {
    // Detect platform
    os_name := ""
    arch_name := ""
    
    when ODIN_OS == .Linux {
        os_name = "linux"
    } else when ODIN_OS == .Darwin {
        os_name = "darwin"
    } else when ODIN_OS == .Windows {
        os_name = "windows"
    }
    
    when ODIN_ARCH == .amd64 {
        arch_name = "x64"
    } else when ODIN_ARCH == .arm64 {
        arch_name = "arm64"
    }
    
    // Check for vendored library first
    vendor_path := fmt.tprintf("vendor/linalg/%s-%s/libopenblas.a", os_name, arch_name)
    if os.exists(vendor_path) {
        abs_path, _ := filepath.abs(vendor_path)
        fmt.printf("Using vendored BLAS: %s\n", abs_path)
        return abs_path
    }
    
    // Check for Windows library name
    if os_name == "windows" {
        vendor_path = fmt.tprintf("vendor/linalg/%s-%s/openblas.lib", os_name, arch_name)
        if os.exists(vendor_path) {
            abs_path, _ := filepath.abs(vendor_path)
            fmt.printf("Using vendored BLAS: %s\n", abs_path)
            return abs_path
        }
    }
    
    // Fall back to system library
    fmt.printf("No vendored BLAS found, using system library\n")
    return ""
}

// Generate compile command with BLAS linking
generate_compile_command :: proc(ll_file: string, output_file: string) -> string {
    blas_lib := get_blas_library_path()
    
    // Base compilation command
    cmd := strings.builder_make()
    
    // Use clang for compilation (works on all platforms)
    strings.write_string(&cmd, "clang ")
    strings.write_string(&cmd, ll_file)
    strings.write_string(&cmd, " -o ")
    strings.write_string(&cmd, output_file)
    
    // Add BLAS library
    if blas_lib != "" {
        strings.write_string(&cmd, " ")
        strings.write_string(&cmd, blas_lib)
    } else {
        // Fall back to system BLAS
        when ODIN_OS == .Linux {
            strings.write_string(&cmd, " -lopenblas")
        } else when ODIN_OS == .Darwin {
            strings.write_string(&cmd, " -framework Accelerate")
        } else when ODIN_OS == .Windows {
            strings.write_string(&cmd, " openblas.lib")
        }
    }
    
    // Add other necessary flags
    when ODIN_OS == .Linux {
        strings.write_string(&cmd, " -lm -lpthread")  // Math and threading
    }
    
    return strings.to_string(cmd)
}

// Create a static library from object file
create_static_library :: proc(obj_file: string, lib_file: string) -> bool {
    // Create static library with ar
    cmd := ""
    when ODIN_OS == .Windows {
        cmd = fmt.aprintf("lib /OUT:%s %s", lib_file, obj_file)
    } else {
        cmd = fmt.aprintf("ar rcs %s %s", lib_file, obj_file)
    }
    
    defer delete(cmd)
    result := libc.system(strings.clone_to_cstring(cmd, context.temp_allocator))
    if result != 0 {
        fmt.eprintln("Failed to create static library")
        return false
    }
    
    fmt.printf("Created static library: %s\n", lib_file)
    return true
}