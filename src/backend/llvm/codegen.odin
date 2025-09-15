package llvm_backend

import "core:fmt"
import "core:os"
import "core:strings"
import llvm "../../llvm"

CodegenResult :: struct {
	output_file: string,
	success:     bool,
}

// Build configuration
Build_Config :: struct {
	output_type:   Output_Type,
	library_type:  Library_Type,
	output_path:   string,
	include_debug: bool,
}

Output_Type :: enum {
	IR_Only,       // Just output LLVM IR
	Object_File,   // Generate .o file
	Static_Library, // Generate .a file with BLAS
	Shared_Library, // Generate .so file
}

// Step 5: Generate code (assembly/object file) and link
generate_code :: proc(
	opt_result: OptimizeResult,
	ir_result: IRResult,
	output_path: string,
) -> CodegenResult {
	// Default configuration - thin library for testing
	config := Build_Config{
		output_type = .Static_Library,
		library_type = .Thin,  // Use thin for now until smart linking is improved
		output_path = output_path,
		include_debug = false,
	}
	
	return generate_code_with_config(opt_result, ir_result, config)
}

// Generate code with specific configuration
generate_code_with_config :: proc(
	opt_result: OptimizeResult,
	ir_result: IRResult,
	config: Build_Config,
) -> CodegenResult {
	fmt.printf("\n=== CODE GENERATION ===\n")
	fmt.printf("Generating code...\n")
	fmt.printf("Output type: %v\n", config.output_type)
	if config.output_type == .Static_Library {
		fmt.printf("Library type: %v\n", config.library_type)
	}

	if !opt_result.success {
		fmt.eprintln("Cannot generate code: optimization failed")
		return CodegenResult{success = false}
	}

	// Write LLVM IR text for debugging/inspection
	ir_path := "out/output.ll"
	ir_string := llvm.LLVMPrintModuleToString(opt_result.module)
	defer llvm.LLVMDisposeMessage(ir_string)
	
	// Write IR to file
	if !os.write_entire_file(ir_path, transmute([]u8)string(ir_string)) {
		fmt.eprintln("Failed to write IR file")
		return CodegenResult{success = false}
	}
	fmt.printf("LLVM IR written to %s\n", ir_path)

	// Handle different output types
	switch config.output_type {
	case .IR_Only:
		return CodegenResult{output_file = ir_path, success = true}
		
	case .Object_File:
		// Generate object file
		obj_path := strings.concatenate([]string{config.output_path, ".o"})
		if !generate_object_file(opt_result.module, obj_path) {
			return CodegenResult{success = false}
		}
		return CodegenResult{output_file = obj_path, success = true}
		
	case .Static_Library:
		// Build static library with selected linking strategy
		lib_ext := ""
		when ODIN_OS == .Windows {
			lib_ext = ".lib"
		} else {
			lib_ext = ".a"
		}
		lib_path := strings.concatenate([]string{config.output_path, lib_ext})
		build_config := Build_Output{
			library_type = config.library_type,
			output_path = lib_path,
			include_debug = config.include_debug,
		}
		
		if !build_library(opt_result.module, build_config) {
			return CodegenResult{success = false}
		}
		return CodegenResult{output_file = lib_path, success = true}
		
	case .Shared_Library:
		fmt.eprintln("Shared library generation not yet implemented")
		return CodegenResult{success = false}
	}

	return CodegenResult{success = false}
}