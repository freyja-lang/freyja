package llvm_backend

import "core:fmt"
import "core:os"
import llvm "../../llvm"

CodegenResult :: struct {
	output_file: string,
	success:     bool,
}

// Step 5: Generate code (assembly/object file) and link
generate_code :: proc(
	opt_result: OptimizeResult,
	ir_result: IRResult,
	output_path: string,
) -> CodegenResult {
	fmt.printf("\n=== CODE GENERATION ===\n")
	fmt.printf("Generating code...\n")

	if !opt_result.success {
		fmt.eprintln("Cannot generate code: optimization failed")
		return CodegenResult{success = false}
	}

	// Write LLVM IR text for linking
	ir_path := "out/output.ll"
	ir_string := llvm.LLVMPrintModuleToString(opt_result.module)
	defer llvm.LLVMDisposeMessage(ir_string)
	
	// Write IR to file
	if !os.write_entire_file(ir_path, transmute([]u8)string(ir_string)) {
		fmt.eprintln("Failed to write IR file")
		return CodegenResult{success = false}
	}

	fmt.printf("LLVM IR written to %s\n", ir_path)
	
	// Also keep bitcode for compatibility
	bitcode_path := "out/output.bc"
	result := llvm.LLVMWriteBitcodeToFile(opt_result.module, cstring(raw_data(bitcode_path)))
	if result == 0 {
		fmt.printf("Bitcode written to %s\n", bitcode_path)
	}

	// TODO: Use LLVM's target machine to generate actual object files
	// TODO: Link with system libraries

	return CodegenResult{output_file = ir_path, success = true}
}