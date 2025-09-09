package llvm_backend

import "core:fmt"
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

	// Write bitcode for now
	bitcode_path := "out/output.bc"
	result := llvm.LLVMWriteBitcodeToFile(opt_result.module, cstring(raw_data(bitcode_path)))

	if result != 0 {
		fmt.eprintln("Failed to write bitcode")
		return CodegenResult{success = false}
	}

	fmt.printf("Bitcode written to %s\n", bitcode_path)

	// TODO: Use LLVM's target machine to generate actual object files
	// TODO: Link with system libraries

	return CodegenResult{output_file = bitcode_path, success = true}
}