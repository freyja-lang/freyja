package freyja

//   Parse → Check → Generate IR → Optimize → Link

import llvm "./llvm"
import llvm_backend "backend/llvm"
import "checker"
import "core:fmt"
import "core:os"
import "parser"

compile_file :: proc(filepath: string) -> bool {
	fmt.printf("Compiling %s\n", filepath)
	fmt.println("=====================================")

	// Step 1: Parse
	parse_result := parser.parse_file(filepath)
	if !parse_result.success {
		fmt.eprintln("\nCompilation failed at parsing stage")
		return false
	}

	// Step 2: Type check
	check_result := checker.check(parse_result)
	if !check_result.success {
		fmt.eprintln("\nCompilation failed at type checking stage")
		return false
	}

	// Step 3: Generate IR
	ir_result := llvm_backend.generate_ir(parse_result, check_result)
	defer if ir_result.success {
		llvm.LLVMDisposeModule(ir_result.module)
		llvm.LLVMContextDispose(ir_result.ctx)
	}

	if !ir_result.success {
		fmt.eprintln("\nCompilation failed at IR generation stage")
		return false
	}

	// Step 4: Optimize
	opt_result := llvm_backend.optimize_ir(ir_result)
	if !opt_result.success {
		fmt.eprintln("\nCompilation failed at optimization stage")
		return false
	}

	// Step 5: Generate code
	codegen_result := llvm_backend.generate_code(opt_result, ir_result, "output")
	if !codegen_result.success {
		fmt.eprintln("\nCompilation failed at code generation stage")
		return false
	}

	fmt.println("\n=====================================")
	fmt.printf("Output: %s\n", codegen_result.output_file)

	return true
}

main :: proc() {
	// Quick test of LLVM
	if len(os.args) == 2 && os.args[1] == "test-llvm" {
		llvm_backend.test_llvm()
		return
	}

	if len(os.args) < 2 {
		fmt.eprintln("Usage: freyja <file.odin>")
		os.exit(1)
	}

	filepath := os.args[1]

	if !compile_file(filepath) {
		os.exit(1)
	}

	fmt.println("Compilation successful!")
}
