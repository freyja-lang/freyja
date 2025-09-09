package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "../../parser"
import "core:fmt"
import "core:odin/ast"

IRResult :: struct {
	module:  llvm.LLVMModuleRef,
	ctx:     llvm.LLVMContextRef,
	success: bool,
}

// Symbol table entry for IR generation (maps to LLVM values)
IRSymbol :: struct {
	name:       string,
	llvm_value: llvm.LLVMValueRef,
	entity:     ^checker.Entity, // Reference to checker entity
}

// IR Generator context
IRGenerator :: struct {
	// LLVM state
	ctx:              llvm.LLVMContextRef,
	module:           llvm.LLVMModuleRef,
	builder:          llvm.LLVMBuilderRef,

	// Checker info with all symbol tables and type information
	checker_info:     ^checker.CheckerInfo,

	// IR symbol table (maps entities to LLVM values)
	ir_symbols:       map[^checker.Entity]llvm.LLVMValueRef,

	// Current function being generated
	current_function: llvm.LLVMValueRef,

	// Built-in LLVM types (cached)
	llvm_i32:         llvm.LLVMTypeRef,
	llvm_i64:         llvm.LLVMTypeRef,
	llvm_f64:         llvm.LLVMTypeRef,
	llvm_void:        llvm.LLVMTypeRef,
}

// Initialize the IR generator with checker info
ir_gen_init :: proc(gen: ^IRGenerator, checker_info: ^checker.CheckerInfo) {
	gen.ctx = llvm.LLVMContextCreate()
	gen.module = llvm.LLVMModuleCreateWithNameInContext("freyja_module", gen.ctx)
	gen.builder = llvm.LLVMCreateBuilderInContext(gen.ctx)

	// Store checker info
	gen.checker_info = checker_info

	// Initialize LLVM types
	gen.llvm_i32 = llvm.LLVMInt32TypeInContext(gen.ctx)
	gen.llvm_i64 = llvm.LLVMInt64TypeInContext(gen.ctx)
	gen.llvm_f64 = llvm.LLVMDoubleTypeInContext(gen.ctx)
	gen.llvm_void = llvm.LLVMVoidTypeInContext(gen.ctx)

	// Type conversion is now done on-demand via type_to_llvm()

	// Initialize IR symbol table
	gen.ir_symbols = make(map[^checker.Entity]llvm.LLVMValueRef)
}

// Cleanup the IR generator
ir_gen_destroy :: proc(gen: ^IRGenerator) {
	delete(gen.ir_symbols)
	llvm.LLVMDisposeBuilder(gen.builder)
}

// Generate a simple main function (hardcoded fallback)
ir_gen_main_function :: proc(gen: ^IRGenerator) {
	fmt.printf("Generating main function (fallback)...\n")

	// Create main function type: i32 main()
	main_type := llvm.LLVMFunctionType(gen.llvm_i32, nil, 0, 0)
	main_fn := llvm.LLVMAddFunction(gen.module, "main", main_type)
	gen.current_function = main_fn

	// Create entry block
	entry := llvm.LLVMAppendBasicBlockInContext(gen.ctx, main_fn, "entry")
	llvm.LLVMPositionBuilderAtEnd(gen.builder, entry)

	// Return 0
	ret_val := llvm.LLVMConstInt(gen.llvm_i32, 0, 0)
	llvm.LLVMBuildRet(gen.builder, ret_val)

	fmt.printf("Generated fallback main function\n")
}

// Generate IR from checker entities (Odin-style approach)
ir_gen_from_entities :: proc(gen: ^IRGenerator) -> bool {
	fmt.printf("Generating IR from entities...\n")

	// First pass: Generate all procedure declarations
	for entity in gen.checker_info.entities {
		if entity.kind == .PROCEDURE {
			// Generate the procedure
			if !gen_procedure(gen, entity) {
				fmt.eprintf("Failed to generate procedure: %s\n", entity.name)
				return false
			}
		}
	}

	// Check if main exists
	main_entity: ^checker.Entity = nil
	for entity in gen.checker_info.entities {
		if entity.kind == .PROCEDURE && entity.name == "main" {
			main_entity = entity
			break
		}
	}

	if main_entity == nil {
		fmt.eprintln("No main procedure found")
		return false
	}

	return true
}

// Generate LLVM IR for a procedure entity
ir_gen_procedure_from_entity :: proc(gen: ^IRGenerator, proc_entity: ^checker.Entity) -> bool {
	// Use the new modular procedure generation
	return gen_procedure(gen, proc_entity)
}

// Old AST-based function removed - now using entity-based approach

// TODO: Implement AST-based IR generation using checker results
// This will be added later when we need to generate procedure bodies

// Step 3: Generate LLVM IR from the checked AST
generate_ir :: proc(
	parse_result: parser.ParseResult,
	check_result: checker.CheckResult,
) -> IRResult {
	fmt.printf("\n=== IR GENERATION ===\n")
	fmt.printf("Generating LLVM IR...\n")

	if !check_result.success {
		fmt.eprintln("Cannot generate IR: type checking failed")
		return IRResult{success = false}
	}

	if check_result.info == nil {
		fmt.eprintln("No checker info available")
		return IRResult{success = false}
	}

	// Create IR generator with checker info
	gen: IRGenerator
	ir_gen_init(&gen, check_result.info)
	defer ir_gen_destroy(&gen)

	// Generate IR from checker entities instead of AST
	fmt.printf("Generating from %d entities\n", len(check_result.info.entities))
	success := ir_gen_from_entities(&gen)
	if !success {
		fmt.eprintln("IR generation failed")
		return IRResult{success = false}
	}

	// Print generated IR
	ir_string := llvm.LLVMPrintModuleToString(gen.module)
	fmt.printf("Generated IR:\n%s\n", ir_string)
	llvm.LLVMDisposeMessage(ir_string)

	return IRResult{module = gen.module, ctx = gen.ctx, success = true}
}
