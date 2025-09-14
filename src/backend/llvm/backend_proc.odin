package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "../../ast"
import "core:c"

// Procedure generation (like Odin's llvm_backend_proc.cpp)

// Generate LLVM IR for a procedure
gen_procedure :: proc(gen: ^IRGenerator, entity: ^checker.Entity) -> bool {
	fmt.printf("  Generating procedure: %s\n", entity.name)
	
	// Create function type from entity's type
	func_type: llvm.LLVMTypeRef
	if entity.type != nil && entity.type.kind == .Proc {
		// Use the procedure type from the entity
		func_type = type_to_llvm(gen, entity.type)
	} else {
		// Fallback: create a simple function that returns i32
		ret_type := llvm.LLVMInt32TypeInContext(gen.ctx)
		func_type = llvm.LLVMFunctionType(ret_type, nil, 0, 0)
	}
	
	// Create the function
	func_name := strings.clone_to_cstring(entity.name, context.temp_allocator)
	func := llvm.LLVMAddFunction(gen.module, func_name, func_type)
	
	// Store in IR symbol table
	gen.ir_symbols[entity] = func
	
	// Set as current function
	gen.current_function = func
	
	// Create entry basic block
	entry_block := llvm.LLVMAppendBasicBlockInContext(gen.ctx, func, "entry")
	llvm.LLVMPositionBuilderAtEnd(gen.builder, entry_block)
	
	// Get the procedure AST
	if entity.decl != nil {
		if value_decl, ok := entity.decl.derived_stmt.(^ast.Value_Decl); ok {
			// Find the procedure literal
			for value in value_decl.values {
				if proc_lit, ok := value.derived_expr.(^ast.Proc_Lit); ok {
					// Generate procedure body
					if !gen_procedure_body(gen, proc_lit) {
						return false
					}
					break
				}
			}
		}
	}
	
	// Add a default return if the function doesn't have one
	// Check if the last instruction is a terminator
	last_block := llvm.LLVMGetInsertBlock(gen.builder)
	if last_block != nil {
		last_inst := llvm.LLVMGetLastInstruction(last_block)
		if last_inst == nil || llvm.LLVMIsATerminatorInst(last_inst) == nil {
			// Add default return based on function's return type
			ret_type := llvm.LLVMGetReturnType(func_type)
			if llvm.LLVMGetTypeKind(ret_type) == .VoidTypeKind {
				llvm.LLVMBuildRetVoid(gen.builder)
			} else {
				zero := llvm.LLVMConstInt(ret_type, 0, 0)
				llvm.LLVMBuildRet(gen.builder, zero)
			}
		}
	}
	
	fmt.printf("  Generated procedure: %s\n", entity.name)
	return true
}

// Generate the body of a procedure
gen_procedure_body :: proc(gen: ^IRGenerator, proc_lit: ^ast.Proc_Lit) -> bool {
	// Map procedure parameters to LLVM function arguments
	if proc_lit.type != nil && len(proc_lit.type.params.list) > 0 {
		param_index := 0
		for param_group in proc_lit.type.params.list {
			for param_name in param_group.names {
				if ident, ok := param_name.derived_expr.(^ast.Ident); ok {
					// Get the parameter entity from the scope
					param_entity := lookup_entity_by_name(gen.checker_info, ident.name)
					if param_entity != nil {
						// Get the corresponding LLVM function parameter
						param_value := llvm.LLVMGetParam(gen.current_function, cast(c.uint)param_index)
						// Store the parameter value in the symbol table
						gen.ir_symbols[param_entity] = param_value
						fmt.printf("    Mapped parameter %s to LLVM arg %d\n", ident.name, param_index)
						param_index += 1
					} else {
						fmt.printf("    WARNING: Could not find parameter entity for %s\n", ident.name)
					}
				}
			}
		}
	}
	
	// Generate procedure body statements
	if proc_lit.body != nil {
		if block_stmt, ok := proc_lit.body.derived.(^ast.Block_Stmt); ok {
			for stmt in block_stmt.stmts {
				if !gen_stmt(gen, stmt) {
					return false
				}
			}
		}
	}
	
	return true
}