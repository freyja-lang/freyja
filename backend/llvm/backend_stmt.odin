package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "core:odin/ast"

// Statement generation (like Odin's llvm_backend_stmt.cpp)

// Generate LLVM IR for a statement
gen_stmt :: proc(gen: ^IRGenerator, stmt: ^ast.Stmt) -> bool {
	#partial switch s in stmt.derived_stmt {
	case ^ast.Value_Decl:
		// Variable declarations (local variables)
		return gen_value_decl(gen, s)
		
	case ^ast.Assign_Stmt:
		// Assignment statements
		return gen_assign_stmt(gen, s)
		
	case ^ast.Block_Stmt:
		// Block statements
		for nested_stmt in s.stmts {
			if !gen_stmt(gen, nested_stmt) {
				return false
			}
		}
		return true
		
	case ^ast.Return_Stmt:
		// Return statements
		if len(s.results) == 0 {
			// Void return
			llvm.LLVMBuildRetVoid(gen.builder)
		} else if len(s.results) == 1 {
			// Single value return
			value := gen_expr(gen, s.results[0])
			if value != nil {
				llvm.LLVMBuildRet(gen.builder, value)
			}
		} else {
			// TODO: Multiple return values
			fmt.printf("Multiple return values not yet supported\n")
			return false
		}
		return true
		
	case ^ast.Expr_Stmt:
		// Expression statements
		_ = gen_expr(gen, s.expr)
		return true
		
	case:
		fmt.printf("Unhandled statement type: %T\n", s)
		return true
	}
}

// Generate IR for value declarations (local variables)
gen_value_decl :: proc(gen: ^IRGenerator, decl: ^ast.Value_Decl) -> bool {
	for i in 0 ..< len(decl.names) {
		name_expr := decl.names[i]
		value_expr := decl.values[i]
		
		// Get the variable name
		name: string
		if ident, ok := name_expr.derived_expr.(^ast.Ident); ok {
			name = ident.name
		} else {
			fmt.printf("Declaration name is not an identifier\n")
			return false
		}
		
		// TODO: Get type from checker info
		// For now, assume i32 for integers
		var_type := llvm.LLVMInt32TypeInContext(gen.ctx)
		
		// Allocate stack space for the variable
		var_name := strings.clone_to_cstring(name, context.temp_allocator)
		alloca := llvm.LLVMBuildAlloca(gen.builder, var_type, var_name)
		
		// Generate the initial value
		init_value := gen_expr(gen, value_expr)
		if init_value == nil {
			fmt.printf("Failed to generate initial value for %s\n", name)
			return false
		}
		
		// Store the initial value
		llvm.LLVMBuildStore(gen.builder, init_value, alloca)
		
		// Look up the entity from checker info
		entity := lookup_entity_by_name(gen.checker_info, name)
		if entity != nil {
			// Add to IR symbol table
			gen.ir_symbols[entity] = alloca
		}
		
		fmt.printf("    Generated local variable: %s\n", name)
	}
	
	return true
}

// Generate IR for assignment statements
gen_assign_stmt :: proc(gen: ^IRGenerator, assign: ^ast.Assign_Stmt) -> bool {
	if len(assign.lhs) != len(assign.rhs) {
		fmt.printf("Assignment count mismatch\n")
		return false
	}
	
	for i in 0 ..< len(assign.lhs) {
		// Get the target variable
		lhs := assign.lhs[i]
		
		// For now, assume it's a simple identifier
		if ident, ok := lhs.derived_expr.(^ast.Ident); ok {
			name := ident.name
			
			// Look up the entity
			entity := lookup_entity_by_name(gen.checker_info, name)
			if entity == nil {
				fmt.printf("Undefined variable in assignment: %s\n", name)
				return false
			}
			
			// Look up in IR symbol table
			if alloca, exists := gen.ir_symbols[entity]; exists {
				// Generate the value to assign
				value := gen_expr(gen, assign.rhs[i])
				if value == nil {
					return false
				}
				
				// Store the value
				llvm.LLVMBuildStore(gen.builder, value, alloca)
			} else {
				fmt.printf("Variable not allocated: %s\n", name)
				return false
			}
		} else {
			fmt.printf("Complex assignment targets not yet supported\n")
			return false
		}
	}
	
	return true
}