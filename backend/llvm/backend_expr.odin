package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "core:odin/ast"

// Expression generation (like Odin's llvm_backend_expr.cpp)

// Generate LLVM IR for an expression
gen_expr :: proc(gen: ^IRGenerator, expr: ^ast.Expr) -> llvm.LLVMValueRef {
	#partial switch e in expr.derived_expr {
	case ^ast.Basic_Lit:
		// Generate literal values
		#partial switch e.tok.kind {
		case .Integer:
			// For now, assume i32
			value := 0 // TODO: Parse actual value from token
			return llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)value, 0)
		case .Float:
			// For now, assume f64
			value := 0.0 // TODO: Parse actual value from token
			return llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), value)
		case:
			fmt.printf("Unhandled literal type: %v\n", e.tok.kind)
			return nil
		}
		
	case ^ast.Ident:
		// Variable reference
		name := e.name
		
		// Look up the entity in checker info
		entity := lookup_entity_by_name(gen.checker_info, name)
		if entity == nil {
			fmt.printf("Undefined identifier in IR generation: %s\n", name)
			return nil
		}
		
		// Look up in our IR symbol table
		if llvm_value, exists := gen.ir_symbols[entity]; exists {
			// Check if this is an alloca (local variable) or a direct value (parameter)
			if entity.kind == .VARIABLE {
				// Check if this is an alloca instruction (local variable) or a parameter
				value_kind := llvm.LLVMGetValueKind(llvm_value)
				if value_kind == .InstructionValueKind {
					// It's an alloca, need to load
					var_type := type_to_llvm(gen, entity.type)
					load_name := strings.clone_to_cstring(fmt.tprintf("%s_load", name), context.temp_allocator)
					return llvm.LLVMBuildLoad2(gen.builder, var_type, llvm_value, load_name)
				}
				// It's a parameter, use directly
				return llvm_value
			}
			return llvm_value
		}
		
		fmt.printf("Undefined identifier in IR generation: %s\n", name)
		return nil
		
	case ^ast.Binary_Expr:
		// Binary operations
		lhs := gen_expr(gen, e.left)
		rhs := gen_expr(gen, e.right)
		
		if lhs == nil || rhs == nil {
			return nil
		}
		
		// Generate operation based on operator
		#partial switch e.op.kind {
		case .Add:
			// TODO: Check types to determine if we need integer or float add
			return llvm.LLVMBuildAdd(gen.builder, lhs, rhs, "add_tmp")
		case .Sub:
			return llvm.LLVMBuildSub(gen.builder, lhs, rhs, "sub_tmp")
		case .Mul:
			return llvm.LLVMBuildMul(gen.builder, lhs, rhs, "mul_tmp")
		case .Quo:
			// TODO: Check types for integer vs float division
			return llvm.LLVMBuildSDiv(gen.builder, lhs, rhs, "div_tmp")
		case:
			fmt.printf("Unhandled binary operator: %v\n", e.op.kind)
			return nil
		}
		
	case ^ast.Call_Expr:
		// Function call
		ident, ident_ok := e.expr.derived.(^ast.Ident)
		if !ident_ok {
			fmt.printf("Complex function expressions not supported in IR generation\n")
			return nil
		}
		
		// Look up the function
		entity := lookup_entity_by_name(gen.checker_info, ident.name)
		if entity == nil {
			fmt.printf("Undefined function in IR generation: %s\n", ident.name)
			return nil
		}
		
		// Get the LLVM function
		func_value, exists := gen.ir_symbols[entity]
		if !exists {
			fmt.printf("Function not generated yet: %s\n", ident.name)
			return nil
		}
		
		// Generate arguments
		args := make([dynamic]llvm.LLVMValueRef, context.temp_allocator)
		for arg_expr in e.args {
			arg_value := gen_expr(gen, arg_expr)
			if arg_value == nil {
				fmt.printf("Failed to generate argument\n")
				return nil
			}
			append(&args, arg_value)
		}
		
		// Generate the call
		call_name := strings.clone_to_cstring(fmt.tprintf("%s_call", ident.name), context.temp_allocator)
		// For now, assume the function returns i32
		func_type := llvm.LLVMFunctionType(llvm.LLVMInt32TypeInContext(gen.ctx), nil, 0, 0)
		return llvm.LLVMBuildCall2(gen.builder, 
			func_type,                  // Function type
			func_value,                 // Function pointer
			raw_data(args),            // Arguments array
			cast(u32)len(args),        // Argument count
			call_name)                  // Name for the result
		
	case:
		fmt.printf("Unhandled expression type in IR generation: %T\n", e)
		return nil
	}
	
	return nil
}