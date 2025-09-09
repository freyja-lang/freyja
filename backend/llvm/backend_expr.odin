package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:odin/ast"

// Expression generation (like Odin's llvm_backend_expr.cpp)

// Generate LLVM IR for an expression (without type hint)
gen_expr :: proc(gen: ^IRGenerator, expr: ^ast.Expr) -> llvm.LLVMValueRef {
	return gen_expr_typed(gen, expr, nil)
}

// Generate LLVM IR for an expression with a target type hint
gen_expr_typed :: proc(gen: ^IRGenerator, expr: ^ast.Expr, target_type: ^checker.Type) -> llvm.LLVMValueRef {
	#partial switch e in expr.derived_expr {
	case ^ast.Basic_Lit:
		// Generate literal values
		#partial switch e.tok.kind {
		case .Integer:
			// Parse the actual integer value
			value, ok := strconv.parse_i64(e.tok.text)
			if !ok {
				fmt.printf("Failed to parse integer literal: %s\n", e.tok.text)
				value = 0
			}
			
			// Use target type if provided, otherwise default to i32
			if target_type != nil && target_type.kind == .Basic {
				if basic, ok := target_type.variant.(checker.TypeBasic); ok {
					if .Float in basic.flags {
						// Convert integer literal to float
						float_value := cast(f64)value
						if basic.kind == .f32 {
							return llvm.LLVMConstReal(llvm.LLVMFloatTypeInContext(gen.ctx), float_value)
						} else {
							return llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), float_value)
						}
					} else if .Integer in basic.flags {
						// Generate integer of the appropriate size
						llvm_type := type_to_llvm(gen, target_type)
						return llvm.LLVMConstInt(llvm_type, cast(u64)value, 0)
					}
				}
			}
			// Default to platform int
			default_int_type := type_to_llvm(gen, checker.t_int)
			return llvm.LLVMConstInt(default_int_type, cast(u64)value, 0)
		case .Float:
			// Parse the actual float value
			value, ok := strconv.parse_f64(e.tok.text)
			if !ok {
				fmt.printf("Failed to parse float literal: %s\n", e.tok.text)
				value = 0.0
			}
			
			// Use target type if provided
			if target_type != nil && target_type.kind == .Basic {
				if basic, ok := target_type.variant.(checker.TypeBasic); ok {
					if basic.kind == .f32 {
						return llvm.LLVMConstReal(llvm.LLVMFloatTypeInContext(gen.ctx), value)
					}
				}
			}
			// Default to f64
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