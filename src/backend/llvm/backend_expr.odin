package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "../../ast"
import "core:c"

// Expression generation (like Odin's llvm_backend_expr.cpp)

// [REMOVED: Old hardcoded test functions - no longer needed]
// Matrix multiplication is now handled by gen_matrix_multiply with actual variables

/*
gen_test_dgemm_call :: proc(gen: ^IRGenerator) -> llvm.LLVMValueRef {
	// Get dgemm function
	dgemm_fn := get_or_declare_dgemm(gen)
	
	// Create constants for dgemm parameters
	// dgemm('N', 'N', 3, 3, 3, 1.0, A, 3, B, 3, 0.0, C, 3)
	
	// Character constants for transpose ('N' = no transpose)
	char_n := llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0) // 'N'
	trans_a := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans_a", context.temp_allocator))
	trans_b := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans_b", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, char_n, trans_a)
	llvm.LLVMBuildStore(gen.builder, char_n, trans_b)
	
	// Integer constants (M=3, N=3, K=3, LDA=3, LDB=3, LDC=3)
	i32_3 := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0)
	m := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("M", context.temp_allocator))
	n := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("N", context.temp_allocator))
	k := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("K", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("LDA", context.temp_allocator))
	ldb := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("LDB", context.temp_allocator))
	ldc := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("LDC", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, i32_3, m)
	llvm.LLVMBuildStore(gen.builder, i32_3, n)
	llvm.LLVMBuildStore(gen.builder, i32_3, k)
	llvm.LLVMBuildStore(gen.builder, i32_3, lda)
	llvm.LLVMBuildStore(gen.builder, i32_3, ldb)
	llvm.LLVMBuildStore(gen.builder, i32_3, ldc)
	
	// Double constants (alpha=1.0, beta=0.0)
	alpha_val := llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 1.0)
	beta_val := llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 0.0)
	alpha := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("alpha", context.temp_allocator))
	beta := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("beta", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, alpha_val, alpha)
	llvm.LLVMBuildStore(gen.builder, beta_val, beta)
	
	// Create test matrices (9 elements each for 3x3)
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9)
	a_matrix := llvm.LLVMBuildAlloca(gen.builder, array_type, strings.clone_to_cstring("A_test", context.temp_allocator))
	b_matrix := llvm.LLVMBuildAlloca(gen.builder, array_type, strings.clone_to_cstring("B_test", context.temp_allocator))
	c_matrix := llvm.LLVMBuildAlloca(gen.builder, array_type, strings.clone_to_cstring("C_test", context.temp_allocator))
	
	// Initialize A matrix with test data: [1,2,3; 4,5,6; 7,8,9] (column-major)
	a_data := [9]f64{1,4,7, 2,5,8, 3,6,9}  // Column-major layout
	for i in 0..<9 {
		val := llvm.LLVMConstReal(double_type, a_data[i])
		zero := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0)
		idx := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)i, 0)
		indices := [2]llvm.LLVMValueRef{zero, idx}
		elem_ptr := llvm.LLVMBuildGEP2(gen.builder, array_type, a_matrix, raw_data(indices[:]), 2, strings.clone_to_cstring("a_elem", context.temp_allocator))
		llvm.LLVMBuildStore(gen.builder, val, elem_ptr)
	}
	
	// Initialize B matrix as identity: [1,0,0; 0,1,0; 0,0,1] (column-major)
	b_data := [9]f64{1,0,0, 0,1,0, 0,0,1}  // Identity matrix
	for i in 0..<9 {
		val := llvm.LLVMConstReal(double_type, b_data[i])
		zero := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0)
		idx := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)i, 0)
		indices := [2]llvm.LLVMValueRef{zero, idx}
		elem_ptr := llvm.LLVMBuildGEP2(gen.builder, array_type, b_matrix, raw_data(indices[:]), 2, strings.clone_to_cstring("b_elem", context.temp_allocator))
		llvm.LLVMBuildStore(gen.builder, val, elem_ptr)
	}
	
	// Cast arrays to double pointers for BLAS
	double_ptr_type := llvm.LLVMPointerType(double_type, 0)
	a_ptr := llvm.LLVMBuildPointerCast(gen.builder, a_matrix, double_ptr_type, strings.clone_to_cstring("A_ptr", context.temp_allocator))
	b_ptr := llvm.LLVMBuildPointerCast(gen.builder, b_matrix, double_ptr_type, strings.clone_to_cstring("B_ptr", context.temp_allocator))
	c_ptr := llvm.LLVMBuildPointerCast(gen.builder, c_matrix, double_ptr_type, strings.clone_to_cstring("C_ptr", context.temp_allocator))
	
	// Prepare arguments for dgemm call
	args := make([dynamic]llvm.LLVMValueRef, context.temp_allocator)
	append(&args, trans_a)  // TRANSA
	append(&args, trans_b)  // TRANSB
	append(&args, m)        // M
	append(&args, n)        // N
	append(&args, k)        // K
	append(&args, alpha)    // ALPHA
	append(&args, a_ptr)    // A
	append(&args, lda)      // LDA
	append(&args, b_ptr)    // B
	append(&args, ldb)      // LDB
	append(&args, beta)     // BETA
	append(&args, c_ptr)    // C
	append(&args, ldc)      // LDC
	
	// Get dgemm function type
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
	f64_ptr_type := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
	
	param_types := make([dynamic]llvm.LLVMTypeRef, context.temp_allocator)
	append(&param_types, i8_ptr_type, i8_ptr_type, i32_ptr_type, i32_ptr_type, i32_ptr_type)
	append(&param_types, f64_ptr_type, f64_ptr_type, i32_ptr_type, f64_ptr_type, i32_ptr_type)
	append(&param_types, f64_ptr_type, f64_ptr_type, i32_ptr_type)
	
	dgemm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	// Call dgemm (void function, no name needed)
	llvm.LLVMBuildCall2(gen.builder, dgemm_type, dgemm_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	fmt.printf("Generated test dgemm call with 3x3 matrices\n")
	
	// Return void (no return value)
	return nil
}
*/

// Commented out old hardcoded functions
/*
gen_freyja_gemm_call :: proc(gen: ^IRGenerator) -> llvm.LLVMValueRef {
	// Create a global array to store the result
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9)
	
	// Create global variable for result
	global_name := strings.clone_to_cstring("gemm_result", context.temp_allocator)
	result_global := llvm.LLVMAddGlobal(gen.module, array_type, global_name)
	
	// Initialize global with zeros
	zero_val := llvm.LLVMConstReal(double_type, 0.0)
	zero_vals := make([]llvm.LLVMValueRef, 9, context.temp_allocator)
	for i in 0..<9 {
		zero_vals[i] = zero_val
	}
	zero_array := llvm.LLVMConstArray(double_type, raw_data(zero_vals), 9)
	llvm.LLVMSetInitializer(result_global, zero_array)
	
	// Perform the same dgemm computation but use the global as C matrix
	gen_test_dgemm_with_result(gen, result_global)
	
	// Return pointer to the global array
	double_ptr_type := llvm.LLVMPointerType(double_type, 0)
	return llvm.LLVMBuildPointerCast(gen.builder, result_global, double_ptr_type, strings.clone_to_cstring("result_ptr", context.temp_allocator))
}

// Generate dgemm call using a specific result array
gen_test_dgemm_with_result :: proc(gen: ^IRGenerator, result_array: llvm.LLVMValueRef) {
	// Get dgemm function
	dgemm_fn := get_or_declare_dgemm(gen)
	
	// Create constants for dgemm parameters
	char_n := llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0) // 'N'
	trans_a := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans_a", context.temp_allocator))
	trans_b := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans_b", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, char_n, trans_a)
	llvm.LLVMBuildStore(gen.builder, char_n, trans_b)
	
	// Integer constants
	i32_3 := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0)
	m := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("M", context.temp_allocator))
	n := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("N", context.temp_allocator))
	k := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("K", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("LDA", context.temp_allocator))
	ldb := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("LDB", context.temp_allocator))
	ldc := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("LDC", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, i32_3, m)
	llvm.LLVMBuildStore(gen.builder, i32_3, n)
	llvm.LLVMBuildStore(gen.builder, i32_3, k)
	llvm.LLVMBuildStore(gen.builder, i32_3, lda)
	llvm.LLVMBuildStore(gen.builder, i32_3, ldb)
	llvm.LLVMBuildStore(gen.builder, i32_3, ldc)
	
	// Double constants
	alpha_val := llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 1.0)
	beta_val := llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 0.0)
	alpha := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("alpha", context.temp_allocator))
	beta := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("beta", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, alpha_val, alpha)
	llvm.LLVMBuildStore(gen.builder, beta_val, beta)
	
	// Create local test matrices
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9)
	a_matrix := llvm.LLVMBuildAlloca(gen.builder, array_type, strings.clone_to_cstring("A_local", context.temp_allocator))
	b_matrix := llvm.LLVMBuildAlloca(gen.builder, array_type, strings.clone_to_cstring("B_local", context.temp_allocator))
	
	// Initialize matrices with same test data
	a_data := [9]f64{1,4,7, 2,5,8, 3,6,9}  // Column-major
	b_data := [9]f64{1,0,0, 0,1,0, 0,0,1}  // Identity
	
	for i in 0..<9 {
		// Initialize A
		val_a := llvm.LLVMConstReal(double_type, a_data[i])
		zero := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0)
		idx := llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)i, 0)
		indices := [2]llvm.LLVMValueRef{zero, idx}
		a_elem_ptr := llvm.LLVMBuildGEP2(gen.builder, array_type, a_matrix, raw_data(indices[:]), 2, strings.clone_to_cstring("a_elem", context.temp_allocator))
		llvm.LLVMBuildStore(gen.builder, val_a, a_elem_ptr)
		
		// Initialize B
		val_b := llvm.LLVMConstReal(double_type, b_data[i])
		b_elem_ptr := llvm.LLVMBuildGEP2(gen.builder, array_type, b_matrix, raw_data(indices[:]), 2, strings.clone_to_cstring("b_elem", context.temp_allocator))
		llvm.LLVMBuildStore(gen.builder, val_b, b_elem_ptr)
	}
	
	// Cast arrays to double pointers
	double_ptr_type := llvm.LLVMPointerType(double_type, 0)
	a_ptr := llvm.LLVMBuildPointerCast(gen.builder, a_matrix, double_ptr_type, strings.clone_to_cstring("A_ptr", context.temp_allocator))
	b_ptr := llvm.LLVMBuildPointerCast(gen.builder, b_matrix, double_ptr_type, strings.clone_to_cstring("B_ptr", context.temp_allocator))
	c_ptr := llvm.LLVMBuildPointerCast(gen.builder, result_array, double_ptr_type, strings.clone_to_cstring("C_ptr", context.temp_allocator))
	
	// Prepare arguments for dgemm call
	args := make([dynamic]llvm.LLVMValueRef, context.temp_allocator)
	append(&args, trans_a, trans_b, m, n, k, alpha, a_ptr, lda, b_ptr, ldb, beta, c_ptr, ldc)
	
	// Get dgemm function type
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
	f64_ptr_type := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
	
	param_types := make([dynamic]llvm.LLVMTypeRef, context.temp_allocator)
	append(&param_types, i8_ptr_type, i8_ptr_type, i32_ptr_type, i32_ptr_type, i32_ptr_type)
	append(&param_types, f64_ptr_type, f64_ptr_type, i32_ptr_type, f64_ptr_type, i32_ptr_type)
	append(&param_types, f64_ptr_type, f64_ptr_type, i32_ptr_type)
	
	dgemm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	// Call dgemm
	llvm.LLVMBuildCall2(gen.builder, dgemm_type, dgemm_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	fmt.printf("Generated dgemm call with global result array\n")
}

// Generate test dgemm call that stores result in global array accessible from C
gen_test_dgemm_call_global :: proc(gen: ^IRGenerator) -> llvm.LLVMValueRef {
	// Create a global array for the result matrix
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9)
	
	// Create global variable for result (accessible from C)
	global_name := strings.clone_to_cstring("freyja_result_matrix", context.temp_allocator)
	result_global := llvm.LLVMAddGlobal(gen.module, array_type, global_name)
	
	// Make it externally visible (for C to access)
	llvm.LLVMSetLinkage(result_global, .ExternalLinkage)
	
	// Initialize global with zeros
	zero_val := llvm.LLVMConstReal(double_type, 0.0)
	zero_vals := make([]llvm.LLVMValueRef, 9, context.temp_allocator)
	for i in 0..<9 {
		zero_vals[i] = zero_val
	}
	zero_array := llvm.LLVMConstArray(double_type, raw_data(zero_vals), 9)
	llvm.LLVMSetInitializer(result_global, zero_array)
	
	// Same GEMM setup as before but use global result
	gen_test_dgemm_with_result(gen, result_global)
	
	fmt.printf("Generated test dgemm call with global result matrix\n")
	return nil
}
*/

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
		// Binary operations - need to check for matrix operations
		
		// Get type information for operands
		lhs_type := get_expr_type(gen.checker_info, e.left)
		rhs_type := get_expr_type(gen.checker_info, e.right)
		
		// Check for matrix multiplication
		if e.op.kind == .Mul && lhs_type != nil && rhs_type != nil {
			if lhs_type.kind == .Tensor && rhs_type.kind == .Tensor {
				fmt.printf("Generating matrix-matrix multiplication\n")
				return gen_matrix_multiply(gen, e.left, e.right, lhs_type, rhs_type)
			}
		}
		
		// Fall back to regular scalar operations
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
		
		// Check for special builtin matrix functions
		if ident.name == "mul" && len(e.args) == 3 {
			// mul(A, B, &C) - multiply A*B and store in C
			fmt.printf("Generating mul call with pre-allocated output\n")
			return gen_mul_into(gen, e.args[0], e.args[1], e.args[2])
		}
		
		// Legacy BLAS functions for compatibility
		if ident.name == "dgemm" && len(e.args) >= 2 {
			// dgemm can be called with 2 args (A, B) or 3 args (A, B, &C)
			fmt.printf("Generating dgemm call\n")
			if len(e.args) == 2 {
				return gen_tensor_matmul(gen, e.args[0], e.args[1])
			} else {
				// Use existing BLAS implementation for 3-arg version
				return gen_mul_sym_into(gen, e.args[0], e.args[1], e.args[2]) 
			}
		}
		
		if ident.name == "dgemv" && len(e.args) == 3 {
			// dgemv(A, x, &y) - matrix-vector multiply y := A*x
			fmt.printf("Generating dgemv call (matrix-vector multiply)\n")
			return gen_dgemv_into(gen, e.args[0], e.args[1], e.args[2])
		}
		
		// Tensor builtin functions (industry standard)
		if ident.name == "zeros" {
			// zeros(shape, dtype) - create zero tensor
			fmt.printf("Generating zeros tensor\n")
			return gen_tensor_zeros(gen, e.args)
		}
		
		if ident.name == "ones" {
			// ones(shape, dtype) - create ones tensor
			fmt.printf("Generating ones tensor\n")
			return gen_tensor_ones(gen, e.args)
		}
		
		if ident.name == "full" {
			// full(shape, value, dtype) - create tensor filled with value
			fmt.printf("Generating full tensor\n")
			return gen_tensor_full(gen, e.args)
		}
		
		if ident.name == "arange" {
			// arange(start, stop, step, dtype) - create range tensor
			fmt.printf("Generating arange tensor\n")
			return gen_tensor_arange(gen, e.args)
		}
		
		if ident.name == "matmul" && len(e.args) == 2 {
			// matmul(A, B) - matrix multiplication (@ operator)
			fmt.printf("Generating tensor matmul\n")
			return gen_tensor_matmul(gen, e.args[0], e.args[1])
		}
		
		if ident.name == "transpose" {
			// transpose(tensor, axes) - transpose tensor
			fmt.printf("Generating tensor transpose\n")
			return gen_tensor_transpose(gen, e.args)
		}
		
		if ident.name == "reshape" {
			// reshape(tensor, shape) - reshape tensor
			fmt.printf("Generating tensor reshape\n")
			return gen_tensor_reshape(gen, e.args)
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
	
	case ^ast.Comp_Lit:
		// Composite literal - for matrices with explicit data
		if target_type != nil && target_type.kind == .Tensor {
			// Create a matrix value with the composite literal data
			matrix_type := type_to_llvm(gen, target_type)
			
			// Allocate a temporary matrix to build the value
			temp_matrix := llvm.LLVMBuildAlloca(gen.builder, matrix_type, 
				strings.clone_to_cstring("matrix_comp_lit", context.temp_allocator))
			
			// Initialize dimensions
			if tensor_type, ok := target_type.variant.(checker.TypeTensor); ok {
				// Get dimensions from dims array (assuming 2D matrix)
				rows := tensor_type.shape[0] if len(tensor_type.shape) > 0 else 3
				cols := tensor_type.shape[1] if len(tensor_type.shape) > 1 else 3
				
				rows_val := llvm.LLVMConstInt(llvm.LLVMInt64TypeInContext(gen.ctx), cast(u64)rows, 0)
				cols_val := llvm.LLVMConstInt(llvm.LLVMInt64TypeInContext(gen.ctx), cast(u64)cols, 0)
				
				// Store dimensions (fields 1, 2, 3)
				rows_ptr := llvm.LLVMBuildStructGEP2(gen.builder, matrix_type, temp_matrix, 1,
					strings.clone_to_cstring("rows_ptr", context.temp_allocator))
				llvm.LLVMBuildStore(gen.builder, rows_val, rows_ptr)
				
				cols_ptr := llvm.LLVMBuildStructGEP2(gen.builder, matrix_type, temp_matrix, 2,
					strings.clone_to_cstring("cols_ptr", context.temp_allocator))
				llvm.LLVMBuildStore(gen.builder, cols_val, cols_ptr)
				
				ld_ptr := llvm.LLVMBuildStructGEP2(gen.builder, matrix_type, temp_matrix, 3,
					strings.clone_to_cstring("ld_ptr", context.temp_allocator))
				llvm.LLVMBuildStore(gen.builder, rows_val, ld_ptr) // ld = rows for column-major
				
				// Now extract and store the matrix data from elems
				double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
				array_type := llvm.LLVMArrayType(double_type, cast(c.uint)rows * cast(c.uint)cols)
				data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, matrix_type, temp_matrix, 0,
					strings.clone_to_cstring("data_array", context.temp_allocator))
				
				// Process the elements from the composite literal
				elem_idx := 0
				for elem in e.elems {
					// Check if this is a nested comp lit (row)
					if nested_comp, ok := elem.derived_expr.(^ast.Comp_Lit); ok {
						// Process each element in the row
						for row_elem in nested_comp.elems {
							if elem_idx < cast(int)rows * cast(int)cols {
								// Generate the value for this element
								val := gen_expr_typed(gen, row_elem, tensor_type.dtype)
								if val != nil {
									// Store in column-major order
									// For a 3x3 matrix with row-major input [r0c0, r0c1, r0c2, r1c0, r1c1, r1c2, r2c0, r2c1, r2c2]
									// We need column-major output [r0c0, r1c0, r2c0, r0c1, r1c1, r2c1, r0c2, r1c2, r2c2]
									row_idx := elem_idx / cast(int)cols
									col_idx := elem_idx % cast(int)cols
									col_major_idx := col_idx * cast(int)rows + row_idx
									
									indices := [2]llvm.LLVMValueRef{
										llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0),
										llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)col_major_idx, 0),
									}
									elem_ptr := llvm.LLVMBuildGEP2(gen.builder, array_type, data_ptr, raw_data(indices[:]), 2,
										strings.clone_to_cstring(fmt.tprintf("elem_%d", col_major_idx), context.temp_allocator))
									llvm.LLVMBuildStore(gen.builder, val, elem_ptr)
								}
								elem_idx += 1
							}
						}
					}
				}
				
				fmt.printf("    Initialized matrix from composite literal with %d elements\n", elem_idx)
			}
			
			// Load and return the complete matrix value
			return llvm.LLVMBuildLoad2(gen.builder, matrix_type, temp_matrix,
				strings.clone_to_cstring("matrix_value", context.temp_allocator))
		}
		// Default zero for other types
		return llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0)
		
	case:
		fmt.printf("Unhandled expression type in IR generation: %T\n", e)
		return nil
	}
	
	return nil
}

// Get a pointer to a matrix variable without loading it
gen_matrix_ptr :: proc(gen: ^IRGenerator, expr: ^ast.Expr) -> llvm.LLVMValueRef {
	// For identifiers, look up the alloca directly
	if ident, ok := expr.derived.(^ast.Ident); ok {
		entity := lookup_entity_by_name(gen.checker_info, ident.name)
		if entity != nil {
			if llvm_value, exists := gen.ir_symbols[entity]; exists {
				// Return the alloca pointer directly without loading
				return llvm_value
			}
		}
	}
	return nil
}

// Helper function to get the type of an expression from checker info
get_expr_type :: proc(checker_info: ^checker.CheckerInfo, expr: ^ast.Expr) -> ^checker.Type {
	// For identifiers, look up in the entity table
	if ident, ok := expr.derived.(^ast.Ident); ok {
		entity := lookup_entity_by_name(checker_info, ident.name)
		if entity != nil {
			return entity.type
		}
	}
	
	// For other expressions, would need proper expression type tracking
	// TODO: Implement full expression type tracking during checking phase
	return nil
}

// Generate mul(A, B, &C) - multiply with pre-allocated output
gen_mul_into :: proc(gen: ^IRGenerator, a_expr: ^ast.Expr, b_expr: ^ast.Expr, c_expr: ^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating mul with pre-allocated output\n")
	
	// Get types
	a_type := get_expr_type(gen.checker_info, a_expr)
	b_type := get_expr_type(gen.checker_info, b_expr)
	
	// Get matrix pointers (allocas)
	a_ptr := gen_matrix_ptr(gen, a_expr)
	b_ptr := gen_matrix_ptr(gen, b_expr)
	
	// For &C, we need to handle the unary & operator
	c_ptr: llvm.LLVMValueRef
	if unary, ok := c_expr.derived_expr.(^ast.Unary_Expr); ok && unary.op.kind == .And {
		// It's &C, get the pointer to C
		c_ptr = gen_matrix_ptr(gen, unary.expr)
	} else {
		// Direct C reference
		c_ptr = gen_matrix_ptr(gen, c_expr)
	}
	
	if a_ptr == nil || b_ptr == nil || c_ptr == nil {
		fmt.printf("Failed to get matrix pointers for multiply\n")
		return nil
	}
	
	// Get dimensions from A type (assuming square matrices for now)
	tensor_type := a_type.variant.(checker.TypeTensor)
	if len(tensor_type.shape) < 2 {
		fmt.printf("Matrix type missing dimensions\n")
		return nil
	}
	
	rows := cast(u32)tensor_type.shape[0]
	cols := cast(u32)tensor_type.shape[1]
	
	// Call BLAS dgemm to compute C = A * B
	gen_blas_dgemm_call(gen, a_ptr, b_ptr, c_ptr, rows, cols, cols)
	
	// Return void/nil since this modifies C in-place
	return nil
}

// Generate matrix multiplication using BLAS dgemm
gen_matrix_multiply :: proc(gen: ^IRGenerator, lhs_expr: ^ast.Expr, rhs_expr: ^ast.Expr, 
                           lhs_type: ^checker.Type, rhs_type: ^checker.Type) -> llvm.LLVMValueRef {
	fmt.printf("Generating matrix multiplication with BLAS dgemm\n")
	
	// Get the actual matrix variables from the expressions
	// For matrices, we need the pointers (allocas), not loaded values
	lhs_value := gen_matrix_ptr(gen, lhs_expr)
	rhs_value := gen_matrix_ptr(gen, rhs_expr)
	
	if lhs_value == nil || rhs_value == nil {
		fmt.printf("Failed to get matrix values for multiplication\n")
		return nil
	}
	
	// Get matrix dimensions from type
	tensor_type := lhs_type.variant.(checker.TypeTensor)
	if len(tensor_type.shape) < 2 {
		fmt.printf("Matrix type missing dimensions\n")
		return nil
	}
	
	rows := cast(u32)tensor_type.shape[0]
	cols := cast(u32)tensor_type.shape[1]
	
	// Create result matrix (same dimensions as input for square matrices)
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, rows * cols)
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
	
	member_types := [4]llvm.LLVMTypeRef{array_type, i64_type, i64_type, i64_type}
	mat_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, 
		raw_data(member_types[:]), 4, 0)
	
	result := llvm.LLVMBuildAlloca(gen.builder, mat_struct_type, strings.clone_to_cstring("result_matrix", context.temp_allocator))
	
	// Initialize result matrix descriptor
	rows_ptr := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, result, 1, strings.clone_to_cstring("result_rows", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)rows, 0), rows_ptr)
	
	cols_ptr := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, result, 2, strings.clone_to_cstring("result_cols", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)cols, 0), cols_ptr)
	
	ld_ptr := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, result, 3, strings.clone_to_cstring("result_ld", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)cols, 0), ld_ptr)
	
	// Call BLAS dgemm with the actual matrices
	gen_blas_dgemm_call(gen, lhs_value, rhs_value, result, rows, cols, cols)
	
	// The result matrix now contains the multiplication result
	// Load and return the complete matrix value
	return llvm.LLVMBuildLoad2(gen.builder, mat_struct_type, result,
		strings.clone_to_cstring("mult_result", context.temp_allocator))
}

// Generate a BLAS dgemm call with actual matrix variables
gen_blas_dgemm_call :: proc(gen: ^IRGenerator, a_matrix: llvm.LLVMValueRef, b_matrix: llvm.LLVMValueRef, 
                            c_matrix: llvm.LLVMValueRef, m: u32, n: u32, k: u32) {
	// Get dgemm function
	dgemm_fn := get_or_declare_dgemm(gen)
	
	// Create BLAS parameters
	char_n := llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0) // 'N' for no transpose
	trans_a := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans_a", context.temp_allocator))
	trans_b := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans_b", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, char_n, trans_a)
	llvm.LLVMBuildStore(gen.builder, char_n, trans_b)
	
	// Integer dimensions
	i32_type := llvm.LLVMInt32TypeInContext(gen.ctx)
	m_ptr := llvm.LLVMBuildAlloca(gen.builder, i32_type, strings.clone_to_cstring("M", context.temp_allocator))
	n_ptr := llvm.LLVMBuildAlloca(gen.builder, i32_type, strings.clone_to_cstring("N", context.temp_allocator))
	k_ptr := llvm.LLVMBuildAlloca(gen.builder, i32_type, strings.clone_to_cstring("K", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, i32_type, strings.clone_to_cstring("LDA", context.temp_allocator))
	ldb := llvm.LLVMBuildAlloca(gen.builder, i32_type, strings.clone_to_cstring("LDB", context.temp_allocator))
	ldc := llvm.LLVMBuildAlloca(gen.builder, i32_type, strings.clone_to_cstring("LDC", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i32_type, cast(u64)m, 0), m_ptr)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i32_type, cast(u64)n, 0), n_ptr)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i32_type, cast(u64)k, 0), k_ptr)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i32_type, cast(u64)m, 0), lda)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i32_type, cast(u64)n, 0), ldb)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i32_type, cast(u64)m, 0), ldc)
	
	// Alpha and beta
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	alpha := llvm.LLVMBuildAlloca(gen.builder, double_type, strings.clone_to_cstring("alpha", context.temp_allocator))
	beta := llvm.LLVMBuildAlloca(gen.builder, double_type, strings.clone_to_cstring("beta", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(double_type, 1.0), alpha)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(double_type, 0.0), beta)
	
	// Get pointers to matrix data (first field of struct)
	array_type := llvm.LLVMArrayType(double_type, m * n)
	i64_type_local := llvm.LLVMInt64TypeInContext(gen.ctx)
	
	member_types := [4]llvm.LLVMTypeRef{array_type, i64_type_local, i64_type_local, i64_type_local}
	mat_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, 
		raw_data(member_types[:]), 4, 0)
	
	a_data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, a_matrix, 0, strings.clone_to_cstring("a_data", context.temp_allocator))
	b_data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, b_matrix, 0, strings.clone_to_cstring("b_data", context.temp_allocator))
	c_data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, c_matrix, 0, strings.clone_to_cstring("c_data", context.temp_allocator))
	
	// Cast to double pointers
	double_ptr_type := llvm.LLVMPointerType(double_type, 0)
	a_ptr := llvm.LLVMBuildPointerCast(gen.builder, a_data_ptr, double_ptr_type, strings.clone_to_cstring("A_ptr", context.temp_allocator))
	b_ptr := llvm.LLVMBuildPointerCast(gen.builder, b_data_ptr, double_ptr_type, strings.clone_to_cstring("B_ptr", context.temp_allocator))
	c_ptr := llvm.LLVMBuildPointerCast(gen.builder, c_data_ptr, double_ptr_type, strings.clone_to_cstring("C_ptr", context.temp_allocator))
	
	// Build argument list
	args := make([dynamic]llvm.LLVMValueRef, context.temp_allocator)
	append(&args, trans_a, trans_b, m_ptr, n_ptr, k_ptr, alpha, a_ptr, lda, b_ptr, ldb, beta, c_ptr, ldc)
	
	// Get function type for dgemm
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr_type := llvm.LLVMPointerType(i32_type, 0)
	f64_ptr_type := llvm.LLVMPointerType(double_type, 0)
	
	param_types := make([dynamic]llvm.LLVMTypeRef, context.temp_allocator)
	append(&param_types, i8_ptr_type, i8_ptr_type, i32_ptr_type, i32_ptr_type, i32_ptr_type)
	append(&param_types, f64_ptr_type, f64_ptr_type, i32_ptr_type, f64_ptr_type, i32_ptr_type)
	append(&param_types, f64_ptr_type, f64_ptr_type, i32_ptr_type)
	
	dgemm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	// Call dgemm with noalias attributes
	call_inst := llvm.LLVMBuildCall2(gen.builder, dgemm_type, dgemm_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	// Add noalias attributes to matrix data pointers
	// Use string lookup to be version-safe across different LLVM versions
	noalias_str := strings.clone_to_cstring("noalias", context.temp_allocator)
	noalias_kind := llvm.LLVMGetEnumAttributeKindForName(noalias_str, cast(c.int)len("noalias"))
	
	// Create the noalias attribute (value 0 means just the flag, no value)
	noalias_attr := llvm.LLVMCreateEnumAttribute(gen.ctx, noalias_kind, 0)
	
	// Parameter indices are 1-based: trans_a(1), trans_b(2), m(3), n(4), k(5), alpha(6), 
	// A(7), lda(8), B(9), ldb(10), beta(11), C(12), ldc(13)
	llvm.LLVMAddCallSiteAttribute(call_inst, 7, noalias_attr)  // A data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 9, noalias_attr)  // B data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 12, noalias_attr) // C data pointer
	
	fmt.printf("Generated BLAS dgemm call with noalias attributes on matrix pointers\n")
}

// DSYMM: Symmetric matrix multiply C = α*A*B + β*C where A is symmetric
gen_mul_sym_into :: proc(gen: ^IRGenerator, a_expr: ^ast.Expr, b_expr: ^ast.Expr, c_expr: ^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating mul_sym with pre-allocated output\n")
	
	// Generate matrix values
	a_matrix := gen_matrix_ptr(gen, a_expr)
	b_matrix := gen_matrix_ptr(gen, b_expr)
	c_addr := gen_matrix_ptr(gen, c_expr)
	
	if a_matrix == nil || b_matrix == nil || c_addr == nil {
		fmt.printf("Failed to generate matrix expressions for mul_sym\n")
		return nil
	}
	
	// Get or declare DSYMM function
	dsymm_name := strings.clone_to_cstring("dsymm_", context.temp_allocator)
	dsymm_fn := llvm.LLVMGetNamedFunction(gen.module, dsymm_name)
	if dsymm_fn == nil {
		fmt.printf("Pre-declaring BLAS dsymm function\n")
		void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
		i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
		i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
		f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
		param_types := []llvm.LLVMTypeRef{i8_ptr, i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr}
		dsymm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
		dsymm_fn = llvm.LLVMAddFunction(gen.module, dsymm_name, dsymm_type)
	}
	
	// DSYMM parameters: dsymm_(side, uplo, m, n, alpha, A, lda, B, ldb, beta, C, ldc)
	// We assume left side ('L') and upper triangular ('U') storage
	
	// Allocate and set parameters
	side := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("side", context.temp_allocator))
	uplo := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("uplo", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 76, 0), side) // 'L'
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 85, 0), uplo) // 'U'
	
	// Matrix dimensions (assuming 3x3 for now)
	m := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("m", context.temp_allocator))
	n := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("n", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("lda", context.temp_allocator))
	ldb := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("ldb", context.temp_allocator))
	ldc := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("ldc", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), m)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), n)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), lda)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), ldb)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), ldc)
	
	// Alpha and beta scalars
	alpha := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("alpha", context.temp_allocator))
	beta := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("beta", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 1.0), alpha)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 0.0), beta)
	
	// Extract matrix data pointers
	// Create matrix struct type: {[9]f64, i64, i64, i64} for data, rows, cols, ld
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9) // 3x3 matrix
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
	member_types := [4]llvm.LLVMTypeRef{array_type, i64_type, i64_type, i64_type}
	mat_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, raw_data(member_types[:]), 4, 0)
	
	a_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, a_matrix, 0, 
		strings.clone_to_cstring("a_data", context.temp_allocator))
	b_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, b_matrix, 0,
		strings.clone_to_cstring("b_data", context.temp_allocator))
	c_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, c_addr, 0,
		strings.clone_to_cstring("c_data", context.temp_allocator))
	
	// Build argument array
	args := []llvm.LLVMValueRef{side, uplo, m, n, alpha, a_data, lda, b_data, ldb, beta, c_data, ldc}
	
	// Call DSYMM
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
	f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
	param_types := []llvm.LLVMTypeRef{i8_ptr, i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr}
	dsymm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	call_inst := llvm.LLVMBuildCall2(gen.builder, dsymm_type, dsymm_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	// Add noalias attributes
	noalias_str := strings.clone_to_cstring("noalias", context.temp_allocator)
	noalias_kind := llvm.LLVMGetEnumAttributeKindForName(noalias_str, cast(c.int)len("noalias"))
	noalias_attr := llvm.LLVMCreateEnumAttribute(gen.ctx, noalias_kind, 0)
	
	llvm.LLVMAddCallSiteAttribute(call_inst, 6, noalias_attr)  // A data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 8, noalias_attr)  // B data pointer  
	llvm.LLVMAddCallSiteAttribute(call_inst, 11, noalias_attr) // C data pointer
	
	fmt.printf("Generated BLAS dsymm call with noalias attributes\n")
	return nil
}

// DTRMM: Triangular matrix multiply B = α*op(A)*B where A is triangular
gen_mul_tri_into :: proc(gen: ^IRGenerator, a_expr: ^ast.Expr, b_expr: ^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating mul_tri (in-place triangular multiply)\n")
	
	// Generate matrix values
	a_matrix := gen_matrix_ptr(gen, a_expr)
	b_addr := gen_matrix_ptr(gen, b_expr)
	
	if a_matrix == nil || b_addr == nil {
		fmt.printf("Failed to generate matrix expressions for mul_tri\n")
		return nil
	}
	
	// Get or declare DTRMM function
	dtrmm_name := strings.clone_to_cstring("dtrmm_", context.temp_allocator)
	dtrmm_fn := llvm.LLVMGetNamedFunction(gen.module, dtrmm_name)
	if dtrmm_fn == nil {
		fmt.printf("Pre-declaring BLAS dtrmm function\n")
		void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
		i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
		i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
		f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
		param_types := []llvm.LLVMTypeRef{i8_ptr, i8_ptr, i8_ptr, i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr}
		dtrmm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
		dtrmm_fn = llvm.LLVMAddFunction(gen.module, dtrmm_name, dtrmm_type)
	}
	
	// DTRMM parameters: dtrmm_(side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb)
	// Assume: left side, upper triangular, no transpose, non-unit diagonal
	
	side := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("side", context.temp_allocator))
	uplo := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("uplo", context.temp_allocator))
	transa := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("transa", context.temp_allocator))
	diag := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("diag", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 76, 0), side)    // 'L'
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 85, 0), uplo)    // 'U'  
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0), transa)  // 'N'
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0), diag)    // 'N'
	
	// Matrix dimensions
	m := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("m", context.temp_allocator))
	n := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("n", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("lda", context.temp_allocator))
	ldb := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("ldb", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), m)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), n)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), lda)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), ldb)
	
	// Alpha scalar
	alpha := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("alpha", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 1.0), alpha)
	
	// Extract matrix data pointers  
	// Create matrix struct type: {[9]f64, i64, i64, i64} for data, rows, cols, ld
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9) // 3x3 matrix
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
	member_types := [4]llvm.LLVMTypeRef{array_type, i64_type, i64_type, i64_type}
	mat_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, raw_data(member_types[:]), 4, 0)
	
	a_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, a_matrix, 0,
		strings.clone_to_cstring("a_data", context.temp_allocator))
	b_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, b_addr, 0,
		strings.clone_to_cstring("b_data", context.temp_allocator))
	
	// Build argument array
	args := []llvm.LLVMValueRef{side, uplo, transa, diag, m, n, alpha, a_data, lda, b_data, ldb}
	
	// Call DTRMM
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
	f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
	param_types := []llvm.LLVMTypeRef{i8_ptr, i8_ptr, i8_ptr, i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr}
	dtrmm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	call_inst := llvm.LLVMBuildCall2(gen.builder, dtrmm_type, dtrmm_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	// Add noalias attributes
	noalias_str := strings.clone_to_cstring("noalias", context.temp_allocator)
	noalias_kind := llvm.LLVMGetEnumAttributeKindForName(noalias_str, cast(c.int)len("noalias"))
	noalias_attr := llvm.LLVMCreateEnumAttribute(gen.ctx, noalias_kind, 0)
	
	llvm.LLVMAddCallSiteAttribute(call_inst, 8, noalias_attr)  // A data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 10, noalias_attr) // B data pointer
	
	fmt.printf("Generated BLAS dtrmm call with noalias attributes\n")
	return nil
}

// DTRSM: Triangular solve B = α*op(A)^(-1)*B where A is triangular
gen_tri_solve_into :: proc(gen: ^IRGenerator, a_expr: ^ast.Expr, b_expr: ^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating tri_solve (triangular solve)\n")
	
	// Generate matrix values
	a_matrix := gen_matrix_ptr(gen, a_expr)
	b_addr := gen_matrix_ptr(gen, b_expr)
	
	if a_matrix == nil || b_addr == nil {
		fmt.printf("Failed to generate matrix expressions for tri_solve\n")
		return nil
	}
	
	// Get or declare DTRSM function
	dtrsm_name := strings.clone_to_cstring("dtrsm_", context.temp_allocator)
	dtrsm_fn := llvm.LLVMGetNamedFunction(gen.module, dtrsm_name)
	if dtrsm_fn == nil {
		fmt.printf("Pre-declaring BLAS dtrsm function\n")
		void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
		i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
		i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
		f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
		param_types := []llvm.LLVMTypeRef{i8_ptr, i8_ptr, i8_ptr, i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr}
		dtrsm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
		dtrsm_fn = llvm.LLVMAddFunction(gen.module, dtrsm_name, dtrsm_type)
	}
	
	// DTRSM parameters: dtrsm_(side, uplo, transa, diag, m, n, alpha, A, lda, B, ldb)
	// Same parameter structure as DTRMM
	
	side := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("side", context.temp_allocator))
	uplo := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("uplo", context.temp_allocator))
	transa := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("transa", context.temp_allocator))
	diag := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("diag", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 76, 0), side)    // 'L'
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 85, 0), uplo)    // 'U'
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0), transa)  // 'N'
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0), diag)    // 'N'
	
	// Matrix dimensions
	m := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("m", context.temp_allocator))
	n := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("n", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("lda", context.temp_allocator))
	ldb := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("ldb", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), m)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), n)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), lda)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), ldb)
	
	// Alpha scalar
	alpha := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("alpha", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 1.0), alpha)
	
	// Extract matrix data pointers
	// Create matrix struct type: {[9]f64, i64, i64, i64} for data, rows, cols, ld
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9) // 3x3 matrix
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
	member_types := [4]llvm.LLVMTypeRef{array_type, i64_type, i64_type, i64_type}
	mat_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, raw_data(member_types[:]), 4, 0)
	
	a_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, a_matrix, 0,
		strings.clone_to_cstring("a_data", context.temp_allocator))
	b_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, b_addr, 0,
		strings.clone_to_cstring("b_data", context.temp_allocator))
	
	// Build argument array
	args := []llvm.LLVMValueRef{side, uplo, transa, diag, m, n, alpha, a_data, lda, b_data, ldb}
	
	// Call DTRSM
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
	f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
	param_types := []llvm.LLVMTypeRef{i8_ptr, i8_ptr, i8_ptr, i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr}
	dtrsm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	call_inst := llvm.LLVMBuildCall2(gen.builder, dtrsm_type, dtrsm_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	// Add noalias attributes
	noalias_str := strings.clone_to_cstring("noalias", context.temp_allocator)
	noalias_kind := llvm.LLVMGetEnumAttributeKindForName(noalias_str, cast(c.int)len("noalias"))
	noalias_attr := llvm.LLVMCreateEnumAttribute(gen.ctx, noalias_kind, 0)
	
	llvm.LLVMAddCallSiteAttribute(call_inst, 8, noalias_attr)  // A data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 10, noalias_attr) // B data pointer
	
	fmt.printf("Generated BLAS dtrsm call with noalias attributes\n")
	return nil
}

// DGEMV: Matrix-vector multiply y := alpha*A*x + beta*y
gen_dgemv_into :: proc(gen: ^IRGenerator, a_expr: ^ast.Expr, x_expr: ^ast.Expr, y_expr: ^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating dgemv (matrix-vector multiply)\n")
	
	// Get matrix and vector pointers
	a_matrix := gen_matrix_ptr(gen, a_expr)
	x_vector := gen_matrix_ptr(gen, x_expr)  // Vectors are 1D matrices
	
	// For &y, handle the unary & operator
	y_ptr: llvm.LLVMValueRef
	if unary, ok := y_expr.derived_expr.(^ast.Unary_Expr); ok && unary.op.kind == .And {
		// It's &y, get the pointer to y
		y_ptr = gen_matrix_ptr(gen, unary.expr)
	} else {
		// Direct y reference
		y_ptr = gen_matrix_ptr(gen, y_expr)
	}
	
	if a_matrix == nil || x_vector == nil || y_ptr == nil {
		fmt.printf("Failed to get matrix/vector pointers for dgemv\n")
		return nil
	}
	
	// Get or declare DGEMV function
	dgemv_name := strings.clone_to_cstring("dgemv_", context.temp_allocator)
	dgemv_fn := llvm.LLVMGetNamedFunction(gen.module, dgemv_name)
	if dgemv_fn == nil {
		fmt.printf("Pre-declaring BLAS dgemv function\n")
		void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
		i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
		i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
		f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
		// dgemv_(trans, m, n, alpha, A, lda, x, incx, beta, y, incy)
		param_types := []llvm.LLVMTypeRef{i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr}
		dgemv_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
		dgemv_fn = llvm.LLVMAddFunction(gen.module, dgemv_name, dgemv_type)
	}
	
	// DGEMV parameters: dgemv_(trans, m, n, alpha, A, lda, x, incx, beta, y, incy)
	// trans='N' (no transpose), alpha=1.0, beta=0.0
	
	trans := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt8TypeInContext(gen.ctx), strings.clone_to_cstring("trans", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt8TypeInContext(gen.ctx), 78, 0), trans) // 'N'
	
	// Matrix dimensions (assuming 3x3 matrix and 3x1 vector for now)
	m := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("m", context.temp_allocator))
	n := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("n", context.temp_allocator))
	lda := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("lda", context.temp_allocator))
	incx := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("incx", context.temp_allocator))
	incy := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMInt32TypeInContext(gen.ctx), strings.clone_to_cstring("incy", context.temp_allocator))
	
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), m)    // rows
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), n)    // cols
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 3, 0), lda)  // leading dimension
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 1, 0), incx) // x stride
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 1, 0), incy) // y stride
	
	// Alpha and beta scalars
	alpha := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("alpha", context.temp_allocator))
	beta := llvm.LLVMBuildAlloca(gen.builder, llvm.LLVMDoubleTypeInContext(gen.ctx), strings.clone_to_cstring("beta", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 1.0), alpha)
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstReal(llvm.LLVMDoubleTypeInContext(gen.ctx), 0.0), beta)
	
	// Extract data pointers
	// Create matrix struct type for extracting data
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(double_type, 9) // 3x3 matrix
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
	member_types := [4]llvm.LLVMTypeRef{array_type, i64_type, i64_type, i64_type}
	mat_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, raw_data(member_types[:]), 4, 0)
	
	// For vectors, use a simpler array type
	vec_array_type := llvm.LLVMArrayType(double_type, 3) // 3-element vector
	vec_member_types := [4]llvm.LLVMTypeRef{vec_array_type, i64_type, i64_type, i64_type}
	vec_struct_type := llvm.LLVMStructTypeInContext(gen.ctx, raw_data(vec_member_types[:]), 4, 0)
	
	a_data := llvm.LLVMBuildStructGEP2(gen.builder, mat_struct_type, a_matrix, 0,
		strings.clone_to_cstring("a_data", context.temp_allocator))
	x_data := llvm.LLVMBuildStructGEP2(gen.builder, vec_struct_type, x_vector, 0,
		strings.clone_to_cstring("x_data", context.temp_allocator))
	y_data := llvm.LLVMBuildStructGEP2(gen.builder, vec_struct_type, y_ptr, 0,
		strings.clone_to_cstring("y_data", context.temp_allocator))
	
	// Cast to double pointers
	double_ptr_type := llvm.LLVMPointerType(double_type, 0)
	a_ptr := llvm.LLVMBuildPointerCast(gen.builder, a_data, double_ptr_type, strings.clone_to_cstring("A_ptr", context.temp_allocator))
	x_ptr := llvm.LLVMBuildPointerCast(gen.builder, x_data, double_ptr_type, strings.clone_to_cstring("x_ptr", context.temp_allocator))
	y_ptr_cast := llvm.LLVMBuildPointerCast(gen.builder, y_data, double_ptr_type, strings.clone_to_cstring("y_ptr", context.temp_allocator))
	
	// Build argument array
	args := []llvm.LLVMValueRef{trans, m, n, alpha, a_ptr, lda, x_ptr, incx, beta, y_ptr_cast, incy}
	
	// Call DGEMV
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	i32_ptr := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0)
	f64_ptr := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0)
	param_types := []llvm.LLVMTypeRef{i8_ptr, i32_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr, f64_ptr, i32_ptr, f64_ptr, f64_ptr, i32_ptr}
	dgemv_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	
	call_inst := llvm.LLVMBuildCall2(gen.builder, dgemv_type, dgemv_fn, raw_data(args), cast(c.uint)len(args), strings.clone_to_cstring("", context.temp_allocator))
	
	// Add noalias attributes
	noalias_str := strings.clone_to_cstring("noalias", context.temp_allocator)
	noalias_kind := llvm.LLVMGetEnumAttributeKindForName(noalias_str, cast(c.int)len("noalias"))
	noalias_attr := llvm.LLVMCreateEnumAttribute(gen.ctx, noalias_kind, 0)
	
	llvm.LLVMAddCallSiteAttribute(call_inst, 5, noalias_attr)  // A data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 7, noalias_attr)  // x data pointer
	llvm.LLVMAddCallSiteAttribute(call_inst, 10, noalias_attr) // y data pointer
	
	fmt.printf("Generated BLAS dgemv call with noalias attributes\n")
	return nil
}