package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "../../ast"
import "core:c"

// ========== Tensor Operation IR Generation (Industry Standard) ==========

// Generate zeros tensor: zeros(shape, dtype)
gen_tensor_zeros :: proc(gen: ^IRGenerator, args: []^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating zeros tensor IR\n")
	
	// For now, create a simple allocation and memset to zero
	// TODO: Parse shape and dtype arguments properly
	// This is a placeholder implementation
	
	f32_type := llvm.LLVMFloatTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(f32_type, 10) // Fixed size for demo
	
	// Allocate memory for tensor data
	tensor_alloca := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("zeros_tensor", context.temp_allocator))
	
	// For now, just create a zero-initialized constant array
	f32_zero := llvm.LLVMConstReal(f32_type, 0.0)
	zeros_array_data: [10]llvm.LLVMValueRef
	for i in 0..<10 {
		zeros_array_data[i] = f32_zero
	}
	
	zeros_array := llvm.LLVMConstArray(f32_type, raw_data(zeros_array_data[:]), 10)
	llvm.LLVMBuildStore(gen.builder, zeros_array, tensor_alloca)
	
	fmt.printf("Generated zeros tensor with memset\n")
	return tensor_alloca
}

// Generate ones tensor: ones(shape, dtype)
gen_tensor_ones :: proc(gen: ^IRGenerator, args: []^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating ones tensor IR\n")
	
	f32_type := llvm.LLVMFloatTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(f32_type, 10) // Fixed size for demo
	one_val := llvm.LLVMConstReal(f32_type, 1.0)
	
	// Create array of ones
	ones_array_data: [10]llvm.LLVMValueRef
	for i in 0..<10 {
		ones_array_data[i] = one_val
	}
	
	ones_array := llvm.LLVMConstArray(f32_type, raw_data(ones_array_data[:]), 10)
	
	// Allocate and initialize
	tensor_alloca := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("ones_tensor", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, ones_array, tensor_alloca)
	
	fmt.printf("Generated ones tensor with constant array\n")
	return tensor_alloca
}

// Generate full tensor: full(shape, value, dtype)
gen_tensor_full :: proc(gen: ^IRGenerator, args: []^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating full tensor IR\n")
	// TODO: Extract value from args[1] and use it to fill tensor
	// For now, create a tensor filled with 42.0
	
	f32_type := llvm.LLVMFloatTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(f32_type, 10)
	fill_val := llvm.LLVMConstReal(f32_type, 42.0)
	
	// Create array filled with value
	fill_array_data: [10]llvm.LLVMValueRef
	for i in 0..<10 {
		fill_array_data[i] = fill_val
	}
	
	fill_array := llvm.LLVMConstArray(f32_type, raw_data(fill_array_data[:]), 10)
	
	tensor_alloca := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("full_tensor", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, fill_array, tensor_alloca)
	
	fmt.printf("Generated full tensor with fill value\n")
	return tensor_alloca
}

// Generate arange tensor: arange(start, stop, step, dtype)
gen_tensor_arange :: proc(gen: ^IRGenerator, args: []^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating arange tensor IR\n")
	// TODO: Parse start, stop, step from args
	// For now, create [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	
	f32_type := llvm.LLVMFloatTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(f32_type, 10)
	
	// Create sequential array
	arange_array_data: [10]llvm.LLVMValueRef
	for i in 0..<10 {
		arange_array_data[i] = llvm.LLVMConstReal(f32_type, f64(i))
	}
	
	arange_array := llvm.LLVMConstArray(f32_type, raw_data(arange_array_data[:]), 10)
	
	tensor_alloca := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("arange_tensor", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, arange_array, tensor_alloca)
	
	fmt.printf("Generated arange tensor [0..9]\n")
	return tensor_alloca
}

// Generate tensor matrix multiplication: matmul(A, B)
gen_tensor_matmul :: proc(gen: ^IRGenerator, a_expr: ^ast.Expr, b_expr: ^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating tensor matmul IR\n")
	
	// For tensors, we need to dispatch based on rank:
	// - rank 1 × rank 1 → dot product (scalar)
	// - rank 2 × rank 1 → matrix-vector multiply (DGEMV)
	// - rank 1 × rank 2 → vector-matrix multiply
	// - rank 2 × rank 2 → matrix-matrix multiply (DGEMM)
	// - higher ranks → batched operations or tensor contractions
	
	// For now, assume 2D × 2D matrix multiplication using DGEMM
	a_matrix := gen_matrix_ptr(gen, a_expr)
	b_matrix := gen_matrix_ptr(gen, b_expr)
	
	if a_matrix == nil || b_matrix == nil {
		fmt.printf("Failed to generate tensor expressions for matmul\n")
		return nil
	}
	
	// Create result tensor allocation
	// TODO: Infer result shape from input tensor shapes
	f64_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	result_array_type := llvm.LLVMArrayType(f64_type, 9) // 3x3 result
	result_alloca := llvm.LLVMBuildAlloca(gen.builder, result_array_type,
		strings.clone_to_cstring("matmul_result", context.temp_allocator))
	
	// Use BLAS DGEMM for the computation
	gen_blas_dgemm_call(gen, a_matrix, b_matrix, result_alloca, 3, 3, 3)
	
	fmt.printf("Generated tensor matmul using DGEMM\n")
	return result_alloca
}

// Generate tensor transpose: transpose(tensor, axes)
gen_tensor_transpose :: proc(gen: ^IRGenerator, args: []^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating tensor transpose IR\n")
	// TODO: Parse axes argument to determine which dimensions to transpose
	// For now, assume simple 2D matrix transpose
	
	if len(args) < 1 {
		fmt.printf("transpose requires at least tensor argument\n")
		return nil
	}
	
	// For a 2D transpose, we need to swap rows and columns
	// This requires generating loops to copy data with swapped indices
	// For now, return a placeholder allocation
	
	f32_type := llvm.LLVMFloatTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(f32_type, 9) // 3x3 matrix
	
	result_alloca := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("transpose_result", context.temp_allocator))
	
	fmt.printf("Generated tensor transpose placeholder\n")
	return result_alloca
}

// Generate tensor reshape: reshape(tensor, shape)
gen_tensor_reshape :: proc(gen: ^IRGenerator, args: []^ast.Expr) -> llvm.LLVMValueRef {
	fmt.printf("Generating tensor reshape IR\n")
	// Reshape doesn't change data, just the view/strides
	// For now, return the original tensor (no-op)
	
	if len(args) < 2 {
		fmt.printf("reshape requires tensor and shape arguments\n")
		return nil
	}
	
	// For now, just return a placeholder tensor
	f32_type := llvm.LLVMFloatTypeInContext(gen.ctx)
	array_type := llvm.LLVMArrayType(f32_type, 10)
	
	result_alloca := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("reshape_result", context.temp_allocator))
	
	fmt.printf("Generated tensor reshape (placeholder)\n")
	return result_alloca
}

// TODO: Add proper tensor IR generation with runtime shape support
// For now, these are placeholder implementations with fixed sizes