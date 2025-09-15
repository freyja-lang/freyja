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
	// Use freyja_main instead of main to avoid conflicts when linking
	actual_name := entity.name
	if entity.name == "main" {
		actual_name = "freyja_main"
	}
	func_name := strings.clone_to_cstring(actual_name, context.temp_allocator)
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

// Get the LLVM type for an Odin slice: {ptr, len}
get_slice_type :: proc(gen: ^IRGenerator, elem_type: llvm.LLVMTypeRef) -> llvm.LLVMTypeRef {
	ptr_type := llvm.LLVMPointerType(elem_type, 0)
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)

	member_types := [2]llvm.LLVMTypeRef{ptr_type, i64_type}
	return llvm.LLVMStructTypeInContext(gen.ctx, raw_data(member_types[:]), 2, 0)
}

// Get the LLVM type for a flexible n-rank tensor descriptor
// Structure: { data: []f64, shape: []i64, strides: []i64 } (using Odin slices)
get_tensor_descriptor_type :: proc(gen: ^IRGenerator) -> llvm.LLVMTypeRef {
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)

	// Each slice is a {ptr, len} pair
	data_slice_type := get_slice_type(gen, double_type)    // []f64
	shape_slice_type := get_slice_type(gen, i64_type)      // []i64
	strides_slice_type := get_slice_type(gen, i64_type)    // []i64

	// Tensor descriptor using Odin slices
	member_types := [3]llvm.LLVMTypeRef{
		data_slice_type,     // data: []f64
		shape_slice_type,    // shape: []i64
		strides_slice_type,  // strides: []i64
	}

	return llvm.LLVMStructTypeInContext(gen.ctx, raw_data(member_types[:]), 3, 0)
}

// Create a tensor descriptor struct from a parameter array
create_tensor_struct_from_param :: proc(gen: ^IRGenerator, param_array: llvm.LLVMValueRef,
                                        tensor_type: ^checker.Type) -> llvm.LLVMValueRef {
	tensor := tensor_type.variant.(checker.TypeTensor)
	i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
	double_type := llvm.LLVMDoubleTypeInContext(gen.ctx)

	// Get the flexible tensor descriptor type
	descriptor_type := get_tensor_descriptor_type(gen)

	// Allocate the descriptor
	descriptor := llvm.LLVMBuildAlloca(gen.builder, descriptor_type,
		strings.clone_to_cstring("tensor_descriptor", context.temp_allocator))

	// Create shape slice
	shape_array_type := llvm.LLVMArrayType(i64_type, cast(u32)tensor.rank)
	shape_array := llvm.LLVMBuildAlloca(gen.builder, shape_array_type,
		strings.clone_to_cstring("shape_array", context.temp_allocator))

	for i in 0..<tensor.rank {
		idx := [2]llvm.LLVMValueRef{
			llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0),
			llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)i, 0),
		}
		elem_ptr := llvm.LLVMBuildGEP2(gen.builder, shape_array_type, shape_array,
			raw_data(idx[:]), 2, strings.clone_to_cstring("shape_elem", context.temp_allocator))
		dim_size := tensor.shape[i] if cast(int)i < len(tensor.shape) else 1
		llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)dim_size, 0), elem_ptr)
	}

	// Build shape slice {ptr, len}
	shape_slice_type := get_slice_type(gen, i64_type)
	shape_slice_ptr := llvm.LLVMBuildStructGEP2(gen.builder, descriptor_type, descriptor, 1,
		strings.clone_to_cstring("shape_slice", context.temp_allocator))

	i64_ptr_type := llvm.LLVMPointerType(i64_type, 0)
	shape_data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, shape_slice_type, shape_slice_ptr, 0,
		strings.clone_to_cstring("shape_data", context.temp_allocator))
	shape_cast := llvm.LLVMBuildPointerCast(gen.builder, shape_array, i64_ptr_type,
		strings.clone_to_cstring("shape_cast", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, shape_cast, shape_data_ptr)

	shape_len_ptr := llvm.LLVMBuildStructGEP2(gen.builder, shape_slice_type, shape_slice_ptr, 1,
		strings.clone_to_cstring("shape_len", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)tensor.rank, 0), shape_len_ptr)

	// Create strides slice (assume contiguous row-major)
	strides_array := llvm.LLVMBuildAlloca(gen.builder, shape_array_type,
		strings.clone_to_cstring("strides_array", context.temp_allocator))

	stride := i64(1)
	for i := tensor.rank - 1; i >= 0; i -= 1 {
		idx := [2]llvm.LLVMValueRef{
			llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), 0, 0),
			llvm.LLVMConstInt(llvm.LLVMInt32TypeInContext(gen.ctx), cast(u64)i, 0),
		}
		elem_ptr := llvm.LLVMBuildGEP2(gen.builder, shape_array_type, strides_array,
			raw_data(idx[:]), 2, strings.clone_to_cstring("stride_elem", context.temp_allocator))
		llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)stride, 0), elem_ptr)

		dim_size := tensor.shape[i] if cast(int)i < len(tensor.shape) else 1
		stride *= dim_size
	}

	// Build strides slice {ptr, len}
	strides_slice_type := get_slice_type(gen, i64_type)
	strides_slice_ptr := llvm.LLVMBuildStructGEP2(gen.builder, descriptor_type, descriptor, 2,
		strings.clone_to_cstring("strides_slice", context.temp_allocator))

	strides_data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, strides_slice_type, strides_slice_ptr, 0,
		strings.clone_to_cstring("strides_data", context.temp_allocator))
	strides_cast := llvm.LLVMBuildPointerCast(gen.builder, strides_array, i64_ptr_type,
		strings.clone_to_cstring("strides_cast", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, strides_cast, strides_data_ptr)

	strides_len_ptr := llvm.LLVMBuildStructGEP2(gen.builder, strides_slice_type, strides_slice_ptr, 1,
		strings.clone_to_cstring("strides_len", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)tensor.rank, 0), strides_len_ptr)

	// Create data slice {ptr, len}
	total_elements := 1
	for dim in tensor.shape {
		if dim > 0 do total_elements *= cast(int)dim
	}

	// Store the parameter array
	array_type := llvm.LLVMArrayType(double_type, cast(u32)total_elements)
	data_storage := llvm.LLVMBuildAlloca(gen.builder, array_type,
		strings.clone_to_cstring("data_storage", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, param_array, data_storage)

	// Build data slice {ptr, len}
	data_slice_type := get_slice_type(gen, double_type)
	data_slice_ptr := llvm.LLVMBuildStructGEP2(gen.builder, descriptor_type, descriptor, 0,
		strings.clone_to_cstring("data_slice", context.temp_allocator))

	double_ptr_type := llvm.LLVMPointerType(double_type, 0)
	data_data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, data_slice_type, data_slice_ptr, 0,
		strings.clone_to_cstring("data_data", context.temp_allocator))
	data_cast := llvm.LLVMBuildPointerCast(gen.builder, data_storage, double_ptr_type,
		strings.clone_to_cstring("data_cast", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, data_cast, data_data_ptr)

	data_len_ptr := llvm.LLVMBuildStructGEP2(gen.builder, data_slice_type, data_slice_ptr, 1,
		strings.clone_to_cstring("data_len", context.temp_allocator))
	llvm.LLVMBuildStore(gen.builder, llvm.LLVMConstInt(i64_type, cast(u64)total_elements, 0), data_len_ptr)

	return descriptor
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

						// Check if this is a tensor parameter that needs conversion
						if param_entity.type != nil && param_entity.type.kind == .Tensor {
							// Create tensor descriptor struct for this parameter
							tensor_struct := create_tensor_struct_from_param(gen, param_value, param_entity.type)
							gen.ir_symbols[param_entity] = tensor_struct
							fmt.printf("    Mapped tensor parameter %s to descriptor\n", ident.name)
						} else {
							// Store the parameter value directly in the symbol table
							gen.ir_symbols[param_entity] = param_value
							fmt.printf("    Mapped parameter %s to LLVM arg %d\n", ident.name, param_index)
						}
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