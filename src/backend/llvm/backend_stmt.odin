package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:strings"
import "../../ast"
import "core:c"

// Statement generation (like Odin's llvm_backend_stmt.cpp)

// Get or declare malloc function
get_or_declare_malloc :: proc(gen: ^IRGenerator) -> llvm.LLVMValueRef {
	malloc_name := strings.clone_to_cstring("malloc", context.temp_allocator)
	
	// Check if already declared
	if malloc_fn := llvm.LLVMGetNamedFunction(gen.module, malloc_name); malloc_fn != nil {
		return malloc_fn
	}
	
	// Declare malloc: void* malloc(size_t size)
	i8_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
	size_t_type := llvm.LLVMInt64TypeInContext(gen.ctx) // Assume 64-bit size_t
	
	malloc_type := llvm.LLVMFunctionType(i8_ptr_type, &size_t_type, 1, 0)
	malloc_fn := llvm.LLVMAddFunction(gen.module, malloc_name, malloc_type)
	
	return malloc_fn
}

// Get or declare BLAS dgemm_ function
get_or_declare_dgemm :: proc(gen: ^IRGenerator) -> llvm.LLVMValueRef {
	dgemm_name := strings.clone_to_cstring("dgemm_", context.temp_allocator)
	
	// Check if already declared
	if dgemm_fn := llvm.LLVMGetNamedFunction(gen.module, dgemm_name); dgemm_fn != nil {
		return dgemm_fn
	}
	
	// Declare dgemm_: void dgemm_(char*, char*, int*, int*, int*, double*, double*, int*, double*, int*, double*, double*, int*)
	// Parameters: TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC
	void_type := llvm.LLVMVoidTypeInContext(gen.ctx)
	i8_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)  // char*
	i32_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt32TypeInContext(gen.ctx), 0) // int*
	f64_ptr_type := llvm.LLVMPointerType(llvm.LLVMDoubleTypeInContext(gen.ctx), 0) // double*
	
	param_types := make([dynamic]llvm.LLVMTypeRef, context.temp_allocator)
	append(&param_types, i8_ptr_type)  // TRANSA
	append(&param_types, i8_ptr_type)  // TRANSB
	append(&param_types, i32_ptr_type) // M
	append(&param_types, i32_ptr_type) // N
	append(&param_types, i32_ptr_type) // K
	append(&param_types, f64_ptr_type) // ALPHA
	append(&param_types, f64_ptr_type) // A
	append(&param_types, i32_ptr_type) // LDA
	append(&param_types, f64_ptr_type) // B
	append(&param_types, i32_ptr_type) // LDB
	append(&param_types, f64_ptr_type) // BETA
	append(&param_types, f64_ptr_type) // C
	append(&param_types, i32_ptr_type) // LDC
	
	dgemm_type := llvm.LLVMFunctionType(void_type, raw_data(param_types), cast(c.uint)len(param_types), 0)
	dgemm_fn := llvm.LLVMAddFunction(gen.module, dgemm_name, dgemm_type)
	
	return dgemm_fn
}

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
		
		// Get value expression if available (may be nil for type-only declarations)
		value_expr: ^ast.Expr
		if i < len(decl.values) {
			value_expr = decl.values[i]
		}
		
		// Get the variable name and entity (Odin's pattern)
		name: string
		entity: ^checker.Entity
		if ident, ok := name_expr.derived_expr.(^ast.Ident); ok {
			name = ident.name
			entity = cast(^checker.Entity)ident.entity
		} else {
			fmt.printf("Declaration name is not an identifier\n")
			return false
		}
		
		if entity == nil {
			fmt.printf("No entity reference found for variable '%s'\n", name)
			return false
		}
		
		fmt.printf("    Converting type for variable '%s', type_kind=%v\n", name, entity.type != nil ? entity.type.kind : checker.TypeKind.Invalid)
		var_type := type_to_llvm(gen, entity.type)
		fmt.printf("    Successfully converted type for variable '%s'\n", name)
		
		// Allocate stack space for the variable
		fmt.printf("    Creating alloca for variable '%s'\n", name)
		var_name := strings.clone_to_cstring(name, context.temp_allocator)
		alloca := llvm.LLVMBuildAlloca(gen.builder, var_type, var_name)
		fmt.printf("    Created alloca for variable '%s'\n", name)
		
		// TODO: Handle tensor descriptor initialization (disabled during Matrix->Tensor transition)
		/*
		if entity.type != nil && entity.type.kind == .Tensor {
			tensor_type := entity.type.variant.(checker.TypeTensor)
			
			// Initialize dimensions in the descriptor
			if tensor_type.rank >= 2 && len(tensor_type.shape) >= 2 {
				rows := cast(u64)tensor_type.shape[0]
				cols := cast(u64)tensor_type.shape[1]
				ld := cols // leading dimension = cols for column-major
				
				// Get element pointers for struct fields
				i64_type := llvm.LLVMInt64TypeInContext(gen.ctx)
				
				// Initialize rows field (index 1)
				rows_ptr := llvm.LLVMBuildStructGEP2(gen.builder, var_type, alloca, 1, strings.clone_to_cstring("rows_ptr", context.temp_allocator))
				rows_val := llvm.LLVMConstInt(i64_type, rows, 0)
				llvm.LLVMBuildStore(gen.builder, rows_val, rows_ptr)
				
				// Initialize cols field (index 2)
				cols_ptr := llvm.LLVMBuildStructGEP2(gen.builder, var_type, alloca, 2, strings.clone_to_cstring("cols_ptr", context.temp_allocator))
				cols_val := llvm.LLVMConstInt(i64_type, cols, 0)
				llvm.LLVMBuildStore(gen.builder, cols_val, cols_ptr)
				
				// Initialize leading dimension field (index 3)
				ld_ptr := llvm.LLVMBuildStructGEP2(gen.builder, var_type, alloca, 3, strings.clone_to_cstring("ld_ptr", context.temp_allocator))
				ld_val := llvm.LLVMConstInt(i64_type, ld, 0)
				llvm.LLVMBuildStore(gen.builder, ld_val, ld_ptr)
				
				// For stack tensors, data is already part of the struct (index 0)
				// For heap tensors, allocate memory and set the pointer
				if !tensor_type.is_contiguous || checker.tensor_calculate_size(&tensor_type) > 16384 {
					// Calculate size in bytes
					elem_type := type_to_llvm(gen, tensor_type.dtype)
					elem_size_val := llvm.LLVMConstInt(llvm.LLVMInt64TypeInContext(gen.ctx), cast(u64)tensor_type.dtype.cached_size, 0)
					total_elements_val := llvm.LLVMConstInt(llvm.LLVMInt64TypeInContext(gen.ctx), rows * cols, 0)
					byte_size := llvm.LLVMBuildMul(gen.builder, elem_size_val, total_elements_val, strings.clone_to_cstring("byte_size", context.temp_allocator))
					
					// Call malloc to allocate heap memory
					malloc_fn := get_or_declare_malloc(gen)
					
					// Get malloc function type
					i8_ptr_type := llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)
					size_t_type := llvm.LLVMInt64TypeInContext(gen.ctx)
					malloc_type := llvm.LLVMFunctionType(i8_ptr_type, &size_t_type, 1, 0)
					
					heap_ptr := llvm.LLVMBuildCall2(gen.builder, malloc_type, malloc_fn, &byte_size, 1, strings.clone_to_cstring("heap_ptr", context.temp_allocator))
					
					// Cast to element type pointer
					typed_ptr := llvm.LLVMBuildPointerCast(gen.builder, heap_ptr, 
						llvm.LLVMPointerType(elem_type, 0), strings.clone_to_cstring("typed_ptr", context.temp_allocator))
					
					// Store the pointer in the data field (index 0)
					data_ptr := llvm.LLVMBuildStructGEP2(gen.builder, var_type, alloca, 0, strings.clone_to_cstring("data_ptr", context.temp_allocator))
					llvm.LLVMBuildStore(gen.builder, typed_ptr, data_ptr)
					
					fmt.printf("    Allocated %d bytes on heap for tensor\n", rows * cols * cast(u64)tensor_type.dtype.cached_size)
				}
				
				fmt.printf("    Initialized tensor descriptor: %dx%d\n", rows, cols)
			}
			
			// Process the initializer expression (if any)
			if value_expr != nil {
				fmt.printf("    Processing matrix initializer expression\n")
				init_value := gen_expr_typed(gen, value_expr, entity.type)
				if init_value != nil {
					fmt.printf("    Generated matrix initialization from expression\n")
					// Store the initialization value into the allocated variable
					llvm.LLVMBuildStore(gen.builder, init_value, alloca)
					fmt.printf("    Stored initialization value for %s\n", name)
				} else {
					fmt.printf("    No initializer value generated for %s\n", name)
				}
			}
		}
		*/
		// Only generate initial value if there's a value expression
		if value_expr != nil {
			// Generate the initial value with target type hint
			init_value := gen_expr_typed(gen, value_expr, entity.type)
			if init_value == nil {
				fmt.printf("Failed to generate initial value for %s\n", name)
				return false
			}
			
			// Store the initial value
			llvm.LLVMBuildStore(gen.builder, init_value, alloca)
		}
		
		// Add to IR symbol table (we already have the entity from above)
		gen.ir_symbols[entity] = alloca
		
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