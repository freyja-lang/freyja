package llvm_backend

import llvm "../../llvm"
import "../../checker"
import "core:fmt"
import "core:c"

// Type conversion (like Odin's llvm_backend_type.cpp)

// Convert a Freyja type to an LLVM type
type_to_llvm :: proc(gen: ^IRGenerator, type: ^checker.Type) -> llvm.LLVMTypeRef {
	if type == nil {
		return llvm.LLVMVoidTypeInContext(gen.ctx)
	}
	
	// Check if we've already converted this type
	// TODO: Add type caching
	
	#partial switch type.kind {
	case .Basic:
		basic, ok := type.variant.(checker.TypeBasic)
		if ok {
			#partial switch basic.kind {
			case .i8, .u8:
				return llvm.LLVMInt8TypeInContext(gen.ctx)
			case .i16, .u16:
				return llvm.LLVMInt16TypeInContext(gen.ctx)
			case .i32, .u32:
				return llvm.LLVMInt32TypeInContext(gen.ctx)
			case .i64, .u64:
				return llvm.LLVMInt64TypeInContext(gen.ctx)
			case .int, .uint:
				// Platform-specific: use pointer-sized integer
				if basic.size == 8 {
					return llvm.LLVMInt64TypeInContext(gen.ctx)
				} else {
					return llvm.LLVMInt32TypeInContext(gen.ctx)
				}
			case .f32:
				return llvm.LLVMFloatTypeInContext(gen.ctx)
			case .f64:
				return llvm.LLVMDoubleTypeInContext(gen.ctx)
			case .bool, .b8:
				return llvm.LLVMInt1TypeInContext(gen.ctx)
			case .Invalid:
				return llvm.LLVMVoidTypeInContext(gen.ctx)
			case:
				return llvm.LLVMInt32TypeInContext(gen.ctx)
			}
		}
		return llvm.LLVMVoidTypeInContext(gen.ctx)
		
	case .Pointer:
		ptr, ptr_ok := type.variant.(checker.TypePointer)
		if ptr_ok {
			elem_type := type_to_llvm(gen, ptr.elem)
			return llvm.LLVMPointerType(elem_type, 0)
		}
		return llvm.LLVMVoidTypeInContext(gen.ctx)
		
	case .Array:
		array, array_ok := type.variant.(checker.TypeArray)
		if array_ok {
			elem_type := type_to_llvm(gen, array.elem)
			return llvm.LLVMArrayType(elem_type, cast(u32)array.count)
		}
		return llvm.LLVMVoidTypeInContext(gen.ctx)
	
	case .Tensor:
			// For tensors, create a descriptor structure or simple array
		tensor, tensor_ok := type.variant.(checker.TypeTensor)
		if tensor_ok {
			
			// Check if dtype is nil
			if tensor.dtype == nil {
				// fmt.printf("ERROR: Tensor has nil dtype\n")
				return llvm.LLVMVoidTypeInContext(gen.ctx)
			}
			
			elem_type := type_to_llvm(gen, tensor.dtype)
			
			if tensor.rank == 0 {
				// Scalar tensor - just the element type
				return elem_type
			}
			
			// Calculate total elements if all dimensions are known
			if checker.tensor_can_calculate_size(&tensor) {
				total_bytes := checker.tensor_calculate_size(&tensor)
				if tensor.dtype.cached_size <= 0 {
					// fmt.printf("ERROR: Tensor dtype has invalid cached_size: %d\n", tensor.dtype.cached_size)
					// Default to pointer type for safety
					return llvm.LLVMPointerType(elem_type, 0)
				}
				total_elements := total_bytes / tensor.dtype.cached_size
				array_type := llvm.LLVMArrayType(elem_type, cast(u32)total_elements)
				return array_type
			} else {
				// Dynamic tensor - use pointer to data
				return llvm.LLVMPointerType(elem_type, 0)
			}
		}
		return llvm.LLVMVoidTypeInContext(gen.ctx)
		
	case .Generic:
		// For generic types, we need specialization first
		// For now, use a placeholder pointer type
		// In a complete implementation, this would error or trigger specialization
		return llvm.LLVMPointerType(llvm.LLVMInt8TypeInContext(gen.ctx), 0)

	case .Proc:
		// For procedure types, generate function type
		proc_type: checker.TypeProc
		ok: bool
		proc_type, ok = type.variant.(checker.TypeProc)
		if ok {
			// Get return type
			ret_type := llvm.LLVMVoidTypeInContext(gen.ctx)
			if proc_type.results != nil && proc_type.results.kind == .Tuple {
				tuple_type: checker.TypeTuple
				tuple_ok: bool
				tuple_type, tuple_ok = proc_type.results.variant.(checker.TypeTuple)
				if tuple_ok && len(tuple_type.types) > 0 {
					ret_type = type_to_llvm(gen, tuple_type.types[0])
				}
			}
			
			// Get parameter types
			param_types: [dynamic]llvm.LLVMTypeRef
			defer delete(param_types)
			
			if proc_type.params != nil && proc_type.params.kind == .Tuple {
				tuple_type: checker.TypeTuple
				tuple_ok: bool
				tuple_type, tuple_ok = proc_type.params.variant.(checker.TypeTuple)
				if tuple_ok {
					for param_type in tuple_type.types {
						append(&param_types, type_to_llvm(gen, param_type))
					}
				}
			}
			
			return llvm.LLVMFunctionType(
				ret_type, 
				raw_data(param_types) if len(param_types) > 0 else nil,
				cast(u32)len(param_types),
				0, // not variadic
			)
		}
		return llvm.LLVMVoidTypeInContext(gen.ctx)
		
	case:
		fmt.printf("Unhandled type kind: %v\n", type.kind)
		return llvm.LLVMVoidTypeInContext(gen.ctx)
	}
}

// Get the LLVM type for a checker entity
entity_type_to_llvm :: proc(gen: ^IRGenerator, entity: ^checker.Entity) -> llvm.LLVMTypeRef {
	if entity.type != nil {
		return type_to_llvm(gen, entity.type)
	}
	
	// Default types based on entity kind
	#partial switch entity.kind {
	case .PROCEDURE:
		// TODO: Generate proper function type
		ret_type := llvm.LLVMInt32TypeInContext(gen.ctx)
		return llvm.LLVMFunctionType(ret_type, nil, 0, 0)
		
	case .VARIABLE, .CONSTANT:
		// Default to i32 for now
		return llvm.LLVMInt32TypeInContext(gen.ctx)
		
	case:
		return llvm.LLVMVoidTypeInContext(gen.ctx)
	}
}