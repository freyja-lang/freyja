package checker

import "../ast"
import "../llvm"
import "core:fmt"

// Type kinds - following Odin's design
TypeKind :: enum {
	Invalid,
	
	// Basic types
	Basic,
	
	// Composite types
	Pointer,
	Array,
	Slice,
	Tensor,  // Industry-standard n-dimensional tensor (replaces Matrix)
	Struct,
	Union,
	Enum,
	Tuple,
	
	// Special types
	Proc,
	Named,
	Generic,

	// TODO: Add more as needed
	// Map,
	// DynamicArray,
}

// Basic type kinds
BasicKind :: enum {
	Invalid,
	
	// Boolean
	bool,
	b8, b16, b32, b64,
	
	// Integers  
	i8, i16, i32, i64, i128,
	u8, u16, u32, u64, u128,
	int, uint,  // Platform-specific integers
	
	// Floats
	f16, f32, f64,
	
	// Special
	string,
	rune,
	rawptr,
	
	// Untyped
	UntypedBool,
	UntypedInteger,
	UntypedFloat,
	UntypedString,
	UntypedRune,
	UntypedNil,
}

BasicFlag :: enum {
	Boolean,
	Integer,
	Unsigned,
	Float,
	Complex,
	Pointer,
	String,
	Rune,
	Untyped,
}

BasicFlags :: bit_set[BasicFlag]

// Type-specific structures

TypeBasic :: struct {
	kind:  BasicKind,
	flags: BasicFlags,
	size:  int, // in bytes
	name:  string,
}

TypeNamed :: struct {
	name:      string,
	base:      ^Type,
	type_name: ^Entity, // Entity with kind == .TYPE_NAME
}

TypePointer :: struct {
	elem: ^Type,
}

// Generic type - following Odin's Type_Generic structure
TypeGeneric :: struct {
	id:          i64,      // Unique identifier for this generic
	name:        string,   // Name of the generic parameter (e.g., "T", "M", "N")
	specialized: ^Type,    // The concrete type this generic resolves to (nil if unresolved)
	scope:       ^Scope,   // The scope where this generic is defined
	entity:      ^Entity,  // Optional: Entity that defines this generic
}

TypeArray :: struct {
	elem:  ^Type,
	count: i64,
}

TypeSlice :: struct {
	elem: ^Type,
}

TypeStruct :: struct {
	fields:            [dynamic]^Entity, // Entity_Variable
	tags:              [dynamic]string,  // count == fields.count
	offsets:           [dynamic]i64,     // count == fields.count
	
	node:              ^ast.Expr,
	scope:             ^Scope,
	
	custom_align:      i64,
	is_packed:         bool,
	is_union:          bool, // C-style union
	are_offsets_set:   bool,
}

TypeUnion :: struct {
	variants:     [dynamic]^Type,
	variant_size: i64,
	custom_align: i64,
}

TypeEnum :: struct {
	fields:     [dynamic]^Entity,
	base_type:  ^Type,
	is_flags:   bool,
}

TypeTuple :: struct {
	types:   [dynamic]^Type,
	offsets: [dynamic]i64,
}

// Legacy Matrix type removed - use TypeTensor instead

TypeProc :: struct {
	node:              ^ast.Expr,
	scope:             ^Scope,
	params:            ^Type, // Should be Type_Tuple
	results:           ^Type, // Should be Type_Tuple
	param_count:       int,
	result_count:      int,
	calling_convention: CallingConvention,
	variadic:          bool,
	diverging:         bool, // never returns
	// TODO: Add more as needed
}

CallingConvention :: enum {
	Invalid,
	Odin,
	Contextless,
	CDecl,
	StdCall,
	FastCall,
}

// Industry-standard tensor definitions (following NumPy/PyTorch/TensorFlow)

// Memory layout order
MemoryOrder :: enum {
	RowMajor,    // C-contiguous (last index varies fastest)
	ColumnMajor, // Fortran-contiguous (first index varies fastest)
}

// Device types for tensor allocation
TensorDevice :: enum {
	CPU,
	GPU,     // CUDA/ROCm
	TPU,     // Tensor Processing Unit
	Metal,   // Apple Silicon
}

// Tensor type - unified n-dimensional array type
TypeTensor :: struct {
	dtype:        ^Type,          // Element data type (f32, f64, i32, etc.)
	rank:         i64,            // Number of dimensions (0 for scalar, 1 for vector, 2 for matrix, etc.)
	shape:        [dynamic]i64,   // Size of each dimension (len = rank)
	strides:      [dynamic]i64,   // Stride for each dimension in elements (len = rank)
	memory_order: MemoryOrder,    // Memory layout
	is_view:      bool,           // True if this is a view/slice of another tensor
	is_contiguous: bool,          // True if memory is contiguous
	requires_grad: bool,          // For automatic differentiation (future)
	device:       TensorDevice,   // Where tensor is allocated
}

// Main Type structure - following Odin's union design
Type :: struct {
	kind: TypeKind,
	
	// Union of type-specific data
	variant: union {
		TypeBasic,
		TypeNamed,
		TypePointer,
		TypeGeneric,
		TypeArray,
		TypeSlice,
		TypeTensor,
		TypeStruct,
		TypeUnion,
		TypeEnum,
		TypeTuple,
		TypeProc,
	},
	
	// Cached values
	cached_size:  i64,
	cached_align: i64,
	flags:        TypeFlags,
}

TypeFlags :: enum {
	Comparable,
	Ordered,
	Numeric,
	Integer,
	Float,
	// TODO: Add more flags as needed
}

// Helper functions

// Check if a type is untyped (will be resolved based on context)
is_type_untyped :: proc(t: ^Type) -> bool {
	if t == nil || t.kind != .Basic { return false }
	if basic, ok := t.variant.(TypeBasic); ok {
		return .Untyped in basic.flags
	}
	return false
}

// Get the default type for an untyped type
default_type :: proc(t: ^Type) -> ^Type {
	if t == nil || t.kind != .Basic { return t }
	if basic, ok := t.variant.(TypeBasic); ok {
		#partial switch basic.kind {
		case .UntypedBool:    return t_bool
		case .UntypedInteger: return t_int
		case .UntypedFloat:   return t_f64
		case .UntypedString:  return t_string
		case .UntypedRune:    return t_rune
		case .UntypedNil:     return t_rawptr
		}
	}
	return t
}

make_type :: proc(kind: TypeKind) -> ^Type {
	t := new(Type)
	t.kind = kind
	return t
}

make_type_basic :: proc(kind: BasicKind, size: int, name: string, flags: BasicFlags) -> ^Type {
	t := make_type(.Basic)
	t.variant = TypeBasic{
		kind = kind,
		size = size,
		name = name,
		flags = flags,
	}
	t.cached_size = i64(size)
	return t
}

make_type_pointer :: proc(elem: ^Type) -> ^Type {
	t := make_type(.Pointer)
	t.variant = TypePointer{elem = elem}
	t.cached_size = 8 // Assuming 64-bit pointers
	t.cached_align = 8
	return t
}

make_type_generic :: proc(scope: ^Scope, id: i64, name: string, specialized: ^Type = nil) -> ^Type {
	t := make_type(.Generic)
	t.variant = TypeGeneric{
		id = id,
		name = name,
		specialized = specialized,
		scope = scope,
		entity = nil,
	}
	// Size and alignment depend on specialization
	if specialized != nil {
		t.cached_size = specialized.cached_size
		t.cached_align = specialized.cached_align
	} else {
		t.cached_size = -1  // Unknown until specialized
		t.cached_align = -1
	}
	return t
}

make_type_array :: proc(elem: ^Type, count: i64) -> ^Type {
	t := make_type(.Array)
	t.variant = TypeArray{
		elem = elem,
		count = count,
	}
	if elem.cached_size > 0 {
		t.cached_size = elem.cached_size * count
		t.cached_align = elem.cached_align
	}
	return t
}

make_type_slice :: proc(elem: ^Type) -> ^Type {
	t := make_type(.Slice)
	t.variant = TypeSlice{elem = elem}
	// A slice is a struct { data: ^T, len: int }
	t.cached_size = 16  // pointer + length
	t.cached_align = 8
	return t
}

make_type_proc :: proc() -> ^Type {
	t := make_type(.Proc)
	t.variant = TypeProc{}
	t.cached_size = 8  // Function pointer
	t.cached_align = 8
	return t
}

make_type_struct :: proc() -> ^Type {
	t := make_type(.Struct)
	t.variant = TypeStruct{}
	return t
}

make_type_tuple :: proc() -> ^Type {
	t := make_type(.Tuple)
	t.variant = TypeTuple{}
	return t
}

// Create a new tensor type (industry standard)
make_type_tensor :: proc(dtype: ^Type, shape: []i64 = nil, memory_order: MemoryOrder = .RowMajor) -> ^Type {
	t := make_type(.Tensor)
	tensor := TypeTensor{
		dtype = dtype,
		memory_order = memory_order,
		device = .CPU,
		is_contiguous = true,
	}
	
	// Set shape and calculate strides
	if shape != nil {
		tensor.rank = i64(len(shape))
		for s in shape {
			append(&tensor.shape, s)
		}
		tensor_calculate_strides(&tensor)
	}
	
	t.variant = tensor
	
	// Calculate size if all dimensions are known
	if tensor_can_calculate_size(&tensor) {
		t.cached_size = tensor_calculate_size(&tensor)
	} else {
		t.cached_size = -1 // Dynamic size
	}
	
	t.cached_align = type_align_of(dtype)
	return t
}

// Calculate strides based on shape and memory order
tensor_calculate_strides :: proc(tensor: ^TypeTensor) {
	if tensor.rank == 0 {
		return // Scalar has no strides
	}
	
	clear(&tensor.strides)
	
	if tensor.memory_order == .RowMajor {
		// C-contiguous: last dimension has stride 1
		stride: i64 = 1
		for i := int(tensor.rank - 1); i >= 0; i -= 1 {
			append(&tensor.strides, stride)
			if i > 0 {
				stride *= tensor.shape[i]
			}
		}
		// Reverse to match shape order
		for i := 0; i < len(tensor.strides) / 2; i += 1 {
			j := len(tensor.strides) - 1 - i
			tensor.strides[i], tensor.strides[j] = tensor.strides[j], tensor.strides[i]
		}
	} else {
		// Fortran-contiguous: first dimension has stride 1
		stride: i64 = 1
		for i := 0; i < int(tensor.rank); i += 1 {
			append(&tensor.strides, stride)
			stride *= tensor.shape[i]
		}
	}
}

// Check if we can calculate tensor size at compile time
tensor_can_calculate_size :: proc(tensor: ^TypeTensor) -> bool {
	if tensor.dtype == nil || tensor.dtype.cached_size <= 0 {
		return false
	}
	for s in tensor.shape {
		if s < 0 { // Dynamic dimension
			return false
		}
	}
	return true
}

// Calculate total tensor size in bytes
tensor_calculate_size :: proc(tensor: ^TypeTensor) -> i64 {
	if !tensor_can_calculate_size(tensor) {
		return -1
	}
	
	num_elements: i64 = 1
	for s in tensor.shape {
		num_elements *= s
	}
	
	return num_elements * tensor.dtype.cached_size
}

// Special tensor creation functions for common cases

// Create a vector type (1D tensor)
make_type_vector :: proc(dtype: ^Type, size: i64) -> ^Type {
	return make_type_tensor(dtype, []i64{size})
}

// Create a matrix type (2D tensor)  
make_type_matrix_tensor :: proc(dtype: ^Type, rows, cols: i64, column_major := false) -> ^Type {
	order := column_major ? MemoryOrder.ColumnMajor : MemoryOrder.RowMajor
	return make_type_tensor(dtype, []i64{rows, cols}, order)
}

// Create a batch of matrices (3D tensor)
make_type_batch_matrix :: proc(dtype: ^Type, batch, rows, cols: i64) -> ^Type {
	return make_type_tensor(dtype, []i64{batch, rows, cols})
}

// Legacy matrix functions removed - use tensor functions instead

// Type checking helpers

is_type_integer :: proc(t: ^Type) -> bool {
	if t == nil do return false
	if t.kind != .Basic do return false
	if basic, ok := t.variant.(TypeBasic); ok {
		return .Integer in basic.flags
	}
	return false
}

is_type_float :: proc(t: ^Type) -> bool {
	if t == nil do return false
	if t.kind != .Basic do return false
	if basic, ok := t.variant.(TypeBasic); ok {
		return .Float in basic.flags
	}
	return false
}

is_type_boolean :: proc(t: ^Type) -> bool {
	if t == nil do return false
	if t.kind != .Basic do return false
	if basic, ok := t.variant.(TypeBasic); ok {
		return .Boolean in basic.flags
	}
	return false
}

is_type_pointer :: proc(t: ^Type) -> bool {
	if t == nil do return false
	return t.kind == .Pointer
}

is_type_proc :: proc(t: ^Type) -> bool {
	if t == nil do return false
	return t.kind == .Proc
}

is_type_tensor :: proc(t: ^Type) -> bool {
	if t == nil do return false
	return t.kind == .Tensor
}

// Tensor dimension compatibility checking

// Check if two tensor shapes are compatible for element-wise operations (addition, subtraction)
tensors_same_shape :: proc(a, b: ^Type) -> bool {
	if a == nil || b == nil do return false
	if a.kind != .Tensor || b.kind != .Tensor do return false

	a_tensor, a_ok := a.variant.(TypeTensor)
	b_tensor, b_ok := b.variant.(TypeTensor)
	if !a_ok || !b_ok do return false

	// Must have same rank
	if a_tensor.rank != b_tensor.rank do return false

	// Check each dimension
	for i := 0; i < int(a_tensor.rank); i += 1 {
		a_dim := a_tensor.shape[i]
		b_dim := b_tensor.shape[i]

		// If both are static, they must match
		if a_dim >= 0 && b_dim >= 0 {
			if a_dim != b_dim do return false
		}
		// If either is dynamic (-1), we assume they'll match at runtime
		// This allows operations on generic tensor dimensions
	}

	// Check element types match
	return types_match(a_tensor.dtype, b_tensor.dtype)
}

// Check if tensors are compatible for matrix multiplication
// Handles all standard cases:
// - Matrix @ Matrix: (M, K) @ (K, N) -> (M, N)
// - Matrix @ Vector: (M, K) @ (K,) -> (M,)
// - Vector @ Matrix: (K,) @ (K, N) -> (N,)
// - Vector @ Vector: (K,) @ (K,) -> scalar
// For higher dims: broadcasting rules apply to batch dimensions
tensors_matmul_compatible :: proc(a, b: ^Type) -> (compatible: bool, result_shape: []i64) {
	if a == nil || b == nil do return false, nil
	if a.kind != .Tensor || b.kind != .Tensor do return false, nil

	a_tensor, a_ok := a.variant.(TypeTensor)
	b_tensor, b_ok := b.variant.(TypeTensor)
	if !a_ok || !b_ok do return false, nil

	// Element types must match
	if !types_match(a_tensor.dtype, b_tensor.dtype) do return false, nil

	// Handle different rank combinations

	// Matrix @ Matrix: (M, K) @ (K, N) -> (M, N)
	if a_tensor.rank == 2 && b_tensor.rank == 2 {
		a_rows := a_tensor.shape[0]
		a_cols := a_tensor.shape[1]
		b_rows := b_tensor.shape[0]
		b_cols := b_tensor.shape[1]

		// Check inner dimensions match (if static)
		if a_cols >= 0 && b_rows >= 0 && a_cols != b_rows {
			return false, nil
		}

		// Result shape is (M, N)
		result_shape = make([]i64, 2)
		result_shape[0] = a_rows
		result_shape[1] = b_cols
		return true, result_shape
	}

	// Matrix @ Vector: (M, K) @ (K,) -> (M,)
	if a_tensor.rank == 2 && b_tensor.rank == 1 {
		a_cols := a_tensor.shape[1]
		b_size := b_tensor.shape[0]

		if a_cols >= 0 && b_size >= 0 && a_cols != b_size {
			return false, nil
		}

		result_shape = make([]i64, 1)
		result_shape[0] = a_tensor.shape[0]
		return true, result_shape
	}

	// Vector @ Matrix: (K,) @ (K, N) -> (N,)
	// Note: This is treating the vector as a row vector
	if a_tensor.rank == 1 && b_tensor.rank == 2 {
		a_size := a_tensor.shape[0]
		b_rows := b_tensor.shape[0]
		b_cols := b_tensor.shape[1]

		if a_size >= 0 && b_rows >= 0 && a_size != b_rows {
			return false, nil
		}

		result_shape = make([]i64, 1)
		result_shape[0] = b_cols
		return true, result_shape
	}

	// Vector @ Vector: (K,) @ (K,) -> scalar (dot product)
	if a_tensor.rank == 1 && b_tensor.rank == 1 {
		a_size := a_tensor.shape[0]
		b_size := b_tensor.shape[0]

		if a_size >= 0 && b_size >= 0 && a_size != b_size {
			return false, nil
		}

		// Result is a scalar (0-rank tensor)
		result_shape = make([]i64, 0)
		return true, result_shape
	}

	// Handle higher-dimensional tensors (batched operations)
	if a_tensor.rank > 2 || b_tensor.rank > 2 {
		// For tensors with rank > 2, the last two dimensions are treated as matrices
		// and the leading dimensions must be broadcastable

		// Extract batch dimensions and matrix dimensions
		a_batch_dims := a_tensor.rank - 2
		b_batch_dims := b_tensor.rank - 2

		// Check if batch dimensions are broadcastable
		// This is a simplified check - full implementation would use NumPy rules

		// Get the matrix dimensions (last 2 dims)
		a_mat_rows := a_tensor.shape[a_tensor.rank - 2]
		a_mat_cols := a_tensor.shape[a_tensor.rank - 1]
		b_mat_rows := b_tensor.shape[b_tensor.rank - 2]
		b_mat_cols := b_tensor.shape[b_tensor.rank - 1]

		// Check matrix dimension compatibility
		if a_mat_cols >= 0 && b_mat_rows >= 0 && a_mat_cols != b_mat_rows {
			return false, nil
		}

		// For now, require exact batch dimension match
		// TODO: Implement full broadcasting for batch dimensions
		if a_batch_dims != b_batch_dims {
			return false, nil
		}

		// Check each batch dimension
		for i := 0; i < int(a_batch_dims); i += 1 {
			if a_tensor.shape[i] >= 0 && b_tensor.shape[i] >= 0 {
				if a_tensor.shape[i] != b_tensor.shape[i] && a_tensor.shape[i] != 1 && b_tensor.shape[i] != 1 {
					return false, nil
				}
			}
		}

		// Result shape: broadcast batch dims + (a_mat_rows, b_mat_cols)
		result_shape = make([]i64, a_tensor.rank)
		for i := 0; i < int(a_batch_dims); i += 1 {
			a_dim := a_tensor.shape[i]
			b_dim := b_tensor.shape[i]
			if a_dim >= 0 && b_dim >= 0 {
				result_shape[i] = max(a_dim, b_dim)
			} else {
				result_shape[i] = -1
			}
		}
		result_shape[a_tensor.rank - 2] = a_mat_rows
		result_shape[a_tensor.rank - 1] = b_mat_cols

		return true, result_shape
	}

	// No other combinations are valid
	return false, nil
}

// Check if broadcasting is possible between two tensor shapes
// Following NumPy broadcasting rules
tensors_broadcast_compatible :: proc(a, b: ^Type) -> (compatible: bool, result_shape: []i64) {
	if a == nil || b == nil do return false, nil
	if a.kind != .Tensor || b.kind != .Tensor do return false, nil

	a_tensor, a_ok := a.variant.(TypeTensor)
	b_tensor, b_ok := b.variant.(TypeTensor)
	if !a_ok || !b_ok do return false, nil

	// Element types must match
	if !types_match(a_tensor.dtype, b_tensor.dtype) do return false, nil

	// Implement NumPy broadcasting rules:
	// 1. If tensors have different ranks, prepend 1s to the smaller rank
	// 2. Dimensions are compatible if they are equal or one is 1
	// 3. Result has the max of each dimension

	max_rank := max(a_tensor.rank, b_tensor.rank)
	result_shape = make([]i64, max_rank)

	for i := 0; i < int(max_rank); i += 1 {
		// Index from the right (broadcasting aligns from the right)
		a_idx := int(a_tensor.rank) - int(max_rank) + i
		b_idx := int(b_tensor.rank) - int(max_rank) + i

		a_dim: i64 = 1
		b_dim: i64 = 1

		if a_idx >= 0 {
			a_dim = a_tensor.shape[a_idx]
		}
		if b_idx >= 0 {
			b_dim = b_tensor.shape[b_idx]
		}

		// Check compatibility
		if a_dim >= 0 && b_dim >= 0 {
			// Both are static
			if a_dim != b_dim && a_dim != 1 && b_dim != 1 {
				return false, nil
			}
			result_shape[i] = max(a_dim, b_dim)
		} else {
			// At least one is dynamic
			result_shape[i] = -1
		}
	}

	return true, result_shape
}

// Get the base type (dereference named types)
base_type :: proc(t: ^Type) -> ^Type {
	if t == nil do return nil
	if t.kind == .Named {
		if named, ok := t.variant.(TypeNamed); ok {
			return base_type(named.base)
		}
	}
	return t
}

// Type size calculation
type_size_of :: proc(t: ^Type) -> i64 {
	if t == nil do return 0
	if t.cached_size > 0 do return t.cached_size
	
	// Calculate size based on type kind
	#partial switch t.kind {
	case .Basic:
		if basic, ok := t.variant.(TypeBasic); ok {
			return i64(basic.size)
		}
	case .Pointer:
		return 8 // 64-bit pointers
	case .Array:
		if array, ok := t.variant.(TypeArray); ok {
			elem_size := type_size_of(array.elem)
			return elem_size * array.count
		}
	case .Slice:
		return 16 // ptr + len
	case .Tensor:
		if tensor, ok := t.variant.(TypeTensor); ok {
			return tensor_calculate_size(&tensor)
		}
	case .Proc:
		return 8 // function pointer
	}
	
	return 0
}

// Type alignment calculation
type_align_of :: proc(t: ^Type) -> i64 {
	if t == nil do return 1
	if t.cached_align > 0 do return t.cached_align
	
	#partial switch t.kind {
	case .Basic:
		if basic, ok := t.variant.(TypeBasic); ok {
			return i64(basic.size) // Usually size == align for basic types
		}
	case .Pointer, .Proc:
		return 8
	case .Array:
		if array, ok := t.variant.(TypeArray); ok {
			return type_align_of(array.elem)
		}
	case .Slice:
		return 8 // Alignment of pointer
	case .Tensor:
		if tensor, ok := t.variant.(TypeTensor); ok {
			return type_align_of(tensor.dtype)
		}
	}
	
	return 1
}

// Check if two types match
types_match :: proc(a, b: ^Type) -> bool {
	if a == b do return true
	if a == nil || b == nil do return false
	
	// For now, just check if kinds match and for basic types, check the specific type
	if a.kind != b.kind do return false
	
	if a.kind == .Basic {
		a_basic, a_ok := a.variant.(TypeBasic)
		b_basic, b_ok := b.variant.(TypeBasic)
		if a_ok && b_ok {
			return a_basic.kind == b_basic.kind
		}
	}
	
	// TODO: Add more sophisticated type matching
	return true
}

// Check if type 'from' can be implicitly converted to type 'to'
is_type_convertible :: proc(from, to: ^Type) -> bool {
	if from == to do return true
	if from == nil || to == nil do return false
	
	// Same types always match
	if types_match(from, to) do return true
	
	// Check implicit conversions
	if from.kind == .Basic && to.kind == .Basic {
		from_basic, from_ok := from.variant.(TypeBasic)
		to_basic, to_ok := to.variant.(TypeBasic)
		if from_ok && to_ok {
			// Integer to float conversions are allowed
			if .Integer in from_basic.flags && .Float in to_basic.flags {
				return true
			}
			
			// Integer to integer conversions (widening only)
			if .Integer in from_basic.flags && .Integer in to_basic.flags {
				// Allow if target type is larger
				return to_basic.size >= from_basic.size
			}
			
			// Untyped literals can convert to their typed equivalents
			if .Untyped in from_basic.flags {
				// Untyped int can become any integer or float
				if from_basic.kind == .UntypedInteger {
					return .Integer in to_basic.flags || .Float in to_basic.flags
				}
				// Untyped float can become any float
				if from_basic.kind == .UntypedFloat {
					return .Float in to_basic.flags
				}
				// Untyped bool can become any bool
				if from_basic.kind == .UntypedBool {
					return .Boolean in to_basic.flags
				}
				// Untyped string can become string
				if from_basic.kind == .UntypedString {
					return .String in to_basic.flags
				}
				// Untyped rune can become rune
				if from_basic.kind == .UntypedRune {
					return .Rune in to_basic.flags
				}
			}
		}
	}
	
	return false
}

// Convert type to string for error messages
type_to_string :: proc(t: ^Type) -> string {
	if t == nil do return "unknown"
	
	#partial switch t.kind {
	case .Basic:
		if basic, ok := t.variant.(TypeBasic); ok {
			return basic.name
		}
	case .Pointer:
		if ptr, ok := t.variant.(TypePointer); ok {
			elem_str := type_to_string(ptr.elem)
			return fmt.tprintf("^%s", elem_str)
		}
	case .Array:
		if array, ok := t.variant.(TypeArray); ok {
			elem_str := type_to_string(array.elem)
			return fmt.tprintf("[%d]%s", array.count, elem_str)
		}
	case .Tensor:
		if tensor, ok := t.variant.(TypeTensor); ok {
			elem_str := type_to_string(tensor.dtype)
			if tensor.rank == 0 {
				return fmt.tprintf("tensor<%s>", elem_str)
			}
			
			shape_str := ""
			for s, i in tensor.shape {
				if i > 0 {
					shape_str = fmt.tprintf("%s×", shape_str)
				}
				if s < 0 {
					shape_str = fmt.tprintf("%s?", shape_str)
				} else {
					shape_str = fmt.tprintf("%s%d", shape_str, s)
				}
			}
			
			layout := tensor.memory_order == .RowMajor ? "C" : "F"
			return fmt.tprintf("tensor<%s>[%s](%s)", elem_str, shape_str, layout)
		}
	case .Proc:
		return "proc"
	case .Generic:
		if generic, ok := t.variant.(TypeGeneric); ok {
			if generic.specialized != nil {
				return fmt.tprintf("$%s=%s", generic.name, type_to_string(generic.specialized))
			}
			return fmt.tprintf("$%s", generic.name)
		}
	}

	return fmt.tprintf("%v", t.kind)
}