package checker

import "core:odin/ast"
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
	Matrix,  // Fortran-style matrix with runtime shape
	Struct,
	Union,
	Enum,
	Tuple,
	
	// Special types
	Proc,
	Named,
	
	// TODO: Add more as needed
	// Generic,
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

// Fortran-style matrix type
MatrixDim :: struct {
	size:        i64,  // -1 for dynamic (:), positive for static
	lower_bound: i64,  // Default 0, can be 1 for Fortran-style
	upper_bound: i64,  // -1 for dynamic
	stride:      i64,  // Default 1, for slicing
}

TypeMatrix :: struct {
	elem:         ^Type,        // Element type
	dims:         [dynamic]MatrixDim, // Dimension info
	column_major: bool,         // Storage order (true by default)
	heap_alloc:   bool,         // True if heap allocated
	is_view:      bool,         // True if this is a view/slice
	// Runtime descriptor will be generated as needed
}

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

// Main Type structure - following Odin's union design
Type :: struct {
	kind: TypeKind,
	
	// Union of type-specific data
	variant: union {
		TypeBasic,
		TypeNamed,
		TypePointer,
		TypeArray,
		TypeSlice,
		TypeMatrix,
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

// Allocation size threshold (16KB)
MATRIX_STACK_THRESHOLD :: 16 * 1024

make_type_matrix :: proc(elem: ^Type, dims: []MatrixDim = nil) -> ^Type {
	t := make_type(.Matrix)
	matrix_variant := TypeMatrix{
		elem = elem,
		column_major = true,  // Default to Fortran-style
	}
	
	if dims != nil {
		for dim in dims {
			append(&matrix_variant.dims, dim)
		}
	}
	
	// Calculate if this should be heap allocated
	if can_calculate_matrix_size(elem, dims) {
		size := calculate_matrix_size(elem, dims)
		matrix_variant.heap_alloc = size > MATRIX_STACK_THRESHOLD
		t.cached_size = size
	} else {
		// Dynamic matrices are always heap allocated
		matrix_variant.heap_alloc = true
		t.cached_size = -1
	}
	
	t.variant = matrix_variant
	t.cached_align = 8  // Pointer alignment for descriptor
	return t
}

can_calculate_matrix_size :: proc(elem: ^Type, dims: []MatrixDim) -> bool {
	if elem == nil || dims == nil do return false
	for dim in dims {
		if dim.size < 0 do return false  // Dynamic dimension
	}
	return elem.cached_size > 0
}

calculate_matrix_size :: proc(elem: ^Type, dims: []MatrixDim) -> i64 {
	if !can_calculate_matrix_size(elem, dims) do return -1
	
	size := elem.cached_size
	for dim in dims {
		size *= dim.size
	}
	return size
}

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
	case .Matrix:
		if mat, ok := t.variant.(TypeMatrix); ok {
			elem_str := type_to_string(mat.elem)
			dim_str := ""
			for d, idx in mat.dims {
				if idx > 0 do dim_str = fmt.tprintf("%s, ", dim_str)
				if d.size < 0 {
					dim_str = fmt.tprintf("%s:", dim_str)
				} else {
					dim_str = fmt.tprintf("%s%d", dim_str, d.size)
				}
			}
			return fmt.tprintf("matrix[%s]%s", dim_str, elem_str)
		}
	case .Proc:
		return "proc"
	}
	
	return fmt.tprintf("%v", t.kind)
}