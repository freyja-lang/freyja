package checker

import "core:odin/ast"
import "../llvm"

// Type kinds - following Odin's design
TypeKind :: enum {
	Invalid,
	
	// Basic types
	Basic,
	
	// Composite types
	Pointer,
	Array,
	Slice,
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