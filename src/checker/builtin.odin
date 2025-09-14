package checker

import "core:fmt"

// Threshold for when matrices should be heap allocated (16KB)
MATRIX_HEAP_THRESHOLD :: 16 * 1024

// Static storage for all builtin types - no heap allocation needed
// Following Odin's C++ compiler design with a static array

@(private)
basic_type_array := [BasicKind]Type{
	.Invalid = {
		kind = .Basic,
		variant = TypeBasic{kind = .Invalid, size = 0, name = "invalid", flags = {}},
		cached_size = 0,
	},
	
	// Boolean types
	.bool = {
		kind = .Basic,
		variant = TypeBasic{kind = .bool, size = 1, name = "bool", flags = {.Boolean}},
		cached_size = 1,
	},
	.b8 = {
		kind = .Basic,
		variant = TypeBasic{kind = .b8, size = 1, name = "b8", flags = {.Boolean}},
		cached_size = 1,
	},
	.b16 = {
		kind = .Basic,
		variant = TypeBasic{kind = .b16, size = 2, name = "b16", flags = {.Boolean}},
		cached_size = 2,
	},
	.b32 = {
		kind = .Basic,
		variant = TypeBasic{kind = .b32, size = 4, name = "b32", flags = {.Boolean}},
		cached_size = 4,
	},
	.b64 = {
		kind = .Basic,
		variant = TypeBasic{kind = .b64, size = 8, name = "b64", flags = {.Boolean}},
		cached_size = 8,
	},
	
	// Signed integers
	.i8 = {
		kind = .Basic,
		variant = TypeBasic{kind = .i8, size = 1, name = "i8", flags = {.Integer}},
		cached_size = 1,
	},
	.i16 = {
		kind = .Basic,
		variant = TypeBasic{kind = .i16, size = 2, name = "i16", flags = {.Integer}},
		cached_size = 2,
	},
	.i32 = {
		kind = .Basic,
		variant = TypeBasic{kind = .i32, size = 4, name = "i32", flags = {.Integer}},
		cached_size = 4,
	},
	.i64 = {
		kind = .Basic,
		variant = TypeBasic{kind = .i64, size = 8, name = "i64", flags = {.Integer}},
		cached_size = 8,
	},
	.i128 = {
		kind = .Basic,
		variant = TypeBasic{kind = .i128, size = 16, name = "i128", flags = {.Integer}},
		cached_size = 16,
	},
	.int = {
		kind = .Basic,
		variant = TypeBasic{kind = .int, size = size_of(int), name = "int", flags = {.Integer}},
		cached_size = i64(size_of(int)),
	},
	
	// Unsigned integers
	.u8 = {
		kind = .Basic,
		variant = TypeBasic{kind = .u8, size = 1, name = "u8", flags = {.Integer, .Unsigned}},
		cached_size = 1,
	},
	.u16 = {
		kind = .Basic,
		variant = TypeBasic{kind = .u16, size = 2, name = "u16", flags = {.Integer, .Unsigned}},
		cached_size = 2,
	},
	.u32 = {
		kind = .Basic,
		variant = TypeBasic{kind = .u32, size = 4, name = "u32", flags = {.Integer, .Unsigned}},
		cached_size = 4,
	},
	.u64 = {
		kind = .Basic,
		variant = TypeBasic{kind = .u64, size = 8, name = "u64", flags = {.Integer, .Unsigned}},
		cached_size = 8,
	},
	.u128 = {
		kind = .Basic,
		variant = TypeBasic{kind = .u128, size = 16, name = "u128", flags = {.Integer, .Unsigned}},
		cached_size = 16,
	},
	.uint = {
		kind = .Basic,
		variant = TypeBasic{kind = .uint, size = size_of(uint), name = "uint", flags = {.Integer, .Unsigned}},
		cached_size = i64(size_of(uint)),
	},
	
	// Float types
	.f16 = {
		kind = .Basic,
		variant = TypeBasic{kind = .f16, size = 2, name = "f16", flags = {.Float}},
		cached_size = 2,
	},
	.f32 = {
		kind = .Basic,
		variant = TypeBasic{kind = .f32, size = 4, name = "f32", flags = {.Float}},
		cached_size = 4,
	},
	.f64 = {
		kind = .Basic,
		variant = TypeBasic{kind = .f64, size = 8, name = "f64", flags = {.Float}},
		cached_size = 8,
	},
	
	// Special types
	.string = {
		kind = .Basic,
		variant = TypeBasic{kind = .string, size = 16, name = "string", flags = {.String}}, // ptr + len
		cached_size = 16,
	},
	.rune = {
		kind = .Basic,
		variant = TypeBasic{kind = .rune, size = 4, name = "rune", flags = {.Rune}},
		cached_size = 4,
	},
	.rawptr = {
		kind = .Basic,
		variant = TypeBasic{kind = .rawptr, size = 8, name = "rawptr", flags = {.Pointer}},
		cached_size = 8,
	},
	
	// Untyped types - size 0, resolved based on context
	.UntypedBool = {
		kind = .Basic,
		variant = TypeBasic{kind = .UntypedBool, size = 0, name = "untyped bool", flags = {.Boolean, .Untyped}},
		cached_size = 0,
	},
	.UntypedInteger = {
		kind = .Basic,
		variant = TypeBasic{kind = .UntypedInteger, size = 0, name = "untyped integer", flags = {.Integer, .Untyped}},
		cached_size = 0,
	},
	.UntypedFloat = {
		kind = .Basic,
		variant = TypeBasic{kind = .UntypedFloat, size = 0, name = "untyped float", flags = {.Float, .Untyped}},
		cached_size = 0,
	},
	.UntypedString = {
		kind = .Basic,
		variant = TypeBasic{kind = .UntypedString, size = 0, name = "untyped string", flags = {.String, .Untyped}},
		cached_size = 0,
	},
	.UntypedRune = {
		kind = .Basic,
		variant = TypeBasic{kind = .UntypedRune, size = 0, name = "untyped rune", flags = {.Rune, .Untyped}},
		cached_size = 0,
	},
	.UntypedNil = {
		kind = .Basic,
		variant = TypeBasic{kind = .UntypedNil, size = 0, name = "untyped nil", flags = {.Untyped}},
		cached_size = 0,
	},
}

// Global builtin type pointers - point to static array elements
// No allocation needed!
t_invalid := &basic_type_array[.Invalid]

// Boolean types
t_bool := &basic_type_array[.bool]
t_b8   := &basic_type_array[.b8]
t_b16  := &basic_type_array[.b16]
t_b32  := &basic_type_array[.b32]
t_b64  := &basic_type_array[.b64]

// Integer types
t_i8   := &basic_type_array[.i8]
t_i16  := &basic_type_array[.i16]
t_i32  := &basic_type_array[.i32]
t_i64  := &basic_type_array[.i64]
t_i128 := &basic_type_array[.i128]
t_int  := &basic_type_array[.int]

t_u8   := &basic_type_array[.u8]
t_u16  := &basic_type_array[.u16]
t_u32  := &basic_type_array[.u32]
t_u64  := &basic_type_array[.u64]
t_u128 := &basic_type_array[.u128]
t_uint := &basic_type_array[.uint]

// Float types
t_f16 := &basic_type_array[.f16]
t_f32 := &basic_type_array[.f32]
t_f64 := &basic_type_array[.f64]

// Special types
t_string := &basic_type_array[.string]
t_rune   := &basic_type_array[.rune]
t_rawptr := &basic_type_array[.rawptr]

// Untyped types
t_untyped_bool    := &basic_type_array[.UntypedBool]
t_untyped_integer := &basic_type_array[.UntypedInteger]
t_untyped_float   := &basic_type_array[.UntypedFloat]
t_untyped_string  := &basic_type_array[.UntypedString]
t_untyped_rune    := &basic_type_array[.UntypedRune]
t_untyped_nil     := &basic_type_array[.UntypedNil]

// Common tensor types (industry standard)
t_tensor_f32:         ^Type  // Scalar tensor
t_tensor_vector_f32:  ^Type  // Vector tensor
t_tensor_matrix_f32:  ^Type  // Matrix tensor
t_tensor_3d_f32:      ^Type  // 3D tensor
t_tensor_f64:         ^Type  // Double precision scalar
t_tensor_i32:         ^Type  // Integer scalar

// Initialize tensor types (industry standard)
init_builtin_types :: proc() {
	
	// Initialize tensor types
	t_tensor_f32        = make_type_tensor(t_f32)                      // Scalar (0D)
	t_tensor_vector_f32 = make_type_tensor(t_f32, []i64{-1})           // Vector (1D, dynamic)
	t_tensor_matrix_f32 = make_type_tensor(t_f32, []i64{-1, -1})       // Matrix (2D, dynamic)
	t_tensor_3d_f32     = make_type_tensor(t_f32, []i64{-1, -1, -1})   // 3D tensor (dynamic)
	t_tensor_f64        = make_type_tensor(t_f64)                      // Double scalar
	t_tensor_i32        = make_type_tensor(t_i32)                      // Integer scalar
	
	fmt.printf("Initialized tensor types:\n")
	fmt.printf("  scalar f32: %s\n", type_to_string(t_tensor_f32))
	fmt.printf("  vector f32: %s\n", type_to_string(t_tensor_vector_f32))
	fmt.printf("  matrix f32: %s\n", type_to_string(t_tensor_matrix_f32))
	fmt.printf("  3D tensor f32: %s\n", type_to_string(t_tensor_3d_f32))
}

// Get builtin type by name - O(1) lookup for common types
get_builtin_type :: proc(name: string) -> ^Type {
	switch name {
	// Boolean
	case "bool": return t_bool
	case "b8":   return t_b8
	case "b16":  return t_b16
	case "b32":  return t_b32
	case "b64":  return t_b64
	
	// Integer
	case "i8":   return t_i8
	case "i16":  return t_i16
	case "i32":  return t_i32
	case "i64":  return t_i64
	case "i128": return t_i128
	case "int":  return t_int
	
	case "u8":   return t_u8
	case "u16":  return t_u16
	case "u32":  return t_u32
	case "u64":  return t_u64
	case "u128": return t_u128
	case "uint": return t_uint
	
	// Float
	case "f16": return t_f16
	case "f32": return t_f32
	case "f64": return t_f64
	
	// Special
	case "string": return t_string
	case "rune":   return t_rune
	case "rawptr": return t_rawptr
	
	// Tensor types (after init_builtin_types is called)
	case "tensor": return t_tensor_f32        // Default to f32 scalar
	case "tensor_f32": return t_tensor_f32
	case "tensor_f64": return t_tensor_f64
	case "tensor_i32": return t_tensor_i32
	
	case: return nil
	}
}