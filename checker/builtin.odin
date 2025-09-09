package checker

import "core:fmt"

// Built-in types following Odin's design

// Global builtin type pointers
t_i8:   ^Type
t_i16:  ^Type
t_i32:  ^Type
t_i64:  ^Type
t_i128: ^Type
t_int:  ^Type  // Platform-specific signed integer

t_u8:   ^Type
t_u16:  ^Type
t_u32:  ^Type
t_u64:  ^Type
t_u128: ^Type
t_uint: ^Type  // Platform-specific unsigned integer

t_f16:  ^Type
t_f32:  ^Type
t_f64:  ^Type

t_bool: ^Type
t_b8:   ^Type
t_b16:  ^Type
t_b32:  ^Type
t_b64:  ^Type

t_string: ^Type
t_rune:   ^Type
t_rawptr: ^Type

// Untyped types - these have size 0 and convert based on context
t_untyped_bool:    ^Type
t_untyped_integer: ^Type
t_untyped_float:   ^Type
t_untyped_string:  ^Type
t_untyped_rune:    ^Type
t_untyped_nil:     ^Type

// Test matrix types (temporary until we have proper syntax)
t_matrix_3x4_f32:     ^Type
t_matrix_100x100_f32: ^Type

// Initialize all builtin types
init_builtin_types :: proc() {
	// Determine pointer size for platform-specific types
	pointer_size := size_of(rawptr)
	
	// Integer types
	t_i8  = make_type_basic(.i8,  1, "i8",  {.Integer})
	t_i16 = make_type_basic(.i16, 2, "i16", {.Integer})
	t_i32 = make_type_basic(.i32, 4, "i32", {.Integer})
	t_i64 = make_type_basic(.i64, 8, "i64", {.Integer})
	t_i128 = make_type_basic(.i128, 16, "i128", {.Integer})
	t_int = make_type_basic(.int, pointer_size, "int", {.Integer})  // Platform-specific
	
	// Unsigned integer types
	t_u8  = make_type_basic(.u8,  1, "u8",  {.Integer, .Unsigned})
	t_u16 = make_type_basic(.u16, 2, "u16", {.Integer, .Unsigned})
	t_u32 = make_type_basic(.u32, 4, "u32", {.Integer, .Unsigned})
	t_u64 = make_type_basic(.u64, 8, "u64", {.Integer, .Unsigned})
	t_u128 = make_type_basic(.u128, 16, "u128", {.Integer, .Unsigned})
	t_uint = make_type_basic(.uint, pointer_size, "uint", {.Integer, .Unsigned})  // Platform-specific
	
	// Float types
	t_f16 = make_type_basic(.f16, 2, "f16", {.Float})
	t_f32 = make_type_basic(.f32, 4, "f32", {.Float})
	t_f64 = make_type_basic(.f64, 8, "f64", {.Float})
	
	// Boolean types
	t_bool = make_type_basic(.bool, 1, "bool", {.Boolean})
	t_b8   = make_type_basic(.b8,  1, "b8",  {.Boolean})
	t_b16  = make_type_basic(.b16, 2, "b16", {.Boolean})
	t_b32  = make_type_basic(.b32, 4, "b32", {.Boolean})
	t_b64  = make_type_basic(.b64, 8, "b64", {.Boolean})
	
	// Special types
	t_string = make_type_basic(.string, 16, "string", {.String}) // ptr + len
	t_rune   = make_type_basic(.rune,   4,  "rune",   {.Rune})
	t_rawptr = make_type_basic(.rawptr, 8,  "rawptr", {.Pointer})
	
	// Untyped types - size 0, will be resolved based on context
	t_untyped_bool    = make_type_basic(.UntypedBool,    0, "untyped bool",    {.Boolean, .Untyped})
	t_untyped_integer = make_type_basic(.UntypedInteger, 0, "untyped integer", {.Integer, .Untyped})
	t_untyped_float   = make_type_basic(.UntypedFloat,   0, "untyped float",   {.Float, .Untyped})
	t_untyped_string  = make_type_basic(.UntypedString,  0, "untyped string",  {.String, .Untyped})
	t_untyped_rune    = make_type_basic(.UntypedRune,    0, "untyped rune",    {.Rune, .Untyped})
	t_untyped_nil     = make_type_basic(.UntypedNil,     0, "untyped nil",     {.Untyped})
	
	// Test matrix types
	// 3x4 f32 matrix = 48 bytes (stack allocated)
	t_matrix_3x4_f32 = make_type_matrix(t_f32, []MatrixDim{
		{size = 3, lower_bound = 0, upper_bound = 2, stride = 1},
		{size = 4, lower_bound = 0, upper_bound = 3, stride = 1},
	})
	
	// 100x100 f32 matrix = 40KB (heap allocated)
	t_matrix_100x100_f32 = make_type_matrix(t_f32, []MatrixDim{
		{size = 100, lower_bound = 0, upper_bound = 99, stride = 1},
		{size = 100, lower_bound = 0, upper_bound = 99, stride = 1},
	})
	
	// Debug output
	if mat_3x4, ok := t_matrix_3x4_f32.variant.(TypeMatrix); ok {
		fmt.printf("3x4 matrix: heap=%v, size=%d bytes\n", mat_3x4.heap_alloc, t_matrix_3x4_f32.cached_size)
	}
	if mat_100x100, ok := t_matrix_100x100_f32.variant.(TypeMatrix); ok {
		fmt.printf("100x100 matrix: heap=%v, size=%d bytes\n", mat_100x100.heap_alloc, t_matrix_100x100_f32.cached_size)
	}
}

// Get the default type for an untyped type
default_type :: proc(t: ^Type) -> ^Type {
	if t == nil do return nil
	if t.kind != .Basic do return t
	
	basic, ok := t.variant.(TypeBasic)
	if !ok do return t
	if !(.Untyped in basic.flags) do return t
	
	// Convert untyped to default concrete type
	#partial switch basic.kind {
	case .UntypedBool:
		return t_bool
	case .UntypedInteger:
		return t_int  // Default to platform-specific int
	case .UntypedFloat:
		return t_f64  // Default to f64
	case .UntypedString:
		return t_string
	case .UntypedRune:
		return t_rune
	case .UntypedNil:
		return t_rawptr
	}
	
	return t
}

// Check if a type is untyped
is_type_untyped :: proc(t: ^Type) -> bool {
	if t == nil do return false
	if t.kind != .Basic do return false
	
	if basic, ok := t.variant.(TypeBasic); ok {
		return .Untyped in basic.flags
	}
	
	return false
}

// Get builtin type by name
get_builtin_type :: proc(name: string) -> ^Type {
	switch name {
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
	
	case "f16":  return t_f16
	case "f32":  return t_f32
	case "f64":  return t_f64
	
	case "bool": return t_bool
	case "b8":   return t_b8
	case "b16":  return t_b16
	case "b32":  return t_b32
	case "b64":  return t_b64
	
	case "string": return t_string
	case "rune":   return t_rune
	case "rawptr": return t_rawptr
	
	// Temporary matrix types
	case "matrix_3x4_f32":     return t_matrix_3x4_f32
	case "matrix_100x100_f32": return t_matrix_100x100_f32
	}
	
	return nil
}