package parser

Node :: struct {
	using span: Span,
}
Span :: struct {
	start: Pos,
	end:   Pos,
}

File :: struct {
	using node: Node,
	path:       string,
	src:        string,
	imports:    [dynamic]^Import_Decl,
	stmts:      [dynamic]^Stmt,
}


Field :: struct {
	node:          Node,
	name:          ^Expr, // odin is []^Expr
	type:          ^Type,
	default_value: ^Expr,
}

//////////////////////
Stmt :: struct {
	node:  Node,
	value: Any_Stmt,
}
Any_Stmt :: union {
	^Bad_Stmt,
	^Empty_Stmt,
	^Expr_Stmt,
	^Assign_Stmt,
	^Block_Stmt,
	^If_Stmt,
	^Return_Stmt,
	^Defer_Stmt,
	^Loop_Stmt,
	^Case_Clause_Stmt,
	^Switch_Stmt,
	//
	^Bad_Decl,
	^Value_Decl,
	^Import_Decl,
	^Foriegn_Import_Decl,
	^Foriegn_Block_Decl,
	^Using_Stmt,
}
/////////////////////////////////////////////////////////
Bad_Stmt :: struct {}
Empty_Stmt :: struct {
	semicolon: Pos,
}
Expr_Stmt :: struct {
	expr: ^Expr,
}
//tag
Assign_Stmt :: struct {
	lhs: []^Expr,
	rhs: []^Expr,
	op:  Token,
}
Block_Stmt :: struct {
	label: ^Expr,
	stmts: []^Stmt,
}
If_Stmt :: struct {
	label:     ^Expr,
	init:      ^Stmt,
	//
	cond:      ^Expr,
	//
	body:      ^Stmt,
	else_stmt: ^Stmt,
}
When_Stmt :: struct {
	cond:      ^Expr,
	//
	body:      ^Stmt,
	else_stmt: ^Stmt,
}
Return_Stmt :: struct {
	results: []^Expr,
}
Defer_Stmt :: struct {
	stmt: ^Stmt,
}
Loop_Stmt :: struct {
	label: ^Expr,
	//
	init:  ^Stmt,
	cond:  ^Expr,
	post:  ^Stmt,
	body:  ^Stmt,
}
//range
//inline-range
Case_Clause_Stmt :: struct {
	list: []^Expr,
	body: []^Stmt,
}
Switch_Stmt :: struct {
	label: ^Expr,
	init:  ^Stmt,
	cond:  ^Expr,
	body:  ^Stmt,
}
Using_Stmt :: struct {
	list: []^Expr,
}
//type-switch
//branch

Bad_Decl :: struct {}
Value_Decl :: struct {
	names:  []^Expr,
	type:   ^Type,
	values: []^Expr,
}
//package
Import_Decl :: struct {
	path:      string, // "core:fmt"
	full_path: string, // c:/xyz/odin/core/fmt
}
Foriegn_Import_Decl :: struct {
	name:  ^Ident_Expr,
	paths: []string, // full-path
}
Foriegn_Block_Decl :: struct {
	target: ^Expr,
	body:   ^Stmt,
}
/////////////////////////////////////////////////////////
Expr :: struct {
	node:  Node,
	value: Any_Expr,
}
Any_Expr :: union {
	^Bad_Expr,
	^Ident_Expr,
	^Basic_Lit_Expr,
	^Proc_Lit_Expr,
	^Comp_Lit_Expr,
	^Ellipsis_Expr,
	^Unary_Expr,
	^Binary_Expr,
	^Paren_Expr,
	^Index_Expr,
	^Deref_Expr,
	^Call_Expr,
	^Type_Cast_Expr,
	//
	Any_Type,
}

Bad_Expr :: struct {}
Ident_Expr :: struct {
	name: string,
}
Basic_Lit_Expr :: struct {
	tok: Token,
}
Proc_Lit_Expr :: struct {
	type: ^Proc_Type,
	body: ^Stmt,
}
Comp_Lit_Expr :: struct {
	type:  ^Type,
	elems: []^Expr,
	//
	open:  Pos,
	close: Pos,
}
Ellipsis_Expr :: struct {
	expr: ^Expr,
}
Unary_Expr :: struct {
	op:   Token,
	expr: ^Expr,
}
Binary_Expr :: struct {
	left:  ^Expr,
	right: ^Expr,
	op:    Token,
}
Paren_Expr :: struct {
	expr: ^Expr,
}
Index_Expr :: struct {
	expr:  ^Expr,
	index: ^Expr,
}
Deref_Expr :: struct {
	op:   Token,
	expr: ^Expr,
}
// Slice_Expr :: struct {}
// Matrix_Index_Expr :: struct {}
Call_Expr :: struct {
	expr: ^Expr,
	args: []^Expr,
}
// Field_Value_Expr :: struct {}
// Ternerary_If_Expr :: struct {}
// Ternerary_When_Expr :: struct {}
// Or_Else_Expr :: struct {}
// Or_Return_Expr :: struct {}
// Or_Branch_Expr :: struct {}
Type_Cast_Expr :: struct {
	expr:     ^Expr,
	new_type: ^Type,
}

Type :: struct {
	node:  Node,
	value: Any_Type,
}
Any_Type :: union {
	^Typeid_Type,
	^Distinct_Type,
	^Proc_Type,
	^Pointer_Type,
	^Multi_Pointer_Type,
	^Array_Type,
	^Dynamic_Array_Type,
	^Struct_Type,
	^Union_Type,
	// ^Bit_Set_Type,
	// ^Bit_Field_Type,
	// ^Map_Type,
	// ^Matrix_Type,
}
Typeid_Type :: struct {
	tok: Token,
	// specialization: ^Expr,
}
Distinct_Type :: struct {
	type: ^Type,
}
Proc_Type :: struct {
	params:  []^Field,
	results: []Field,
	//diverging, convention, generic, tags
}
Pointer_Type :: struct {
	expr: ^Expr,
}
Multi_Pointer_Type :: struct {
	expr: ^Expr,
}
Array_Type :: struct {
	expr: ^Expr,
	len:  ^Expr, // Ellipsis node for [?]T arrray types, nil for slice types
}
Dynamic_Array_Type :: struct {
	expr: ^Expr,
}
Struct_Type :: struct {
	fields: []^Field,
}
Union_Type :: struct {
	variants: []^Type,
}
Enum_Type :: struct {
	fields:    []^Expr,
	base_type: ^Type,
}
// Bit_Set_Type :: struct {}
// Bit_Field_Type :: struct {}
// Map_Type :: struct {}
// Matrix_Type :: struct {}
