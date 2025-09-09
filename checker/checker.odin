package checker

import llvm "../llvm"
import "../parser"
import "core:fmt"
import "core:odin/ast"
import "core:odin/tokenizer"

// Entity - represents a named language construct (variable, function, type, etc.)
Entity :: struct {
	kind:  EntityKind,
	name:  string,
	type:  ^Type,
	decl:  ^ast.Stmt, // The declaration that created this entity
	scope: ^Scope, // Scope where this entity is defined
	state: EntityState,
}

EntityKind :: enum {
	INVALID,
	VARIABLE,
	CONSTANT,
	PROCEDURE,
	TYPE_NAME,
	PACKAGE,
}

EntityState :: enum {
	UNRESOLVED,
	IN_PROGRESS,
	RESOLVED,
}

// Import the new type system from types.odin
// The Type structure and related types are now defined in types.odin

// Scope - represents lexical scoping with symbol tables
Scope :: struct {
	parent:   ^Scope,
	entities: map[string]^Entity,
	kind:     ScopeKind,
}

ScopeKind :: enum {
	UNIVERSE, // Built-in types and functions
	PACKAGE, // Package-level scope
	FILE, // File-level scope
	PROCEDURE, // Function scope
	BLOCK, // Block scope
}

// CheckerInfo - central compilation state (like Odin's CheckerInfo)
CheckerInfo :: struct {
	// All entities discovered during checking
	entities:        [dynamic]^Entity,

	// Type information for expressions
	expr_info:       map[^ast.Expr]ExprInfo,

	// Scopes
	universe_scope:  ^Scope, // Built-in scope
	package_scope:   ^Scope, // Package-level scope
	current_scope:   ^Scope, // Currently active scope

	// Built-in types (for easy access)
	builtin_int:     ^Type,
	builtin_float:   ^Type,
	builtin_bool:    ^Type,
	builtin_void:    ^Type,

	// Deferred procedure checking (Phase 2 - following Odin's pattern)
	procedure_queue: [dynamic]^Entity, // Procedures to check bodies later
}

// ExprInfo - information about an expression after type checking
ExprInfo :: struct {
	type:  ^Type,
	mode:  ExprMode,
	value: ExprValue, // For constant expressions
}

ExprMode :: enum {
	INVALID,
	VALUE, // Regular value expression
	VARIABLE, // Assignable lvalue
	CONSTANT, // Compile-time constant
	TYPE, // Type expression
}

ExprValue :: union {
	int,
	f64,
	bool,
	string,
}

// CheckResult - results from type checking (much richer than before)
CheckResult :: struct {
	info:          ^CheckerInfo, // Complete compilation state
	success:       bool,
	error_count:   int, // Number of errors encountered
	warning_count: int, // Number of warnings encountered
}

// Initialize checker info with built-in types and universe scope
checker_info_init :: proc() -> ^CheckerInfo {
	info := new(CheckerInfo)

	// Create universe scope with built-in types
	info.universe_scope = new(Scope)
	info.universe_scope.kind = .UNIVERSE
	info.universe_scope.entities = make(map[string]^Entity)

	// Create built-in types
	// Create built-in types using the new type system
	info.builtin_int = make_type_basic(.i32, 4, "i32", {.Integer})
	info.builtin_float = make_type_basic(.f32, 4, "f32", {.Float})
	info.builtin_bool = make_type_basic(.bool, 1, "bool", {.Boolean})
	
	// Create a void type (size 0)
	info.builtin_void = make_type(.Basic)
	info.builtin_void.variant = TypeBasic{
		kind = .Invalid,
		size = 0,
		name = "void",
		flags = {},
	}

	// Add built-in type entities to universe scope
	add_builtin_entity :: proc(info: ^CheckerInfo, name: string, type: ^Type) {
		entity := new(Entity)
		entity.kind = .TYPE_NAME
		entity.name = name
		entity.type = type
		entity.state = .RESOLVED
		entity.scope = info.universe_scope

		info.universe_scope.entities[name] = entity
		append(&info.entities, entity)
	}

	add_builtin_entity(info, "int", info.builtin_int)
	add_builtin_entity(info, "float", info.builtin_float)
	add_builtin_entity(info, "bool", info.builtin_bool)
	add_builtin_entity(info, "void", info.builtin_void)

	// Initialize other data structures
	info.expr_info = make(map[^ast.Expr]ExprInfo)

	return info
}

// Create a new scope as child of current scope
push_scope :: proc(info: ^CheckerInfo, kind: ScopeKind) -> ^Scope {
	scope := new(Scope)
	scope.kind = kind
	scope.parent = info.current_scope
	scope.entities = make(map[string]^Entity)

	info.current_scope = scope
	return scope
}

// Return to parent scope
pop_scope :: proc(info: ^CheckerInfo) {
	if info.current_scope.parent != nil {
		info.current_scope = info.current_scope.parent
	}
}

// Look up an entity by name in current scope chain
lookup_entity :: proc(info: ^CheckerInfo, name: string) -> ^Entity {
	scope := info.current_scope
	for scope != nil {
		if entity, exists := scope.entities[name]; exists {
			return entity
		}
		scope = scope.parent
	}
	return nil
}

// Add entity to current scope
add_entity :: proc(info: ^CheckerInfo, entity: ^Entity) -> bool {
	// Check for redeclaration in current scope
	if existing_entity, exists := info.current_scope.entities[entity.name]; exists {
		// Try to get position from the declaration if available
		pos := tokenizer.Pos{}
		error(pos, "Redeclaration of '%s'", entity.name)
		return false
	}

	info.current_scope.entities[entity.name] = entity
	entity.scope = info.current_scope
	append(&info.entities, entity)
	return true
}

// Queue a procedure for deferred body checking (Phase 2)
check_procedure_later :: proc(info: ^CheckerInfo, proc_entity: ^Entity) {
	assert(proc_entity.kind == .PROCEDURE, "Only procedures can be queued for deferred checking")
	append(&info.procedure_queue, proc_entity)
	fmt.printf("  Queued procedure '%s' for body checking\n", proc_entity.name)
}

// Phase 2: Check all queued procedure bodies (following Odin's pattern)
check_procedure_bodies :: proc(info: ^CheckerInfo) -> bool {
	fmt.printf("\n--- Phase 2: Checking procedure bodies ---\n")
	fmt.printf("Processing %d queued procedures...\n", len(info.procedure_queue))

	for proc_entity in info.procedure_queue {
		if !check_procedure_body(info, proc_entity) {
			return false
		}
	}

	fmt.printf("Phase 2 complete\n")
	return true
}

// Check a single procedure body
check_procedure_body :: proc(info: ^CheckerInfo, proc_entity: ^Entity) -> bool {
	fmt.printf("  Checking body of procedure '%s'\n", proc_entity.name)

	// Get the procedure AST from the entity's declaration
	value_decl: ^ast.Value_Decl
	if vd, ok := proc_entity.decl.derived_stmt.(^ast.Value_Decl); ok {
		value_decl = vd
	} else {
		error(tokenizer.Pos{}, "Procedure entity declaration is not a Value_Decl")
		return false
	}

	proc_lit: ^ast.Proc_Lit

	// Find the procedure literal in the value declaration
	for value in value_decl.values {
		if pl, ok := value.derived_expr.(^ast.Proc_Lit); ok {
			proc_lit = pl
			break
		}
	}

	if proc_lit == nil {
		error(tokenizer.Pos{}, "Could not find procedure literal for '%s'", proc_entity.name)
		return false
	}

	// Create procedure scope
	proc_scope := push_scope(info, .PROCEDURE)
	defer pop_scope(info)

	// Add procedure parameters to scope
	if proc_lit.type != nil && len(proc_lit.type.params.list) > 0 {
		for param_group in proc_lit.type.params.list {
			param_type := resolve_type_spec(info, param_group.type)
			for param_name in param_group.names {
				// Get parameter name
				param_name_str := ""
				if ident, ok := param_name.derived_expr.(^ast.Ident); ok {
					param_name_str = ident.name
				}
				
				if param_name_str != "" {
					// Create parameter entity
					param_entity := new(Entity)
					param_entity.name = param_name_str
					param_entity.kind = .VARIABLE  // Parameters are like local variables
					param_entity.type = param_type
					param_entity.state = .RESOLVED
					param_entity.decl = nil  // Parameters don't have separate declarations
					
					// Add to scope
					add_entity(info, param_entity)
					
					fmt.printf("    Added parameter: %s : %v\n", param_name_str, param_type.kind)
				}
			}
		}
	}

	// Check procedure body if it exists
	if proc_lit.body != nil {
		if block_stmt, ok := proc_lit.body.derived.(^ast.Block_Stmt); ok {
			for stmt in block_stmt.stmts {
				if !check_statement(info, stmt) {
					return false
				}
			}
		}
	}

	fmt.printf("    Procedure '%s' body checked successfully\n", proc_entity.name)
	return true
}

// Step 2: Type check the AST (following Odin's two-phase pattern)
check :: proc(parse_result: parser.ParseResult) -> CheckResult {
	fmt.printf("\n=== TYPE CHECK ===\n")
	fmt.printf("Type checking AST...\n")

	// Initialize error collector
	init_error_collector()

	if !parse_result.success {
		error(tokenizer.Pos{}, "Cannot type check: parsing failed")
		return CheckResult {
			success = false,
			error_count = global_error_collector.error_count,
			warning_count = global_error_collector.warning_count,
		}
	}

	// Initialize checker info
	info := checker_info_init()

	// Create package scope
	info.package_scope = push_scope(info, .PACKAGE)

	// Phase 1: Check declarations (collect all procedure signatures)
	fmt.printf("--- Phase 1: Checking declarations ---\n")
	if parse_result.file != nil {
		success := check_file(info, parse_result.file)
		if !success {
			fmt.eprintln("Phase 1: Declaration checking failed")
			return CheckResult{success = false}
		}
	}

	fmt.printf(
		"Phase 1 complete - found %d entities, %d procedures queued\n",
		len(info.entities),
		len(info.procedure_queue),
	)

	// Phase 2: Check procedure bodies (following Odin's pattern)
	if !check_procedure_bodies(info) {
		fmt.eprintln("Phase 2: Procedure body checking failed")
		return CheckResult{success = false}
	}

	fmt.printf("Type checking complete - found %d entities\n", len(info.entities))

	// Print discovered entities
	for entity in info.entities {
		fmt.printf("  Entity: %s (%v) in %v scope\n", entity.name, entity.kind, entity.scope.kind)
	}

	// Print any errors/warnings
	if any_errors() || any_warnings() {
		print_all_errors()
	}

	success := !any_errors()
	return CheckResult {
		info = info,
		success = success,
		error_count = global_error_collector.error_count,
		warning_count = global_error_collector.warning_count,
	}
}

// Check a file (find and process all declarations)
check_file :: proc(info: ^CheckerInfo, file: ^ast.File) -> bool {
	fmt.printf("Checking file: %s\n", file.fullpath)

	// Process all top-level declarations
	for decl_stmt in file.decls {
		if !check_declaration(info, decl_stmt) {
			return false
		}
	}

	return true
}

// Check a declaration and add entities to symbol table
check_declaration :: proc(info: ^CheckerInfo, decl_stmt: ^ast.Stmt) -> bool {
	#partial switch decl in decl_stmt.derived {
	case ^ast.Value_Decl:
		return check_value_declaration(info, decl)
	case ^ast.Package_Decl:
		// Package declarations don't create entities in our simple model
		return true
	case:
		fmt.printf("  Unhandled declaration type: %T\n", decl)
		return true
	}
}

// Check value declarations (variables, constants, procedures)
check_value_declaration :: proc(info: ^CheckerInfo, decl: ^ast.Value_Decl) -> bool {
	if len(decl.names) != len(decl.values) {
		error(tokenizer.Pos{}, "Name/value count mismatch in declaration")
		return false
	}

	// Process each name/value pair
	for i in 0 ..< len(decl.names) {
		name_expr := decl.names[i]
		value_expr := decl.values[i]

		// Get the name
		name: string
		if ident, ok := name_expr.derived.(^ast.Ident); ok {
			name = ident.name
		} else {
			error(tokenizer.Pos{}, "Declaration name is not identifier")
			return false
		}

		// Determine entity kind and type based on value
		entity := new(Entity)
		entity.name = name
		entity.decl = cast(^ast.Stmt)decl
		entity.state = .IN_PROGRESS

		// Check if it's a procedure
		if proc_lit, ok := value_expr.derived.(^ast.Proc_Lit); ok {
			entity.kind = .PROCEDURE
			
			// Create procedure type using the new system
			proc_type := make_type_proc()
			
			// Create tuple types for params and results
			params_tuple := make_type_tuple()
			results_tuple := make_type_tuple()
			
			param_count := 0
			result_count := 0
			
			// Extract parameter types
			if proc_lit.type != nil {
				// Get parameter types from the Proc_Type
				if len(proc_lit.type.params.list) > 0 {
					// Get the params tuple variant
					params_tuple_variant := &params_tuple.variant.(TypeTuple)
					for param_group in proc_lit.type.params.list {
						// Each param group can have multiple names with same type
						param_type := resolve_type_spec(info, param_group.type)
						for _ in param_group.names {
							append(&params_tuple_variant.types, param_type)
							param_count += 1
						}
					}
				}
				
				// Get return type(s)
				if proc_lit.type.results != nil {
					// Get the results tuple variant
					results_tuple_variant := &results_tuple.variant.(TypeTuple)
					// Handle return types
					for result_group in proc_lit.type.results.list {
						result_type := resolve_type_spec(info, result_group.type)
						if len(result_group.names) > 0 {
							for _ in result_group.names {
								append(&results_tuple_variant.types, result_type)
								result_count += 1
							}
						} else {
							// Unnamed result
							append(&results_tuple_variant.types, result_type)
							result_count += 1
						}
					}
				}
			}
			
			// Set the procedure type info
			proc_type.variant = TypeProc{
				params = params_tuple,
				results = results_tuple,
				param_count = param_count,
				result_count = result_count,
			}
			entity.type = proc_type
			fmt.printf("  Found procedure: %s\n", name)

			// Add to current scope first
			if !add_entity(info, entity) {
				return false
			}

			// Queue for deferred body checking (Phase 2)
			check_procedure_later(info, entity)
			continue // Skip the regular add_entity call at the bottom
		} else {
			// Regular value - infer type from expression
			entity.kind = .VARIABLE
			entity.type = check_expression(info, value_expr)
			if entity.type == nil {
				error(tokenizer.Pos{}, "Could not determine type for '%s'", name)
				return false
			}
			fmt.printf("  Found variable: %s : %v\n", name, entity.type.kind)
		}

		entity.state = .RESOLVED

		// Add to current scope
		if !add_entity(info, entity) {
			return false
		}
	}

	return true
}

// Check a statement within a procedure body
check_statement :: proc(info: ^CheckerInfo, stmt: ^ast.Stmt) -> bool {
	#partial switch s in stmt.derived_stmt {
	case ^ast.Value_Decl:
		// Local variable declaration (a := 1, b := 2, c := a + b)
		return check_local_declaration(info, s)
	case ^ast.Expr_Stmt:
		// Expression statement (standalone expression)
		result_type := check_expression(info, s.expr)
		return result_type != nil
	case ^ast.Assign_Stmt:
		// Assignment statement (x = y)
		return check_assignment_statement(info, s)
	case ^ast.Block_Stmt:
		// Nested block - create new scope
		block_scope := push_scope(info, .BLOCK)
		defer pop_scope(info)

		for nested_stmt in s.stmts {
			if !check_statement(info, nested_stmt) {
				return false
			}
		}
		return true
	case ^ast.Return_Stmt:
		// Return statement - check return expressions
		for result_expr in s.results {
			result_type := check_expression(info, result_expr)
			if result_type == nil {
				fmt.eprintf("ERROR: Could not determine type for return expression\n")
				return false
			}
		}
		return true
	case:
		fmt.printf("    Unhandled statement type: %T\n", s)
		return true // Don't fail on unhandled statements for now
	}
}

// Check local variable declarations inside procedures
check_local_declaration :: proc(info: ^CheckerInfo, decl: ^ast.Value_Decl) -> bool {
	fmt.printf("    Checking local declaration\n")

	if len(decl.names) != len(decl.values) {
		fmt.eprintln("ERROR: Name/value count mismatch in local declaration")
		return false
	}

	// Process each name/value pair
	for i in 0 ..< len(decl.names) {
		name_expr := decl.names[i]
		value_expr := decl.values[i]

		// Get the name
		name: string
		if ident, ok := name_expr.derived.(^ast.Ident); ok {
			name = ident.name
		} else {
			fmt.eprintln("ERROR: Local declaration name is not identifier")
			return false
		}

		// Create local variable entity
		entity := new(Entity)
		entity.name = name
		entity.kind = .VARIABLE
		entity.decl = cast(^ast.Stmt)decl
		entity.state = .IN_PROGRESS

		// Type inference from RHS expression
		entity.type = check_expression(info, value_expr)
		if entity.type == nil {
			fmt.eprintf("ERROR: Could not determine type for local variable '%s'\n", name)
			return false
		}

		entity.state = .RESOLVED

		fmt.printf("      Local variable: %s : %v\n", name, entity.type.kind)

		// Add to current scope (procedure or block scope)
		if !add_entity(info, entity) {
			return false
		}
	}

	return true
}

// Check assignment statements
check_assignment_statement :: proc(info: ^CheckerInfo, assign: ^ast.Assign_Stmt) -> bool {
	fmt.printf("    Checking assignment statement\n")

	// Check that we have equal numbers of LHS and RHS expressions
	if len(assign.lhs) != len(assign.rhs) {
		fmt.eprintln("ERROR: Assignment count mismatch")
		return false
	}

	// Type-check each LHS/RHS pair
	for i in 0 ..< len(assign.lhs) {
		lhs_type := check_expression(info, assign.lhs[i])
		rhs_type := check_expression(info, assign.rhs[i])

		if lhs_type == nil || rhs_type == nil {
			return false
		}

		// Type compatibility check (simplified)
		if lhs_type != rhs_type {
			fmt.eprintln("ERROR: Type mismatch in assignment")
			return false
		}
	}

	return true
}

// Check an expression and return its type
check_expression :: proc(info: ^CheckerInfo, expr: ^ast.Expr) -> ^Type {
	#partial switch e in expr.derived {
	case ^ast.Basic_Lit:
		// Literal values
		#partial switch e.tok.kind {
		case .Integer:
			return info.builtin_int
		case .Float:
			return info.builtin_float
		case:
			fmt.printf("    Unhandled literal type: %v\n", e.tok.kind)
			return nil
		}
	case ^ast.Ident:
		// Variable reference - look up in symbol table
		if entity := lookup_entity(info, e.name); entity != nil {
			return entity.type
		} else {
			fmt.eprintf("ERROR: Undefined identifier '%s'\n", e.name)
			return nil
		}
	case ^ast.Binary_Expr:
		// Binary operations - check operand types
		lhs_type := check_expression(info, e.left)
		rhs_type := check_expression(info, e.right)

		if lhs_type == nil || rhs_type == nil {
			return nil
		}

		// For now, assume both operands must have same type
		if lhs_type != rhs_type {
			fmt.eprintln("ERROR: Type mismatch in binary expression")
			return nil
		}

		// Result type depends on operation
		#partial switch e.op.kind {
		case .Add, .Sub, .Mul:
			return lhs_type // Arithmetic preserves type
		case:
			fmt.printf("ERROR: Unhandled binary operator: %v\n", e.op.kind)
			return nil
		}
	case ^ast.Call_Expr:
		// Function call expression
		// Get the function being called
		ident, ident_ok := e.expr.derived.(^ast.Ident)
		if ident_ok {
			// Look up the function
			entity := lookup_entity(info, ident.name)
			if entity != nil {
				if entity.kind == .PROCEDURE {
					// Get the actual return type from the procedure type
					if entity.type != nil && entity.type.kind == .Proc {
						proc_type, proc_ok := entity.type.variant.(TypeProc)
						if proc_ok {
							if proc_type.results != nil && proc_type.results.kind == .Tuple {
								tuple, tuple_ok := proc_type.results.variant.(TypeTuple)
								if tuple_ok && len(tuple.types) > 0 {
									// Return the first result type
									return tuple.types[0]
								}
							}
						}
					}
					// Default to int if no type info
					return info.builtin_int
				} else {
					fmt.eprintf("ERROR: '%s' is not a procedure\n", ident.name)
					return nil
				}
			} else {
				fmt.eprintf("ERROR: Undefined function '%s'\n", ident.name)
				return nil
			}
		} else {
			fmt.eprintln("ERROR: Complex function expressions not yet supported")
			return nil
		}
	case:
		fmt.printf("  Unhandled expression type: %T\n", e)
		return nil
	}

	return nil
}

// Resolve a type spec from the AST to our Type structure
resolve_type_spec :: proc(info: ^CheckerInfo, type_expr: ^ast.Expr) -> ^Type {
	if type_expr == nil {
		return info.builtin_void
	}
	
	// Handle identifier types (e.g., "i32", "int", etc.)
	if ident, ok := type_expr.derived.(^ast.Ident); ok {
		switch ident.name {
		case "i32", "int":
			return info.builtin_int
		case "f32", "float":
			return info.builtin_float  
		case "bool":
			return info.builtin_bool
		case "void":
			return info.builtin_void
		case:
			// Look up user-defined types
			if entity := lookup_entity(info, ident.name); entity != nil {
				if entity.kind == .TYPE_NAME {
					return entity.type
				}
			}
			fmt.eprintf("Unknown type: %s\n", ident.name)
			return info.builtin_int // Default to int
		}
	}
	
	// For other type expressions, default to int
	return info.builtin_int
}
