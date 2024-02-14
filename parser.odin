package parser

import "core:fmt"
import "core:strings"

error :: proc(p: ^Parser, pos: Pos, msg: string, args: ..any) {
	sb := strings.builder_make()
	strings.write_string(&sb, fmt.tprintf("%s(%d:%d): ", pos.file, pos.line, pos.col))
	strings.write_string(&sb, fmt.tprintf(msg, args))
	str := strings.to_string(sb)
	append(&p.errors, Error{pos, str})
	fmt.eprintln(str)
}

Error :: struct {
	at:  Pos,
	msg: string,
}
Warning :: distinct Error
Parser :: struct {
	tokenizer:  Tokenizer,
	tokens:     [dynamic]Token,
	current_i:  int,
	//
	errors:     [dynamic]Error,
	warnings:   [dynamic]Warning,
	//
	expr_level: int,
	//
	file:       File,
}
at_eof :: proc(p: ^Parser) -> bool {
	return p.current_i >= len(p.tokens)
}
current_token :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current_i]
}
prev_token :: proc(p: ^Parser) -> Token {
	return p.tokens[p.current_i - 1]
}
parse_file :: proc(path: string) -> (parser: Parser) {
	parser = Parser {
		tokenizer = init_tokenizer(path),
	}
	p := &parser
	advance_token(p)
	stmts := [dynamic]Stmt{}
	imports := [dynamic]^Import_Decl{}
	for !at_eof(p) {
		stmt := statement(p)

		#partial switch v in stmt.value {
		case (^Empty_Stmt):
		case (^Import_Decl):
			append(&imports, v)
		case (^Foriegn_Import_Decl):
			// append(&imports, v)
			unimplemented("need in imports?")
		case:
			append(&stmts, stmt)
		}
	}

	return
}


print_token :: proc(t: Token, newline: bool = false) {
	fmt.printf("%v <%v>%v", t.kind, t.text, newline ? "\n" : "")
}

advance_token :: proc(p: ^Parser) -> Token {
	t := scan(&p.tokenizer)
	append(&p.tokens, t)
	return t
}
peek_token :: proc(p: ^Parser, lookahead := 1) -> Token {
	tmp_tok := p.tokenizer
	tok: Token
	for i in 0 ..< lookahead {
		if tmp_tok.offset >= len(tmp_tok.src) {
			return Token{pos = get_pos(&tmp_tok), kind = .EOF}
		}
		tok = scan(&tmp_tok)
	}
	return tok
}
expect_token :: proc(p: ^Parser, kind: Token_Kind) -> Token {
	t := advance_token(p)
	fmt.assertf(kind == t.kind, "Expected %v, got %v (Line: %v)", kind, t.kind, t.pos.line)
	return t
}
match_token :: proc(p: ^Parser, kind: Token_Kind) -> bool {
	if kind == peek_token(p).kind {
		advance_token(p)
		return true
	}
	return false
}
parse_assert :: proc(
	p: ^Parser,
	cond: bool,
	fmt_str: string,
	args: ..any,
	loc := #caller_location,
) {
	if !cond {
		fmt.printf(fmt_str, ..args)
		parse_fail(p)
	}
}
parse_fail :: proc(p: ^Parser) -> ! {
	fmt.println("Parser Error: Preceding Tokens")
	n := 5
	for i := max(0, p.current_i - n); i < p.current_i; i += 1 {
		print_token(p.tokens[i], true)
	}
	print_token(p.tokens[p.current_i])
	fmt.print(" <-- Current Token\n")
	for i in 0 ..< n {
		tok := advance_token(p)
		print_token(tok, true)
		if advance_token(p).kind == .EOF {break}
	}
	panic("Fatal Parse Error")
}

////////////////////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
// Statement =
// 	Declaration | LabeledStmt | SimpleStmt |
// 	GoStmt | ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
// 	FallthroughStmt | Block | IfStmt | SwitchStmt | SelectStmt | ForStmt |
// 	DeferStmt .

// SimpleStmt = EmptyStmt | ExpressionStmt | SendStmt | IncDecStmt | Assignment | ShortVarDecl .

statement :: proc(p: ^Parser) -> Stmt {
	// prefix_directives: [dynamic]string
	// for match_token(p, .Hash) {
	// 	append(&prefix_directives, expect_token(p, .String).text)
	// }
	tok := current_token(p)
	// label: ^Token
	// if tok.kind == .Ident && peek_token(p).kind == .Colon {
	// 	label = &p.tokens[p.current_i]
	// 	advance_token(p)
	// 	advance_token(p)
	// }

	#partial switch tok.kind {
	case .Proc, .Ident, .Integer, .Float, .Imaginary, .Rune, .String, .Open_Paren, .Pointer, .Add, .Sub, .Xor, .Not, .And:
		return simple_stmt(p)
	case .Import:
		return import_decl(p)
	case .Open_Brace:
		return block_stmt(p)
	case .Semicolon:
		return empty_stmt(p)
	case .If:
		return if_stmt(p)
	case .Loop:
		return loop_stmt(p)
	case:
		error(p, tok.pos, "unhandled token %v (%v)", tok.kind, tok.text)
	}
	return {}
}
// statements:
decl_stmt :: proc(p: ^Parser) {}
// labeled_stmt :: proc(p: ^Parser) {}
return_stmt :: proc(p: ^Parser) {}
break_stmt :: proc(p: ^Parser) {}
continue_stmt :: proc(p: ^Parser) {}
fallthrough_stmt :: proc(p: ^Parser) {}
block_stmt :: proc(p: ^Parser) -> Stmt {
	unimplemented()
}
if_stmt :: proc(p: ^Parser) -> Stmt {
	unimplemented()
}
switch_stmt :: proc(p: ^Parser) {}
loop_stmt :: proc(p: ^Parser) -> Stmt {
	unimplemented()
}
defer_stmt :: proc(p: ^Parser) {}
simple_stmt :: proc(p: ^Parser) -> Stmt {
	start := current_token(p)
	lhs := expression_list(p)
	if len(lhs) == 0 {error(p, start.pos, "no lhs in simple-stmt")}
	op := current_token(p)

	switch {
	//  decls:
	case op.kind == .Colon:
		value_decl(p, lhs)
	// assignments of existing:
	case is_assignment_operator(op.kind):
		advance_token(p)
		rhs := expression_list(p, true)
		if len(rhs) == 0 {
			error(p, op.pos, "no rhs in assignment")
		}
		stmt := new(Assign_Stmt)
		stmt.op = op
		stmt.lhs = lhs
		stmt.rhs = rhs
		return into_stmt(p, start, prev_token(p), stmt)
	case op.kind == .In:
		unimplemented()


	}
	unimplemented()
}
// Simple-Statements:
empty_stmt :: proc(p: ^Parser) -> Stmt {
	start := expect_token(p, .Semicolon)
	es := new(Empty_Stmt)
	return into_stmt(p, start, start, es)
}
expr_stmt :: proc(p: ^Parser) {}
assign_stmt :: proc(p: ^Parser) {}
inc_dec_stmt :: proc(p: ^Parser) {}

// Declaration   = ConstDecl | TypeDecl | VarDecl | ImportDecl
// Decls:
type_decl :: proc(p: ^Parser) {} 	// expand to struct,union,enum,distinct
value_decl :: proc(p: ^Parser, lhs: []^Expr) -> Stmt {
	unimplemented()
}
proc_decl :: proc(p: ^Parser) {}
method_decl :: proc(p: ^Parser) {} 	// add target of method?
import_decl :: proc(p: ^Parser) -> Stmt {
	start := expect_token(p, .Import)
	rel_path := expect_token(p, .String)
	id := new(Import_Decl)
	id.path = rel_path.text
	return into_stmt(p, start, rel_path, id)
}

into_stmt :: proc(p: ^Parser, start_token: Token, end_token: Token, val: Any_Stmt) -> Stmt {
	start := start_token.pos
	end := end_token.pos
	end.col = len(end_token.text) // line breaks..?
	stmt := Stmt {
		node = Node{span = {start, end}},
		value = val,
	}
	stmt.node.start = start
	return stmt
}

// Expression = UnaryExpr | Expression binary_op Expression .
// UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
// binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
// rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
// add_op     = "+" | "-" | "|" | "^" .
// mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
// unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .

// PrimaryExpr =
// 	Operand |
// 	Conversion |
// 	MethodExpr |
// 	PrimaryExpr Selector |
// 	PrimaryExpr Index |
// 	PrimaryExpr Slice |
// 	PrimaryExpr TypeAssertion |
// 	PrimaryExpr Arguments .

// Selector       = "." identifier .
// Index          = "[" Expression [ "," ] "]" .
// Slice          = "[" [ Expression ] ":" [ Expression ] "]" |
//                  "[" [ Expression ] ":" Expression ":" Expression "]" .
// TypeAssertion  = "." "(" Type ")" .
// Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .

// Exprs:
expression_list :: proc(p: ^Parser, rhs := false) -> []^Expr {
	exprs: [dynamic]^Expr
	for !at_eof(p) {
		expr := expression(p)
		append(&exprs, expr)
		if current_token(p).kind != .Comma {break}
		expect_token(p, .Comma)
	}
	return exprs[:]
}
expression :: proc(p: ^Parser, rhs := false) -> ^Expr {
	return binary_expr(p, 1, rhs)
}
//
binary_expr :: proc(p: ^Parser, prec_in: int, rhs := false) -> ^Expr {
	start := current_token(p)
	expr := unary_expr(p)
	if expr == nil {
		return into_expr(p, start, p.tokens[p.current_i - 1], new(Bad_Expr))
	}
	for prec := token_precedence(p, current_token(p).kind); prec > prec_in; prec -= 1 {
		loop: for {
			op := current_token(p)
			op_prec := token_precedence(p, op.kind)
			if op_prec != prec {break loop}
			right := binary_expr(p, prec + 1)
			if right == nil {
				error(p, op.pos, "Expected expression on rhs of binary operator (%v)", op.text)
			}
			be := new(Binary_Expr)
			be.left = expr
			be.op = op
			be.right = right
			expr.value = be
			expr.node.end = end_pos(prev_token(p))
		}
	}
	return expr
}

unary_expr :: proc(p: ^Parser, rhs := false) -> ^Expr {
	tok := current_token(p)
	#partial switch tok.kind {
	case .Transmute, .Cast:
		unimplemented()
	case .Auto:
		unimplemented()
	case .Add, .Sub, .Not, .Xor, .And:
		op := advance_token(p)
		expr := unary_expr(p)
		ue := new(Unary_Expr)
		ue.op = op
		ue.expr = expr
		return into_expr(p, tok, prev_token(p), ue)
	case .Period:
		unimplemented("Implicit selector")
	case .Open_Brace:
		// needs LHS flag
		unimplemented("comp-lits")
	case .Open_Paren:
		return paren_expr(p)
	}
	op := operand_expr(p)
	unimplemented()

}

atom_expr :: proc(p: ^Parser, rhs := false) -> ^Expr {
	expr: ^Expr
	tok := current_token(p)
	#partial switch tok.kind {
	case .Ident:
		id_tok := expect_token(p, .Ident)
		id := new(Ident_Expr)
		id.name = id_tok.text
		expr = into_expr(p, tok, prev_token(p), id)
	case .Integer, .Float, .Imaginary, .Rune, .String:
		bl := new(Basic_Lit_Expr)
		bl.tok = advance_token(p)
		expr = into_expr(p, tok, prev_token(p), bl)
	case .Open_Brace:
		if rhs {
			expr = compound_lit_expr(p, nil)
		}
	case .Open_Paren:
		pe := new(Paren_Expr)
		start := expect_token(p, .Open_Paren)
		p.expr_level += 1
		pe.expr = expression(p, true)
		p.expr_level -= 1
		end := expect_token(p, .Close_Paren)
		expr = into_expr(p, start, end, pe)
	case .Hash:
		unimplemented("directives")
	case .Proc:
		unimplemented("proc?")
	case .Pointer:
		ptr := expect_token(p, .Pointer)
		pt := new(Pointer_Type)
		pt.expr = type_expr(p)
		at: Any_Type = pt
		expr = into_expr(p, tok, ptr, at)
	}
	unimplemented()
}

paren_expr :: proc(p: ^Parser) -> ^Expr {
	open := expect_token(p, .Open_Paren)
	p.expr_level += 1
	expr := expression(p)
	p.expr_level -= 1
	close := expect_token(p, .Close_Paren)
	pe := new(Paren_Expr)
	pe.expr = expr
	return into_expr(p, open, close, pe)
}

compound_lit_expr :: proc(p: ^Parser, type: ^Expr) -> ^Expr {
	unimplemented()
}
// literals:
// Literal     = BasicLit | CompositeLit | FunctionLit .
// BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
literal :: proc(p: ^Parser) {}

into_expr :: proc(p: ^Parser, start_token: Token, end_token: Token, val: Any_Expr) -> ^Expr {
	start := start_token.pos
	end := end_pos(end_token)
	expr := new(Expr)
	expr.node = Node{Span{start, end}}
	expr.value = val
	return expr
}


end_pos :: proc(tok: Token) -> Pos {
	pos := tok.pos
	pos.offset += len(tok.text)

	if tok.kind == .Comment {
		if tok.text[:2] != "/*" {
			pos.col += len(tok.text)
		} else {
			for i := 0; i < len(tok.text); i += 1 {
				c := tok.text[i]
				if c == '\n' {
					pos.line += 1
					pos.col = 1
				} else {
					pos.col += 1
				}
			}
		}
	} else {
		pos.col += len(tok.text)
	}
	return pos
}


token_precedence :: proc(p: ^Parser, kind: Token_Kind) -> int {
	#partial switch kind {
	case .Question, .If, .When, .Or_Else:
		return 1
	case .Ellipsis, .Range_Half, .Range_Full:
		// if !p.allow_range {
		// 	return 0
		// }
		// return 2
		unimplemented()
	case .Cmp_Or:
		return 3
	case .Cmp_And:
		return 4
	case .Cmp_Eq, .Not_Eq, .Lt, .Gt, .Lt_Eq, .Gt_Eq:
		return 5
	case .In, .Not_In:
		// if p.expr_level < 0 && !p.allow_in_expr {
		// 	return 0
		// }
		// fallthrough
		unimplemented()
	case .Add, .Sub, .Or, .Xor:
		return 6
	case .Mul, .Quo, .Mod, .Mod_Mod, .And, .And_Not, .Shl, .Shr:
		return 7
	}
	return 0
}

is_assignment_operator :: proc(kind: Token_Kind) -> bool {
	return .B_Assign_Op_Begin < kind && .B_Assign_Op_End > kind || kind == .Eq
}
