package parser

import "core:fmt"

Parser :: struct {
	tokenizer: Tokenizer,
	tokens:    [dynamic]Token,
	current_i: int,
	//
}

parse_file :: proc(path: string) -> (parser: Parser, file: File) {
	parser = Parser {
		tokenizer = init_tokenizer(path),
	}
	p := &parser

	advance_token(p)

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

statement :: proc(p: ^Parser) {}
// statements:
decl_stmt :: proc(p: ^Parser) {}
labeled_stmt :: proc(p: ^Parser) {}
return_stmt :: proc(p: ^Parser) {}
break_stmt :: proc(p: ^Parser) {}
continue_stmt :: proc(p: ^Parser) {}
fallthrough_stmt :: proc(p: ^Parser) {}
block_stmt :: proc(p: ^Parser) {}
if_stmt :: proc(p: ^Parser) {}
switch_stmt :: proc(p: ^Parser) {}
loop_stmt :: proc(p: ^Parser) {}
defer_stmt :: proc(p: ^Parser) {}
simple_stmt :: proc(p: ^Parser) {}
// Simple-Statements:
empty_stmt :: proc(p: ^Parser) {}
expr_stmt :: proc(p: ^Parser) {}
assign_stmt :: proc(p: ^Parser) {}
inc_dec_stmt :: proc(p: ^Parser) {}

// Declaration   = ConstDecl | TypeDecl | VarDecl | ImportDecl
// Decls:
type_decl :: proc(p: ^Parser) {} 	// expand to struct,union,enum,distinct
variable_decl :: proc(p: ^Parser) {}
proc_decl :: proc(p: ^Parser) {}
method_decl :: proc(p: ^Parser) {} 	// add target of method?
import_decl :: proc(p: ^Parser) {}

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
expression :: proc(p: ^Parser) {}
//
unary_expr :: proc(p: ^Parser) {}
binary_expr :: proc(p: ^Parser) {}
paren_expr :: proc(p: ^Parser) {}


// literals:
// Literal     = BasicLit | CompositeLit | FunctionLit .
// BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
literal :: proc(p: ^Parser) {}
