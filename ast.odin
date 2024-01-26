package parser

File :: struct {
	stmts: [dynamic]^Stmt,
	//imports: [dynamic]^Import_Decl,
}


Stmt :: union {}
Expr :: union {}

Ident :: struct {
	name:  string,
	token: Token,
}
