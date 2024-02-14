package parser

Token :: struct {
	kind: Token_Kind,
	text: string,
	pos:  Pos,
}

Pos :: struct {
	file:   string,
	offset: int,
	line:   int,
	col:    int,
}
Token_Kind :: enum {
	Invalid,
	EOF,
	Comment,
	Multi_Comment,
	//
	Ident, // main
	Integer, // 12345
	Float, // 123.45
	Imaginary, // 123.45i
	Rune, // 'a'
	String, // "abc"
	//
	Eq, // =
	Not, // !
	Hash, // #
	At, // @
	Dollar, // $
	Pointer, // ^
	Question, // ?
	Add, // +
	Sub, // -
	Mul, // *
	Quo, // /
	Mod, // %
	Mod_Mod, // %%
	And, // &
	Or, // |
	Xor, // ~
	And_Not, // &~
	Shl, // <<
	Shr, // >>
	Cmp_And, // &&
	Cmp_Or, // ||
	//
	Add_Eq, // +=
	Sub_Eq, // -=
	Mul_Eq, // *=
	Quo_Eq, // /=
	Mod_Eq, // %=
	Mod_Mod_Eq, // %%=
	And_Eq, // &=
	Or_Eq, // |=
	Xor_Eq, // ~=
	And_Not_Eq, // &~=
	Shl_Eq, // <<=
	Shr_Eq, // >>=
	Cmp_And_Eq, // &&=
	Cmp_Or_Eq, // ||=
	//
	Arrow_Right, // ->
	Undef, // ---
	//
	Cmp_Eq, // ==
	Not_Eq, // !=
	Lt, // <
	Gt, // >
	Lt_Eq, // <=
	Gt_Eq, // >=
	//
	Open_Paren, // (
	Close_Paren, // )
	Open_Bracket, // [
	Close_Bracket, // ]
	Open_Brace, // {
	Close_Brace, // }
	Colon, // :
	Semicolon, // ;
	Period, // .
	Comma, // ,
	Ellipsis, // ..
	Range_Half, // ..<
	Range_Full, // ..=
	//
	Import, // import
	Foreign, // foreign
	Package, // package
	Typeid, // typeid
	When, // when
	Where, // where
	If, // if
	Else, // else
	For, // for
	Switch, // switch
	In, // in
	Not_In, // not_in
	Do, // do
	Case, // case
	Break, // break
	Continue, // continue
	Fallthrough, // fallthrough
	Defer, // defer
	Return, // return
	Proc, // proc
	Struct, // struct
	Union, // union
	Enum, // enum
	Bit_Set, // bit_set
	Bit_Field, // bit_field
	Map, // map
	Dynamic, // dynamic
	Auto, // auto_cast
	Cast, // cast
	Transmute, // transmute
	Distinct, // distinct
	Using, // using
	Context, // context
	Or_Else, // or_else
	Or_Return, // or_return
	Or_Break, // or_break
	Or_Continue, // or_continue
	Asm, // asm
	Inline, // inline
	No_Inline, // no_inline
	Matrix, // matrix
	//
}

KEYWORDS := map[string]Token_Kind {
	"import"      = .Import,
	"foreign"     = .Foreign,
	"package"     = .Package,
	"typeid"      = .Typeid,
	"when"        = .When,
	"where"       = .Where,
	"if"          = .If,
	"else"        = .Else,
	"for"         = .For,
	"switch"      = .Switch,
	"in"          = .In,
	"not_in"      = .Not_In,
	"do"          = .Do, // then?
	"case"        = .Case,
	"break"       = .Break,
	"continue"    = .Continue,
	"fallthrough" = .Fallthrough,
	"defer"       = .Defer,
	"return"      = .Return,
	"proc"        = .Proc,
	"struct"      = .Struct,
	"union"       = .Union,
	"enum"        = .Enum,
	"bit_set"     = .Bit_Set,
	"bit_field"   = .Bit_Field,
	"map"         = .Map,
	"dynamic"     = .Dynamic,
	"auto"        = .Auto,
	"cast"        = .Cast,
	"transmute"   = .Transmute,
	"distinct"    = .Distinct,
	"using"       = .Using,
	"or_else"     = .Or_Else,
	"or_return"   = .Or_Return,
	"or_break"    = .Or_Break,
	"or_continue" = .Or_Continue,
	"asm"         = .Asm,
	"inline"      = .Inline,
	"no_inline"   = .No_Inline,
	"matrix"      = .Matrix,
}
