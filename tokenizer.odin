package parser
import "core:fmt"
import "core:os"
Tokenizer :: struct {
	// Immutable data 
	path:       string,
	src:        string,
	// Tokenizing state 
	ch:         u8,
	offset:     int,
	line_count: int,
}
// todo: move os read out of tokenizer 
init_tokenizer :: proc(path: string) -> Tokenizer {
	src, ok := os.read_entire_file(path)
	fmt.assertf(ok, "Failed to read: %v", path)
	t := Tokenizer {
		src        = string(src),
		path       = path,
		line_count = len(src) > 0 ? 1 : 0,
		ch         = len(src) > 0 ? src[0] : 0,
	}
	return t
}
scan :: proc(t: ^Tokenizer) -> Token {
	skip_whitespace(t)
	if t.offset >= len(t.src) {return Token{kind = .EOF}}
	start := t.offset
	lit: string
	kind := Token_Kind.Invalid
	pos := get_pos(t)
	trivial_parse := true
	switch t.ch {
	case ',':
		kind = .Comma
	case '#':
		kind = .Hash
	case '(':
		kind = .Open_Paren
	case ')':
		kind = .Close_Paren
	case '[':
		kind = .Open_Bracket
	case ']':
		kind = .Close_Bracket
	case '{':
		kind = .Open_Brace
	case '}':
		kind = .Close_Brace
	case ':':
		kind = .Colon
	case ';':
		kind = .Semicolon
	case '?':
		kind = .Question
	case '~':
		kind = .Xor
	case:
		trivial_parse = false
	}
	if trivial_parse {
		advance_char(t)
		lit = t.src[start:t.offset]
		return Token{text = lit, kind = kind, pos = pos}
	}
	///// 
	medium_parse := true
	n_consume := 1
	switch t.ch {
	case '+':
		switch peek_char(t) {
		case:
			kind = .Add
		case '=':
			kind = .Add_Eq
			n_consume += 1
		}
	case '-':
		next := peek_char(t)
		switch peek_char(t) {
		case:
			kind = .Sub
		case '>':
			kind = .Arrow_Right
			n_consume += 1
		case '-':
			if peek_char(t, 2) == '-' {
				kind = .Undef
				n_consume += 2
			} else {
				panic("todo error, got 2 -- need 3")
			}
		case '=':
			kind = .Sub_Eq
			n_consume += 1
		case '0' ..= '9':
			return scan_number(t)
		}
	case '*':
		switch peek_char(t) {
		case:
			kind = .Mul
		case '>':
			kind = .Mul_Eq
			n_consume += 1
		}
	case '/':
		switch peek_char(t) {
		case:
			kind = .Quo
		case '=':
			kind = .Quo_Eq
			n_consume += 1
		case '/':
			kind = .Comment
			for t.ch != '\n' {advance_char(t)}
			end := t.offset
			if t.src[t.offset - 1] == '\r' {
				end -= 1
			}
			lit = t.src[start:end]
			return Token{pos = pos, kind = kind, text = lit}
		case '*':
			advance_char(t)
			kind = .Multi_Comment
			for {
				if t.ch == '*' && peek_char(t) == '/' {
					advance_char(t)
					advance_char(t)
					break
				}
				advance_char(t)
			}
			lit = t.src[start:t.offset]
			return Token{pos = pos, kind = kind, text = lit}
		}
	case '%':
		switch peek_char(t) {
		case:
			kind = .Mod
		case '=':
			kind = .Mod_Eq
			n_consume += 1
		case '%':
			if peek_char(t, 2) == '=' {
				kind = .Mod_Mod_Eq
				n_consume += 2
			} else {
				kind = .Mod_Mod
				n_consume += 1
			}
		}
	case '^':
		kind = .Pointer
	case '|':
		switch peek_char(t) {
		case:
			kind = .Or
		case '=':
			kind = .Or_Eq
			n_consume += 1
		case '|':
			kind = .Cmp_Or
			n_consume += 1
		}
	case '&':
		switch peek_char(t) {
		case:
			kind = .And
		case '=':
			kind = .And_Eq
			n_consume += 1
		case '~':
			kind = .And_Not // shaw says this is retarded 
			n_consume += 1
		case '&':
			kind = .Cmp_And
			n_consume += 1
		}
	case '!':
		switch peek_char(t) {
		case:
			kind = .Not
		case '=':
			kind = .Not_Eq
			n_consume += 1
		}
	case '=':
		switch peek_char(t) {
		case:
			kind = .Eq
		case '=':
			kind = .Cmp_Eq
			n_consume += 1
		}
	case '.':
		switch peek_char(t) {
		case:
			kind = .Period
		case '.':
			switch peek_char(t, 2) {
			case:
				kind = .Ellipsis
				n_consume += 1
			case '<':
				kind = .Range_Half
				n_consume += 2
			case '=':
				kind = .Range_Full
				n_consume += 2
			}
		}
	case '<':
		switch peek_char(t) {
		case:
			kind = .Lt
		case '=':
			kind = .Lt_Eq
			n_consume += 1
		case '<':
			if peek_char(t, 2) == '=' {
				kind = .Shl_Eq
				n_consume += 2
			} else {
				kind = .Shl
				n_consume += 1
			}
		}
	case '>':
		switch peek_char(t) {
		case:
			kind = .Gt
		case '=':
			kind = .Gt_Eq
			n_consume += 1
		case '>':
			if peek_char(t, 2) == '=' {
				kind = .Shr_Eq
				n_consume += 2
			} else {
				kind = .Shr
				n_consume += 1
			}
		}
	case '"':
		kind = .String
		ch := advance_char(t) // consume starting `"`,  
		n_consume = 0 // manage advancing ourselves 
		for t.offset < len(t.src) {
			if ch == '\\' && peek_char(t) == '"' {
				advance_char(t) // consume \ 
			}
			if ch == '"' {
				advance_char(t)
				break
			}
			ch = advance_char(t)
		}
	case '\'':
		kind = .Rune
		advance_char(t) // consume starting `'`,  
		n_consume = 0 // manage advancing ourselves 
		if t.ch == '\\' {
			switch peek_char(t) {
			case:
				advance_char(t)
				advance_char(t)
			case 'u', 'U', 'x':
				panic("boom")
			}
		} else {
			advance_char(t)
		}
		assert(t.ch == '\'', "expected a ' to close")
		advance_char(t)
	case:
		medium_parse = false
	}
	if medium_parse {
		for _ in 0 ..< n_consume {advance_char(t)}
		lit = t.src[start:t.offset]
		return Token{pos = pos, kind = kind, text = lit}
	}
	////// 
	if is_digit(t.ch) {
		return scan_number(t)
	} else if is_alpha(t.ch) {
		for is_alpha(t.ch) || is_digit(t.ch) {advance_char(t)}
		lit = t.src[start:t.offset]
		if lit in KEYWORDS {
			kind = KEYWORDS[lit]
		} else {
			kind = .Ident
		}
		return Token{kind = kind, text = lit, pos = pos}
	} else {
		fmt.panicf("Failed to parse <%v>", rune(t.ch))
	}
	fmt.panicf("Failed to parse <%v>", rune(t.ch))
}
scan_number :: proc(t: ^Tokenizer) -> Token {
	// do not support nutty hex-floats: double foo() { return 0x1.570a3d70a3d71p-1; } 
	start := t.offset
	pos := get_pos(t)
	is_float := false
	if t.ch == '-' {advance_char(t)}
	// integer-part: 
	for is_digit(t.ch) {advance_char(t)}
	if t.ch == '.' {
		is_float = true
		advance_char(t)
		for is_digit(t.ch) {advance_char(t)}
	}
	if t.ch == 'e' || t.ch == 'E' {
		is_float = true
		advance_char(t)
		if t.ch == '+' || t.ch == '-' {
			advance_char(t)
		}
		assert(is_digit(t.ch), "Scientific Notation requires at least one digit past the `E`")
		for is_digit(t.ch) {advance_char(t)}
	}
	if (!is_float) {
		// Integer literal suffixes 
		for t.ch == 'u' || t.ch == 'U' || t.ch == 'l' || t.ch == 'L' {
			advance_char(t)
		}
	} else {
		// Float literal suffixes 
		if (t.ch == 'f' || t.ch == 'F' || t.ch == 'l' || t.ch == 'L') {
			advance_char(t)
		}
	}
	kind: Token_Kind = is_float ? .Float : .Integer
	return Token{kind = kind, text = t.src[start:t.offset], pos = pos}
}
get_pos :: proc(t: ^Tokenizer) -> Pos {
	// file   = t.path, 
	return Pos{offset = t.offset, line = t.line_count}
}
advance_char :: proc(t: ^Tokenizer) -> u8 {
	if t.ch == '\n' {
		t.line_count += 1
	}
	if t.offset >= len(t.src) - 1 {
		t.offset += 1
		t.ch = 0
	} else {
		t.offset += 1
		t.ch = t.src[t.offset]
	}
	return t.ch
}
peek_char :: proc(t: ^Tokenizer, lookahead := 1) -> u8 {
	if t.offset + lookahead >= len(t.src) {return 0}
	return t.src[t.offset + lookahead]
}
skip_whitespace :: proc(t: ^Tokenizer) {
	for {
		switch t.ch {
		case ' ', '\t', '\r', '\n':
			advance_char(t)
		case:
			return
		}
	}
}
is_digit :: proc(ch: u8) -> bool {
	return '0' <= ch && ch <= '9'
}
is_alpha :: proc(ch: u8) -> bool {
	is_lower := 'a' <= ch && ch <= 'z'
	is_upper := 'A' <= ch && ch <= 'Z'
	return is_lower || is_upper || '_' == ch
}
