package parser

import "core:fmt"
import ast "../ast"
import tokenizer "../tokenizer"
import "core:os"
import "core:strings"

ParseResult :: struct {
	file:    ^ast.File, // Pointer to AST file
	success: bool,
}

// Step 1: Parse the source file using Odin's parser
parse_freyja_file :: proc(filepath: string) -> ParseResult {
	fmt.printf("=== PARSE ===\n")
	fmt.printf("Parsing %s...\n", filepath)

	// Read the source file
	source_data, read_ok := os.read_entire_file(filepath)
	if !read_ok {
		fmt.eprintln("Failed to read file:", filepath)
		return ParseResult{success = false}
	}
	defer delete(source_data)

	source := string(source_data)

	fmt.printf("Successfully read %d bytes from %s\n", len(source), filepath)

	// Create AST file structure
	file := new(ast.File)
	file.fullpath = strings.clone(filepath)
	file.src = strings.clone(source)

	// Create parser
	p := default_parser()

	// Set custom error handler
	p.err = proc(pos: tokenizer.Pos, format: string, args: ..any) {
		message := fmt.tprintf(format, ..args)
		fmt.eprintf("%s(%d:%d): ERROR: %s\n", pos.file, pos.line, pos.column, message)
	}

	// Initialize tokenizer
	tokenizer.init(&p.tok, file.src, file.fullpath, p.err)

	// Parse the file
	parse_ok := parse_file(&p, file)
	if !parse_ok {
		fmt.eprintln("Parse failed")
		return ParseResult{success = false}
	}

	fmt.printf("Parse complete! Found %d declarations\n", len(file.decls))

	// Print some info about what we parsed
	for decl, i in file.decls {
		#partial switch stmt in decl.derived {
		case ^ast.Value_Decl:
			if len(stmt.names) > 0 {
				#partial switch name in stmt.names[0].derived {
				case ^ast.Ident:
					fmt.printf("  [%d] Value declaration: %s\n", i, name.name)
				}
			}
		case ^ast.Package_Decl:
			fmt.printf("  [%d] Package declaration: %s\n", i, stmt.name)
		case:
			fmt.printf("  [%d] Other declaration: %T\n", i, stmt)
		}
	}

	return ParseResult{file = file, success = true}
}
