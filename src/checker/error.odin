package checker

import "core:fmt"
import "../tokenizer"

// Error reporting system (modeled after Odin's error.cpp)

ErrorKind :: enum {
	ERROR,
	WARNING,
	SYNTAX_ERROR,
}

// Single error/warning with source location and message
ErrorValue :: struct {
	kind:    ErrorKind,
	pos:     tokenizer.Pos,
	end_pos: tokenizer.Pos,
	message: string,
}

// Global error collector (like Odin's ErrorCollector)
ErrorCollector :: struct {
	errors:        [dynamic]ErrorValue,
	error_count:   int,
	warning_count: int,
	max_errors:    int, // Stop compilation after this many errors
}

// Global error collector instance
global_error_collector: ErrorCollector

// Initialize the error collector
init_error_collector :: proc() {
	global_error_collector.errors = make([dynamic]ErrorValue)
	global_error_collector.error_count = 0
	global_error_collector.warning_count = 0
	global_error_collector.max_errors = 100 // Stop after 100 errors
}

// Add an error to the collector
add_error :: proc(kind: ErrorKind, pos: tokenizer.Pos, message: string) {
	error_value := ErrorValue {
		kind    = kind,
		pos     = pos,
		message = message,
	}

	append(&global_error_collector.errors, error_value)

	switch kind {
	case .ERROR, .SYNTAX_ERROR:
		global_error_collector.error_count += 1
	case .WARNING:
		global_error_collector.warning_count += 1
	}
}

// Main error reporting functions (like Odin's error.cpp)
error :: proc {
	error_with_pos,
	error_with_token,
}

error_with_pos :: proc(pos: tokenizer.Pos, fmt_str: string, args: ..any) {
	message := fmt.aprintf(fmt_str, ..args)
	add_error(.ERROR, pos, message)
}

error_with_token :: proc(token: tokenizer.Token, fmt_str: string, args: ..any) {
	message := fmt.aprintf(fmt_str, ..args)
	add_error(.ERROR, token.pos, message)
}

// Warning reporting
warning :: proc {
	warning_with_pos,
	warning_with_token,
}

warning_with_pos :: proc(pos: tokenizer.Pos, fmt_str: string, args: ..any) {
	message := fmt.aprintf(fmt_str, ..args)
	add_error(.WARNING, pos, message)
}

warning_with_token :: proc(token: tokenizer.Token, fmt_str: string, args: ..any) {
	message := fmt.aprintf(fmt_str, ..args)
	add_error(.WARNING, token.pos, message)
}

// Syntax error reporting
syntax_error :: proc {
	syntax_error_with_pos,
	syntax_error_with_token,
}

syntax_error_with_pos :: proc(pos: tokenizer.Pos, fmt_str: string, args: ..any) {
	message := fmt.aprintf(fmt_str, ..args)
	add_error(.SYNTAX_ERROR, pos, message)
}

syntax_error_with_token :: proc(token: tokenizer.Token, fmt_str: string, args: ..any) {
	message := fmt.aprintf(fmt_str, ..args)
	add_error(.SYNTAX_ERROR, token.pos, message)
}

// Check if we have any errors
any_errors :: proc() -> bool {
	return global_error_collector.error_count > 0
}

// Check if we have any warnings
any_warnings :: proc() -> bool {
	return global_error_collector.warning_count > 0
}

// Check if we should stop compilation due to too many errors
too_many_errors :: proc() -> bool {
	return global_error_collector.error_count >= global_error_collector.max_errors
}

// Format a position for display (similar to token_pos_to_string)
format_pos :: proc(pos: tokenizer.Pos) -> string {
	if len(pos.file) == 0 {
		return fmt.aprintf("(unknown):%d:%d", pos.line, pos.column)
	}
	return fmt.aprintf("%s:%d:%d", pos.file, pos.line, pos.column)
}

// Print all collected errors (like Odin's print_all_errors)
print_all_errors :: proc() {
	if len(global_error_collector.errors) == 0 {
		return
	}

	fmt.printf("\n=== COMPILATION ERRORS ===\n")

	for error_value in global_error_collector.errors {
		pos_str := format_pos(error_value.pos)

		switch error_value.kind {
		case .ERROR:
			fmt.printf("Error: %s: %s\n", pos_str, error_value.message)
		case .WARNING:
			fmt.printf("Warning: %s: %s\n", pos_str, error_value.message)
		case .SYNTAX_ERROR:
			fmt.printf("Syntax Error: %s: %s\n", pos_str, error_value.message)
		}
	}

	fmt.printf(
		"\n%d error(s), %d warning(s)\n",
		global_error_collector.error_count,
		global_error_collector.warning_count,
	)
}

// Clear all errors (for testing)
clear_errors :: proc() {
	clear(&global_error_collector.errors)
	global_error_collector.error_count = 0
	global_error_collector.warning_count = 0
}
