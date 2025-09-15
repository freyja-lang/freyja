package freyja_lsp

import "core:encoding/json"
import "core:strings"
import "core:path/filepath"
import "core:os"
import "core:fmt"
import "../../src/parser"
import "../../src/checker"

Document :: struct {
	uri:     string,
	content: string,
	version: i64,
}

// Use the real Freyja compiler's parser and checker
check_document :: proc(server: ^Server, doc: ^Document) {
	diagnostics := make([dynamic]json.Value)
	defer delete(diagnostics)

	// Convert URI to file path for the parser
	log_debug("Converting URI to path:", doc.uri)
	file_path := uri_to_path(doc.uri)
	log_debug("Converted to path:", file_path)
	if file_path == "" {
		log_error("Invalid URI:", doc.uri)
		return
	}

	// Write content to a temporary file for parsing
	// (The parser expects a file path, not content directly)
	temp_path := fmt.tprintf("%s.tmp", file_path)
	log_debug("Writing temp file:", temp_path)
	write_ok := os.write_entire_file(temp_path, transmute([]byte)doc.content)
	if !write_ok {
		log_error("Failed to write temp file:", temp_path)
		diagnostic := create_diagnostic(0, 0, 0, 1,
			"Internal error: Failed to create temporary file",
			DIAGNOSTIC_SEVERITY_ERROR)
		append(&diagnostics, diagnostic)
		send_diagnostics(server, doc.uri, diagnostics[:])
		return
	}
	defer os.remove(temp_path)

	// Step 1: Parse the file using Freyja's parser
	log_debug("Parsing file with Freyja parser")
	parse_result := parser.parse_freyja_file(temp_path)
	log_debug("Parse result success:", parse_result.success)
	if !parse_result.success {
		// Parse failed - report generic error for now
		log_debug("Parse failed, sending error diagnostic")
		diagnostic := create_diagnostic(0, 0, 0, 1,
			"Parse error: Failed to parse file",
			DIAGNOSTIC_SEVERITY_ERROR)
		append(&diagnostics, diagnostic)
		send_diagnostics(server, doc.uri, diagnostics[:])
		return
	}

	// Step 2: Type check using Freyja's checker
	log_debug("Running type checker")
	checker.init_error_collector()
	check_result := checker.check(parse_result)
	log_debug("Check result - errors:", check_result.error_count, "warnings:", check_result.warning_count)

	// Convert checker errors to LSP diagnostics
	log_debug("Converting", len(checker.global_error_collector.errors), "errors to diagnostics")
	for error in checker.global_error_collector.errors {
		severity := DIAGNOSTIC_SEVERITY_ERROR
		if error.kind == .WARNING {
			severity = DIAGNOSTIC_SEVERITY_WARNING
		}

		// Adjust line numbers (checker uses 1-based, LSP uses 0-based)
		start_line := max(0, error.pos.line - 1)
		start_char := max(0, error.pos.column - 1)
		end_line := start_line
		end_char := start_char + 1

		if error.end_pos.line > 0 {
			end_line = error.end_pos.line - 1
			end_char = error.end_pos.column - 1
		}

		diagnostic := create_diagnostic(
			start_line, start_char, end_line, end_char,
			error.message,
			i64(severity))
		append(&diagnostics, diagnostic)
	}

	// Send diagnostics to client
	log_debug("Sending", len(diagnostics), "diagnostics to client")
	send_diagnostics(server, doc.uri, diagnostics[:])
	log_debug("Diagnostics check complete")
}

create_diagnostic :: proc(start_line, start_char, end_line, end_char: int, message: string, severity: i64) -> json.Value {
	diagnostic := json.Object{}
	range_obj := json.Object{}
	start_pos := json.Object{}
	end_pos := json.Object{}

	start_pos["line"] = i64(start_line)
	start_pos["character"] = i64(start_char)
	end_pos["line"] = i64(end_line)
	end_pos["character"] = i64(end_char)

	range_obj["start"] = start_pos
	range_obj["end"] = end_pos

	diagnostic["range"] = range_obj
	diagnostic["severity"] = severity
	diagnostic["source"] = "freyja"
	diagnostic["message"] = message

	return diagnostic
}


send_diagnostics :: proc(server: ^Server, uri: string, diagnostics: []json.Value) {
	params := json.Object{}
	defer delete(params)

	// Convert slice to json.Array
	diag_array := make(json.Array, len(diagnostics))
	for i := 0; i < len(diagnostics); i += 1 {
		diag_array[i] = diagnostics[i]
	}

	params["uri"] = uri
	params["diagnostics"] = diag_array

	send_notification(server, "textDocument/publishDiagnostics", params)
}

uri_to_path :: proc(uri: string) -> string {
	// Simple conversion - just strip file:/// prefix
	if strings.has_prefix(uri, "file:///") {
		path := uri[8:]

		// Decode URL encoding
		path, _ = strings.replace_all(path, "%3A", ":")
		path, _ = strings.replace_all(path, "%2F", "/")
		path, _ = strings.replace_all(path, "%20", " ")

		// On Windows, convert forward slashes to backslashes
		when ODIN_OS == .Windows {
			path, _ = strings.replace_all(path, "/", "\\")
			// Handle drive letters (e.g., "d:" becomes "D:\")
			if len(path) >= 2 && path[1] == ':' {
				return path
			}
		}
		return path
	}
	return ""
}

path_to_uri :: proc(path: string) -> string {
	// Convert path to file URI
	clean_path := filepath.clean(path)
	when ODIN_OS == .Windows {
		// Convert backslashes to forward slashes for URI
		clean_path, _ = strings.replace_all(clean_path, "\\", "/")
	}
	return strings.concatenate({"file:///", clean_path})
}