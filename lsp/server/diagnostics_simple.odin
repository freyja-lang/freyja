package freyja_lsp

import "core:encoding/json"
import "core:strings"

// Simplified diagnostics for testing
check_document_simple :: proc(server: ^Server, doc: ^Document) {
	log_debug("Simple diagnostics check starting for:", doc.uri)
	diagnostics := make([dynamic]json.Value)
	defer delete(diagnostics)

	// Just check for a simple pattern as a test
	if strings.contains(doc.content, "ERROR") {
		diagnostic := create_diagnostic(0, 0, 0, 5,
			"Found ERROR marker",
			DIAGNOSTIC_SEVERITY_ERROR)
		append(&diagnostics, diagnostic)
	}

	log_debug("Sending", len(diagnostics), "simple diagnostics")
	send_diagnostics(server, doc.uri, diagnostics[:])
	log_debug("Simple diagnostics complete")
}