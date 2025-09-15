#+feature dynamic-literals
package freyja_lsp

import "core:encoding/json"
import "core:fmt"
import "core:log"

handle_message :: proc(server: ^Server, message: json.Value) {
	log_debug("handle_message: starting")
	msg_obj, is_object := message.(json.Object)
	if !is_object {
		log_error("Message is not a JSON object")
		return
	}

	// Check if it's a request or notification by presence of id
	id, has_id := msg_obj["id"]
	method_val, has_method := msg_obj["method"]

	if !has_method {
		log_error("Message missing method field")
		return
	}

	method, is_string := method_val.(json.String)
	if !is_string {
		log_error("Method is not a string")
		return
	}

	params := msg_obj["params"]

	if has_id {
		// It's a request
		log_debug("Received request:", string(method), "id:", id)
		handle_request(server, id, string(method), params)
	} else {
		// It's a notification
		log_debug("Received notification:", string(method))
		handle_notification(server, string(method), params)
	}
}

handle_request :: proc(server: ^Server, id: json.Value, method: string, params: json.Value) {
	log_info("Handling request:", method)

	switch method {
	case "initialize":
		handle_initialize(server, id, params)
	case "shutdown":
		handle_shutdown(server, id)
	case "textDocument/definition":
		if !server.initialized {
			send_error(server, id, ERROR_SERVER_NOT_INITIALIZED, "Server not initialized")
			return
		}
		handle_definition(server, id, params)
	case "textDocument/hover":
		if !server.initialized {
			send_error(server, id, ERROR_SERVER_NOT_INITIALIZED, "Server not initialized")
			return
		}
		handle_hover(server, id, params)
	case "textDocument/completion":
		if !server.initialized {
			send_error(server, id, ERROR_SERVER_NOT_INITIALIZED, "Server not initialized")
			return
		}
		handle_completion(server, id, params)
	case:
		send_error(server, id, ERROR_METHOD_NOT_FOUND, fmt.tprintf("Method not found: %s", method))
	}
}

handle_notification :: proc(server: ^Server, method: string, params: json.Value) {
	// log.info("Handling notification:", method)

	switch method {
	case "initialized":
		handle_initialized(server, params)
	case "exit":
		handle_exit(server)
	case "textDocument/didOpen":
		if !server.initialized {
			return
		}
		handle_did_open(server, params)
	case "textDocument/didChange":
		if !server.initialized {
			return
		}
		handle_did_change(server, params)
	case "textDocument/didClose":
		if !server.initialized {
			return
		}
		handle_did_close(server, params)
	case "$/cancelRequest":
		// Ignore cancel requests for now
		log_debug("Ignoring $/cancelRequest")
	case:
		log_debug("Unknown notification:", method)
	}
}

handle_initialize :: proc(server: ^Server, id: json.Value, params: json.Value) {
	log_info("Initializing server")

	// Parse initialization params
	params_obj, is_object := params.(json.Object)
	if is_object {
		if root_uri, ok := params_obj["rootUri"].(json.String); ok {
			server.root_uri = string(root_uri)
			log_info("Root URI:", server.root_uri)
		}

		// TODO: Parse client capabilities
	}

	// Build server capabilities
	capabilities := json.Object{}

	// Text document sync
	text_sync := json.Object{}
	text_sync["openClose"] = true
	text_sync["change"] = i64(TEXT_DOCUMENT_SYNC_FULL)
	capabilities["textDocumentSync"] = text_sync

	// Basic capabilities
	capabilities["hoverProvider"] = true
	capabilities["definitionProvider"] = true

	trigger_chars := make(json.Array, 1)
	trigger_chars[0] = json.String(".")

	completion_provider := json.Object{}
	completion_provider["resolveProvider"] = false
	completion_provider["triggerCharacters"] = trigger_chars
	capabilities["completionProvider"] = completion_provider

	// Server info
	server_info := json.Object{}
	server_info["name"] = "freyja-lsp"
	server_info["version"] = VERSION

	// Build result
	result := json.Object{}
	result["capabilities"] = capabilities
	result["serverInfo"] = server_info

	server.initialized = true
	log_info("Sending initialize response")
	send_response(server, id, result)
	log_info("Initialize response sent")
}

handle_initialized :: proc(server: ^Server, params: json.Value) {
	log_info("Server initialized notification received")
}

handle_shutdown :: proc(server: ^Server, id: json.Value) {
	log_info("Shutting down server")
	server.initialized = false
	send_response(server, id, nil)
}

handle_exit :: proc(server: ^Server) {
	log_info("Exit notification received, stopping server")
	server.running = false
}

// Document synchronization handlers
handle_did_open :: proc(server: ^Server, params: json.Value) {
	params_obj, is_object := params.(json.Object)
	if !is_object {
		return
	}

	text_doc_val, has_doc := params_obj["textDocument"]
	if !has_doc {
		return
	}

	text_doc, is_doc_obj := text_doc_val.(json.Object)
	if !is_doc_obj {
		return
	}

	// Extract document info
	uri_val, has_uri := text_doc["uri"]
	text_val, has_text := text_doc["text"]
	version_val, has_version := text_doc["version"]

	if !has_uri || !has_text {
		return
	}

	uri, uri_ok := uri_val.(json.String)
	text, text_ok := text_val.(json.String)

	if !uri_ok || !text_ok {
		return
	}

	version := i64(0)
	if has_version {
		if ver, ver_ok := version_val.(json.Float); ver_ok {
			version = i64(ver)
		} else if ver, ver_ok := version_val.(json.Integer); ver_ok {
			version = i64(ver)
		}
	}

	// Create new document
	doc := new(Document)
	doc.uri = string(uri)
	doc.content = string(text)
	doc.version = version

	// Store document
	server.documents[doc.uri] = doc

	// Run diagnostics
	log_debug("Running diagnostics for:", doc.uri)
	// Use real diagnostics but with parser disabled
	check_document(server, doc)
}

handle_did_change :: proc(server: ^Server, params: json.Value) {
	params_obj, is_object := params.(json.Object)
	if !is_object {
		return
	}

	text_doc_val, has_doc := params_obj["textDocument"]
	if !has_doc {
		return
	}

	text_doc, is_doc_obj := text_doc_val.(json.Object)
	if !is_doc_obj {
		return
	}

	// Get document URI and version
	uri_val, has_uri := text_doc["uri"]
	version_val, has_version := text_doc["version"]

	if !has_uri {
		return
	}

	uri, uri_ok := uri_val.(json.String)
	if !uri_ok {
		return
	}

	// Find existing document
	doc, doc_exists := server.documents[string(uri)]
	if !doc_exists {
		return
	}

	// Update version
	if has_version {
		if ver, ver_ok := version_val.(json.Float); ver_ok {
			doc.version = i64(ver)
		} else if ver, ver_ok := version_val.(json.Integer); ver_ok {
			doc.version = i64(ver)
		}
	}

	// Get content changes
	changes_val, has_changes := params_obj["contentChanges"]
	if !has_changes {
		return
	}

	changes, is_array := changes_val.(json.Array)
	if !is_array || len(changes) == 0 {
		return
	}

	// For full document sync, we get the entire new content
	change, is_obj := changes[0].(json.Object)
	if !is_obj {
		return
	}

	text_val, has_text := change["text"]
	if !has_text {
		return
	}

	text, text_ok := text_val.(json.String)
	if !text_ok {
		return
	}

	// Update document content
	doc.content = string(text)

	// Re-run diagnostics
	log_debug("Running diagnostics for changed document:", doc.uri)
	// Use real diagnostics but with parser disabled
	check_document(server, doc)
}

handle_did_close :: proc(server: ^Server, params: json.Value) {
	params_obj, is_object := params.(json.Object)
	if !is_object {
		return
	}

	text_doc_val, has_doc := params_obj["textDocument"]
	if !has_doc {
		return
	}

	text_doc, is_doc_obj := text_doc_val.(json.Object)
	if !is_doc_obj {
		return
	}

	uri_val, has_uri := text_doc["uri"]
	if !has_uri {
		return
	}

	uri, uri_ok := uri_val.(json.String)
	if !uri_ok {
		return
	}

	// Remove document from storage
	if doc, exists := server.documents[string(uri)]; exists {
		free(doc)
		delete_key(&server.documents, string(uri))
	}

	// Clear diagnostics for this document - disabled for now
	// diagnostics := make([dynamic]json.Value)
	// defer delete(diagnostics)
	// send_diagnostics(server, string(uri), diagnostics[:])
}

// Feature handlers (stubs for now)
handle_hover :: proc(server: ^Server, id: json.Value, params: json.Value) {
	// log.info("Hover request")
	// Return null for now
	send_response(server, id, nil)
}

handle_definition :: proc(server: ^Server, id: json.Value, params: json.Value) {
	// log.info("Definition request")
	// Return null for now
	send_response(server, id, nil)
}

handle_completion :: proc(server: ^Server, id: json.Value, params: json.Value) {
	// log.info("Completion request")
	// Return empty completion list
	items := make(json.Array, 0)

	result := json.Object{}
	defer delete(result)
	result["isIncomplete"] = false
	result["items"] = items
	send_response(server, id, result)
}
