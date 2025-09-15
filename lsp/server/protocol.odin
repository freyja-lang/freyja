package freyja_lsp

import "core:bufio"
import "core:bytes"
import "core:encoding/json"
import "core:fmt"
import "core:io"
import "core:log"
import "core:strconv"
import "core:strings"

read_message :: proc(server: ^Server) -> (message: json.Value, ok: bool) {
	log_debug("read_message: starting")
	// Read headers
	headers := make(map[string]string)
	defer delete(headers)

	for {
		line_bytes, err := bufio.reader_read_slice(server.reader, '\n')
		if err != nil {
			log_error("Failed to read header line:", err)
			return {}, false
		}

		line := string(line_bytes)
		line = strings.trim_right(line, "\r\n")

		if len(line) == 0 {
			// Empty line signals end of headers
			break
		}

		parts := strings.split(line, ":")
		if len(parts) != 2 {
			log_error("Invalid header:", line)
			return {}, false
		}

		key := strings.trim_space(parts[0])
		value := strings.trim_space(parts[1])
		headers[key] = value
	}

	// Get content length
	content_length_str, has_length := headers["Content-Length"]
	if !has_length {
		log_error("Missing Content-Length header")
		return {}, false
	}
	log_debug("Content-Length:", content_length_str)

	content_length, parse_ok := strconv.parse_int(content_length_str)
	if !parse_ok {
		log_error("Invalid Content-Length:", content_length_str)
		return {}, false
	}

	// Read content
	content := make([]byte, content_length)
	defer delete(content)

	// Read in a loop until we get all content
	total_read := 0
	for total_read < int(content_length) {
		n, err := bufio.reader_read(server.reader, content[total_read:])
		if err != nil {
			log_error("Failed to read message content. Read", total_read, "expected", content_length, "error:", err)
			return {}, false
		}
		total_read += n
	}

	if total_read != int(content_length) {
		log_error("Content length mismatch. Read", total_read, "expected", content_length)
		return {}, false
	}
	log_debug("Read content:", string(content[:min(100, len(content))]), "...")

	// Parse JSON
	json_value, json_err := json.parse(content)
	if json_err != .None {
		log_error("Failed to parse JSON:", json_err)
		return {}, false
	}

	log_debug("Successfully parsed message")
	return json_value, true
}

write_message :: proc(server: ^Server, message: json.Value) {
	log_debug("write_message: starting")
	// Marshal to JSON
	json_bytes, err := json.marshal(message)
	if err != nil {
		log_error("Failed to marshal JSON:", err)
		return
	}
	defer delete(json_bytes)
	log_debug("Sending:", string(json_bytes[:min(200, len(json_bytes))]), "...")

	// Write headers
	header := fmt.tprintf("Content-Length: %d\r\n\r\n", len(json_bytes))
	bufio.writer_write_string(server.writer, header)

	// Write content
	bufio.writer_write(server.writer, json_bytes)
	bufio.writer_flush(server.writer)
	log_debug("Message sent")
}

send_response :: proc(server: ^Server, id: json.Value, result: json.Value) {
	log_debug("Sending response for id:", id)
	response := json.Object{}
	defer delete(response)

	response["jsonrpc"] = "2.0"

	// Convert float IDs to integers for proper JSON formatting
	if id_float, ok := id.(json.Float); ok {
		response["id"] = json.Integer(id_float)
	} else {
		response["id"] = id
	}

	response["result"] = result

	write_message(server, response)
}

send_error :: proc(server: ^Server, id: json.Value, code: i32, message: string) {
	log_error("Sending error response:", message, "code:", code)
	error_obj := json.Object{}
	defer delete(error_obj)

	error_obj["code"] = i64(code)
	error_obj["message"] = message

	response := json.Object{}
	defer delete(response)

	response["jsonrpc"] = "2.0"

	// Convert float IDs to integers for proper JSON formatting
	if id_float, ok := id.(json.Float); ok {
		response["id"] = json.Integer(id_float)
	} else {
		response["id"] = id
	}

	response["error"] = error_obj

	write_message(server, response)
}

send_notification :: proc(server: ^Server, method: string, params: json.Value) {
	log_debug("Sending notification:", method)
	notification := json.Object{}
	defer delete(notification)

	notification["jsonrpc"] = "2.0"
	notification["method"] = method
	notification["params"] = params

	write_message(server, notification)
}
