package freyja_lsp

import "core:bufio"
import "core:encoding/json"
import "core:fmt"
import "core:io"
import "core:log"
import "core:os"
import "core:strings"

VERSION :: "0.0.1"

main :: proc() {
	// Initialize file logger
	if !init_logger() {
		os.exit(1)
	}
	defer close_logger()

	log_info("Freyja LSP Server starting, version:", VERSION)

	log_debug("Initializing reader and writer")
	reader := bufio.Reader{}
	bufio.reader_init(&reader, os.stream_from_handle(os.stdin))
	defer bufio.reader_destroy(&reader)

	writer := bufio.Writer{}
	bufio.writer_init(&writer, os.stream_from_handle(os.stdout))
	defer bufio.writer_destroy(&writer)

	log_debug("Creating server instance")
	server := Server {
		reader      = &reader,
		writer      = &writer,
		initialized = false,
		running     = true,
		documents   = make(map[string]^Document),
	}
	defer delete(server.documents)

	log_info("Server initialized, starting main loop")
	run(&server)
	log_info("Server shutting down")
}

run :: proc(server: ^Server) {
	for server.running {
		log_debug("Waiting for message")
		message, ok := read_message(server)
		if !ok {
			log_error("Failed to read message, exiting loop")
			break
		}

		log_debug("Message received, handling")
		handle_message(server, message)

		// Clean up the message after handling
		if msg_obj, is_obj := message.(json.Object); is_obj {
			// Object cleanup is handled by destroy_value
		}
		json.destroy_value(message)
	}
	log_info("Main loop exited")
}
