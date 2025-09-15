package freyja_lsp

import "core:fmt"
import "core:os"
import "core:time"
import "core:sync"

Logger :: struct {
	file: os.Handle,
	mutex: sync.Mutex,
}

global_logger: ^Logger

init_logger :: proc() -> bool {
	global_logger = new(Logger)

	// Open log file in append mode
	file, err := os.open("freyja-lsp.log", os.O_CREATE | os.O_WRONLY | os.O_APPEND)
	if err != 0 {
		return false
	}

	global_logger.file = file

	// Write startup message
	log_info("Freyja LSP Server starting...")
	log_info("Version:", VERSION)

	return true
}

close_logger :: proc() {
	if global_logger != nil {
		log_info("Freyja LSP Server shutting down")
		os.close(global_logger.file)
		free(global_logger)
		global_logger = nil
	}
}

log_write :: proc(level: string, args: ..any) {
	if global_logger == nil {
		return
	}

	sync.mutex_lock(&global_logger.mutex)
	defer sync.mutex_unlock(&global_logger.mutex)

	// Get timestamp
	now := time.now()
	year, month, day := time.date(now)
	hour, min, sec := time.clock(now)

	// Write log entry with formatted timestamp
	fmt.fprintf(global_logger.file, "[%04d-%02d-%02d %02d:%02d:%02d] %s: ",
		year, int(month), day, hour, min, sec, level)
	fmt.fprintln(global_logger.file, ..args)

	// Flush immediately for debugging
	os.flush(global_logger.file)
}

log_info :: proc(args: ..any) {
	log_write("INFO", ..args)
}

log_error :: proc(args: ..any) {
	log_write("ERROR", ..args)
}

log_debug :: proc(args: ..any) {
	log_write("DEBUG", ..args)
}