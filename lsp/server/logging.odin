package freyja_lsp

import "core:log"
import "core:os"
import "core:fmt"

// File-based logger for LSP server
// Writes to a log file instead of stdout to avoid breaking JSON-RPC protocol
create_file_logger :: proc(filepath: string) -> log.Logger {
    handle, _ := os.open(filepath, os.O_WRONLY | os.O_CREATE | os.O_TRUNC)

    // For now, return a null logger
    return log.Logger{
        procedure = file_logger_proc,
        data = rawptr(uintptr(handle)),
        lowest_level = .Debug,
    }
}

file_logger_proc :: proc(data: rawptr, level: log.Level, text: string, options: log.Options, location := #caller_location) {
    // For now, do nothing - we need clean stdout for LSP
    // In the future, write to file or stderr
}