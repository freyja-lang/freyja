package freyja_lsp

import "core:fmt"
import "core:os"

DEBUG :: #config(DEBUG, false)

// Debug logging to stderr (safe for LSP)
debug_log :: proc(args: ..any) {
	when DEBUG {
		fmt.fprintln(os.stderr, ..args)
	}
}

debug_logf :: proc(format: string, args: ..any) {
	when DEBUG {
		fmt.fprintf(os.stderr, format, ..args)
		fmt.fprintln(os.stderr)
	}
}