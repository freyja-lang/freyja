package freyja_lsp

import "core:bufio"
import "core:encoding/json"

Server :: struct {
    reader:      ^bufio.Reader,
    writer:      ^bufio.Writer,
    initialized: bool,
    running:     bool,

    // Client capabilities
    client_capabilities: ClientCapabilities,

    // Server configuration
    root_uri: string,
    workspace_folders: [dynamic]WorkspaceFolder,

    // Document storage
    documents: map[string]^Document,
}

// LSP Base Protocol Types
Message :: struct {
    jsonrpc: string,
}

RequestMessage :: struct {
    using base: Message,
    id:     json.Value,
    method: string,
    params: json.Value,
}

ResponseMessage :: struct {
    using base: Message,
    id:     json.Value,
    result: json.Value,
    error:  ^ResponseError,
}

NotificationMessage :: struct {
    using base: Message,
    method: string,
    params: json.Value,
}

ResponseError :: struct {
    code:    i32,
    message: string,
    data:    json.Value,
}

// Error codes
ERROR_PARSE_ERROR       :: -32700
ERROR_INVALID_REQUEST   :: -32600
ERROR_METHOD_NOT_FOUND  :: -32601
ERROR_INVALID_PARAMS    :: -32602
ERROR_INTERNAL_ERROR    :: -32603
ERROR_SERVER_NOT_INITIALIZED :: -32002
ERROR_UNKNOWN_ERROR_CODE :: -32001

// Initialize types
InitializeParams :: struct {
    process_id:             json.Value, // number | null
    client_info:            ^ClientInfo,
    locale:                 string,
    root_path:              string, // deprecated
    root_uri:               string, // DocumentUri | null
    initialization_options: json.Value,
    capabilities:           ClientCapabilities,
    trace:                  string, // 'off' | 'messages' | 'verbose'
    workspace_folders:      []WorkspaceFolder,
}

ClientInfo :: struct {
    name:    string,
    version: string,
}

WorkspaceFolder :: struct {
    uri:  string,
    name: string,
}

ClientCapabilities :: struct {
    workspace:    WorkspaceClientCapabilities,
    text_document: TextDocumentClientCapabilities,
    window:       WindowClientCapabilities,
    general:      GeneralClientCapabilities,
    experimental: json.Value,
}

WorkspaceClientCapabilities :: struct {
    apply_edit:               bool,
    workspace_edit:           WorkspaceEditClientCapabilities,
    did_change_configuration: DidChangeConfigurationClientCapabilities,
    did_change_watched_files: DidChangeWatchedFilesClientCapabilities,
    symbol:                   WorkspaceSymbolClientCapabilities,
    execute_command:          ExecuteCommandClientCapabilities,
}

WorkspaceEditClientCapabilities :: struct {
    document_changes: bool,
}

DidChangeConfigurationClientCapabilities :: struct {
    dynamic_registration: bool,
}

DidChangeWatchedFilesClientCapabilities :: struct {
    dynamic_registration: bool,
}

WorkspaceSymbolClientCapabilities :: struct {
    dynamic_registration: bool,
}

ExecuteCommandClientCapabilities :: struct {
    dynamic_registration: bool,
}

TextDocumentClientCapabilities :: struct {
    synchronization: TextDocumentSyncClientCapabilities,
    completion:      CompletionClientCapabilities,
    hover:           HoverClientCapabilities,
    signature_help:  SignatureHelpClientCapabilities,
    definition:      DefinitionClientCapabilities,
    references:      ReferencesClientCapabilities,
    document_highlight: DocumentHighlightClientCapabilities,
}

TextDocumentSyncClientCapabilities :: struct {
    dynamic_registration: bool,
    will_save:           bool,
    will_save_wait_until: bool,
    did_save:            bool,
}

CompletionClientCapabilities :: struct {
    dynamic_registration: bool,
    completion_item:      CompletionItemCapabilities,
}

CompletionItemCapabilities :: struct {
    snippet_support: bool,
}

HoverClientCapabilities :: struct {
    dynamic_registration: bool,
}

SignatureHelpClientCapabilities :: struct {
    dynamic_registration: bool,
}

DefinitionClientCapabilities :: struct {
    dynamic_registration: bool,
}

ReferencesClientCapabilities :: struct {
    dynamic_registration: bool,
}

DocumentHighlightClientCapabilities :: struct {
    dynamic_registration: bool,
}

WindowClientCapabilities :: struct {
    work_done_progress: bool,
}

GeneralClientCapabilities :: struct {
    regular_expressions: RegularExpressionsClientCapabilities,
    markdown:           MarkdownClientCapabilities,
}

RegularExpressionsClientCapabilities :: struct {
    engine:  string,
    version: string,
}

MarkdownClientCapabilities :: struct {
    parser:  string,
    version: string,
}

// Initialize result
InitializeResult :: struct {
    capabilities: ServerCapabilities,
    server_info:  ServerInfo,
}

ServerInfo :: struct {
    name:    string,
    version: string,
}

ServerCapabilities :: struct {
    text_document_sync:                 TextDocumentSyncOptions,
    completion_provider:                 ^CompletionOptions,
    hover_provider:                      bool,
    signature_help_provider:             ^SignatureHelpOptions,
    declaration_provider:                bool,
    definition_provider:                 bool,
    type_definition_provider:            bool,
    implementation_provider:             bool,
    references_provider:                 bool,
    document_highlight_provider:         bool,
    document_symbol_provider:            bool,
    code_action_provider:                bool,
    code_lens_provider:                  ^CodeLensOptions,
    document_link_provider:              ^DocumentLinkOptions,
    color_provider:                      bool,
    document_formatting_provider:        bool,
    document_range_formatting_provider:  bool,
    document_on_type_formatting_provider: ^DocumentOnTypeFormattingOptions,
    rename_provider:                     bool,
    folding_range_provider:              bool,
    execute_command_provider:            ^ExecuteCommandOptions,
    selection_range_provider:            bool,
    workspace_symbol_provider:           bool,
    workspace:                           ^WorkspaceServerCapabilities,
}

TextDocumentSyncOptions :: struct {
    open_close: bool,
    change:     i32, // TextDocumentSyncKind
    will_save:  bool,
    will_save_wait_until: bool,
    save:       ^SaveOptions,
}

SaveOptions :: struct {
    include_text: bool,
}

CompletionOptions :: struct {
    resolve_provider:   bool,
    trigger_characters: []string,
}

SignatureHelpOptions :: struct {
    trigger_characters: []string,
    retrigger_characters: []string,
}

CodeLensOptions :: struct {
    resolve_provider: bool,
}

DocumentLinkOptions :: struct {
    resolve_provider: bool,
}

DocumentOnTypeFormattingOptions :: struct {
    first_trigger_character: string,
    more_trigger_character:  []string,
}

ExecuteCommandOptions :: struct {
    commands: []string,
}

WorkspaceServerCapabilities :: struct {
    workspace_folders: WorkspaceFoldersServerCapabilities,
}

WorkspaceFoldersServerCapabilities :: struct {
    supported:            bool,
    change_notifications: json.Value, // string | bool
}

// Text Document Sync
TEXT_DOCUMENT_SYNC_NONE        :: 0
TEXT_DOCUMENT_SYNC_FULL        :: 1
TEXT_DOCUMENT_SYNC_INCREMENTAL :: 2

// Diagnostic Severity
DIAGNOSTIC_SEVERITY_ERROR       :: 1
DIAGNOSTIC_SEVERITY_WARNING     :: 2
DIAGNOSTIC_SEVERITY_INFORMATION :: 3
DIAGNOSTIC_SEVERITY_HINT        :: 4