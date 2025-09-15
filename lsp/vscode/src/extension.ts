import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';
import * as os from 'os';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext) {
    // Immediate feedback
    console.log('=============================================');
    console.log('FREYJA EXTENSION ACTIVATING');
    console.log('=============================================');

    // Show activation message
    vscode.window.showInformationMessage('Freyja Language extension is starting...');

    // Get the server path from configuration or look for it in common locations
    const config = vscode.workspace.getConfiguration('freyja');
    let serverPath = config.get<string>('server.path');

    if (!serverPath) {
        // Try to find the server in common locations
        const isWindows = process.platform === 'win32';
        const exeName = isWindows ? 'freyja-lsp.exe' : 'freyja-lsp';

        const possiblePaths = [
            // Absolute path to the LSP server
            path.join('D:', 'dev', 'freyja', 'lsp', 'server', exeName),
            // In the extension directory
            path.join(context.extensionPath, '..', 'server', exeName),
            // In the workspace
            path.join(vscode.workspace.rootPath || '', 'lsp', 'server', exeName),
            // Relative to extension
            path.join(context.extensionPath, '..', '..', 'lsp', 'server', exeName),
            // System PATH (just the command name)
            exeName
        ];

        for (const testPath of possiblePaths) {
            if (testPath === exeName || fs.existsSync(testPath)) {
                serverPath = testPath;
                console.log(`Found Freyja LSP server at: ${serverPath}`);
                break;
            }
        }

        if (!serverPath) {
            const msg = 'Freyja Language Server (freyja-lsp.exe) not found. Please set freyja.server.path in settings or build the LSP server.';
            console.error(msg);
            console.error('Searched paths:', possiblePaths);
            vscode.window.showErrorMessage(msg);
            return;
        }

        console.log('Using Freyja LSP server at:', serverPath);
    }

    // Server options
    const serverOptions: ServerOptions = {
        run: {
            command: serverPath,
            transport: TransportKind.stdio
        },
        debug: {
            command: serverPath,
            transport: TransportKind.stdio,
            options: {
                env: { ...process.env, FREYJA_LSP_DEBUG: '1' }
            }
        }
    };

    // Client options
    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'freyja' },
            { scheme: 'untitled', language: 'freyja' }
        ],
        synchronize: {
            // Automatically synchronize workspace folders
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.{freyja,fj}')
        },
        outputChannel: vscode.window.createOutputChannel('Freyja Language Server')
    };

    // Create and start the language client
    client = new LanguageClient(
        'freyja-lsp',
        'Freyja Language Server',
        serverOptions,
        clientOptions
    );

    // Register commands
    const restartCommand = vscode.commands.registerCommand('freyja.restart', async () => {
        if (client) {
            // Stop the existing client
            await client.stop();
            await client.start();
            vscode.window.showInformationMessage('Freyja Language Server restarted');
        } else {
            // Create and start a new client
            client = new LanguageClient(
                'freyja-lsp',
                'Freyja Language Server',
                serverOptions,
                clientOptions
            );

            try {
                await client.start();
                context.subscriptions.push(client);
                vscode.window.showInformationMessage('Freyja Language Server started');
            } catch (error: any) {
                vscode.window.showErrorMessage(`Failed to start Freyja Language Server: ${error.message}`);
                client = undefined;
            }
        }
    });
    context.subscriptions.push(restartCommand);

    const stopCommand = vscode.commands.registerCommand('freyja.stop', async () => {
        if (client) {
            await client.stop();
            client = undefined;
            vscode.window.showInformationMessage('Freyja Language Server stopped. You can now recompile it.');
        } else {
            vscode.window.showErrorMessage('Freyja Language Server is not running');
        }
    });
    context.subscriptions.push(stopCommand);

    // Start the client
    client.start().then(() => {
        console.log('Freyja Language Server started successfully');
        vscode.window.showInformationMessage('Freyja Language Server is running');
    }).catch((error) => {
        console.error('Failed to start Freyja Language Server:', error);
        vscode.window.showErrorMessage(`Failed to start Freyja Language Server: ${error.message}`);
    });

    context.subscriptions.push(client);
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}