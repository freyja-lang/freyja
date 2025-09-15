@echo off
setlocal enabledelayedexpansion

echo Building Freyja LSP and VSCode Extension...

:: Build the LSP server
echo Building LSP server...
cd server
odin build . -out:freyja-lsp.exe
if %errorlevel% neq 0 (
    echo Error: Failed to build LSP server
    exit /b 1
)
echo LSP server built: freyja-lsp.exe
cd ..

:: Build the VSCode extension
echo Building VSCode extension...
cd vscode

:: Install dependencies if needed
if not exist node_modules (
    echo Installing npm dependencies...
    call npm install
    if %errorlevel% neq 0 (
        echo Error: Failed to install npm dependencies
        exit /b 1
    )
)

:: Compile TypeScript
echo Compiling TypeScript...
call npm run compile
if %errorlevel% neq 0 (
    echo Error: Failed to compile TypeScript
    exit /b 1
)

echo VSCode extension compiled

:: Optional: Package the extension
where vsce >nul 2>nul
if %errorlevel% equ 0 (
    echo Packaging extension...
    call vsce package --no-dependencies
    if %errorlevel% equ 0 (
        echo Extension packaged as .vsix
    ) else (
        echo Warning: Failed to package extension
    )
) else (
    echo Note: Install vsce to package the extension: npm install -g vsce
)

cd ..

echo.
echo Build complete!
echo To use the extension:
echo 1. Open the vscode folder in VSCode
echo 2. Press F5 to test in a new VSCode window
echo 3. Or install the .vsix file via Extensions menu - Install from VSIX