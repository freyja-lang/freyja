@echo off

:: Build the Freyja Language Server
odin build . -out:freyja-lsp.exe

if %errorlevel% equ 0 (
    echo Build successful: freyja-lsp.exe
) else (
    echo Build failed
    exit /b 1
)