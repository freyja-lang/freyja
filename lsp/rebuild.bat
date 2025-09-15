@echo off
echo Rebuilding Freyja LSP Server...
cd server
odin build . -out:freyja-lsp.exe
if %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    pause
    exit /b 1
)
copy freyja-lsp.exe ..\..\freyja-lsp.exe
echo.
echo LSP Server rebuilt and deployed!
echo.
echo To use the new server:
echo 1. In VSCode, press Ctrl+Shift+P
echo 2. Run "Freyja: Stop Freyja Language Server"
echo 3. Open a .freyja file to restart with the new server
echo.
pause