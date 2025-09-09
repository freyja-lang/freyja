# Freyja Compiler Project - Claude Context

## Project Overview

**Freyja** is a new programming language that uses Odin syntax with Fortran-style semantics for scientific computing. The project follows a "baby steps" approach, starting with basic compilation infrastructure before adding new language features.

## Current Architecture

The compiler follows a 5-stage pipeline modeled after Odin's architecture:

```
Parse → Check → Generate IR → Optimize → Link
```

### Stage Details

1. **Parse** (`parser/parser.odin`) - Uses Odin's official parser/AST
2. **Check** (`checker/checker.odin`) - Two-phase type checking following Odin's pattern
3. **IR Generation** (`backend/llvm/ir_gen.odin`) - LLVM-C API for code generation  
4. **Optimize** (`backend/llvm/optimizer.odin`) - LLVM optimization passes (stub)
5. **Link** (`backend/llvm/codegen.odin`) - System linker integration (stub)

## Key Implementation Decisions

### Parser Strategy
- **Decision**: Use Odin's existing parser/AST instead of creating custom parser
- **Rationale**: Focus on semantics/codegen rather than syntax parsing
- **Implementation**: Direct integration with `core:odin/ast` and `core:odin/parser`

### Checker Architecture  
- **Decision**: Model after Odin's CheckerInfo/Entity/Scope system
- **Rationale**: Proven architecture for complex semantic analysis
- **Implementation**: Two-phase checking (declarations → procedure bodies)

### LLVM Integration
- **Decision**: Use LLVM-C API via odin-c-bindgen
- **Rationale**: Type-safe bindings with proper error handling
- **Implementation**: Direct IR generation (no string-based approach)

## Current Status

### ✅ Working Features

1. **Basic Compilation Pipeline** - All 5 stages connected and working
2. **Odin Parser Integration** - Real AST parsing of `.freyja` files
3. **LLVM-C Bindings** - Fixed bindgen-generated signatures with proper types
4. **Two-Phase Type Checker** - Following Odin's architectural patterns:
   - Phase 1: Declaration pass (procedure signatures)
   - Phase 2: Body checking pass (local variables, statements)
5. **Entity System** - Hierarchical symbol tables with proper scoping
6. **Statement Processing** - Handles declarations, assignments, expressions, blocks
7. **Expression Type Checking** - Scope-aware variable lookup and type inference
8. **Error Reporting System** - Odin-style error collection with source positions and formatted messages

### Test Case Working
File: `tests/test.freyja`
```odin
package main

main :: proc() {
    a := 1
    b := 2  
    c := a + b
}
```

**Output**: Successfully compiles through all stages, generates LLVM bitcode

## File Structure

```
/mnt/d/dev/freyja/
├── main.odin              # Main compilation orchestrator
├── parser/
│   └── parser.odin        # Odin parser integration
├── checker/
│   ├── checker.odin       # Two-phase type checker
│   ├── error.odin         # Error reporting system
│   └── plan.md            # Checker implementation plan
├── backend/
│   └── llvm/              # LLVM backend implementation
│       ├── ir_gen.odin    # LLVM IR generation
│       ├── optimizer.odin # LLVM optimization passes
│       ├── codegen.odin   # Code generation/linking
│       └── test_llvm.odin # LLVM testing utilities
├── llvm/                  # LLVM-C bindings
│   ├── Core.odin          # Core LLVM functions
│   ├── Types.odin         # LLVM type system
│   └── BitWriter.odin     # Bitcode writing
├── tests/
│   └── test.freyja        # Test program
├── out/                   # Build outputs
└── old/                   # Legacy code (odin-c-bindgen, etc.)
```

## Critical Implementation Details

### AST Field Access
- **Statements**: Use `stmt.derived_stmt` (not `stmt.derived`)
- **Expressions**: Use `expr.derived_expr` (not `expr.derived`)
- **Statement Types**: `ast.Expr_Stmt`, `ast.Assign_Stmt`, `ast.Value_Decl`, `ast.Block_Stmt`

### LLVM Type Safety
- **Opaque Types**: Use `distinct rawptr` (not `struct {}`) to prevent segfaults
- **Context Management**: Always pass LLVMContextRef to type creation functions
- **Function Signatures**: Fixed from bindgen errors - return proper LLVM types

### Checker Patterns
- **Entity States**: `UNRESOLVED` → `IN_PROGRESS` → `RESOLVED`
- **Scope Hierarchy**: `UNIVERSE` (builtins) → `PACKAGE` → `PROCEDURE` → `BLOCK`
- **Deferred Processing**: Use `procedure_queue` for two-phase checking

## Known Issues & TODOs

### Short-term (Next Sessions)
1. **Enhanced IR Generation** - Generate proper local variable allocas and stores
2. **Function Types** - Replace `builtin_void` with proper procedure types
3. **Parameter Handling** - Add procedure parameters to scope
4. **Source Position Tracking** - Extract real source positions from AST nodes for better error messages

### Medium-term
1. **Type System** - Implement full Freyja type inference rules
2. **Scientific Computing Features** - Array operations, restrict pointers, SIMD
3. **Optimization Pipeline** - Hook up LLVM optimization passes
4. **Linking** - Proper executable generation

### Long-term
1. **Fortran Semantics** - Column-major arrays, no-alias by default
2. **Performance** - Vectorization hints, parallel constructs
3. **Interop** - C/Fortran library integration

## Build & Test Commands

```bash
# Build compiler
odin build . -out:freyja

# Test compilation
./freyja tests/test.freyja

# Run lint/typecheck (when available)
odin check .
```

## Dependencies

- **Odin Compiler** - Available at `/mnt/c/odin`
- **LLVM Libraries** - System installation with C bindings
- **libclang** - For bindgen (symlinked as `libclang.so`)

## Development Context

### Previous Major Refactors
1. **Parser Integration** - Moved from stub to real Odin parser
2. **LLVM Bindings** - Fixed bindgen-generated signatures manually
3. **Checker Architecture** - Refactored to match Odin's patterns
4. **Two-Phase Implementation** - Added deferred procedure checking
5. **Error Reporting System** - Implemented Odin-style error collection and display
6. **Backend Organization** - Moved LLVM code to `backend/llvm/` structure

### Design Philosophy
- **Follow Odin Patterns** - Mirror proven compiler architecture
- **Scientific Computing Focus** - Optimize for numerical workloads
- **Incremental Development** - Baby steps, verify at each stage
- **Type Safety** - Leverage Odin's type system for compiler robustness

## Future Sessions Notes

When continuing development:
1. Always test with `./freyja tests/test.freyja` after changes
2. Use `odin build . -out:freyja` to verify compilation
3. Refer to `/mnt/c/odin/core/odin/ast/ast.odin` for AST structure
4. Check `checker/plan.md` for detailed implementation guidance
5. Follow Odin's checker patterns in `/mnt/c/odin/src/` (C++ source)
6. The odin compiler's architecture is documented in COMPILER_ARCHITECTURE.md

## Success Metrics

The compiler successfully:
- ✅ Parses Odin syntax using official parser
- ✅ Performs two-phase semantic analysis
- ✅ Discovers and types local variables in procedures  
- ✅ Generates valid LLVM IR with proper function definitions
- ✅ Maintains hierarchical symbol tables with scope resolution
- ✅ Handles basic expressions (literals, identifiers, binary ops)

Next milestone: Generate proper local variable IR with alloca/load/store patterns.