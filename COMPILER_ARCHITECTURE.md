# Odin Compiler Architecture & Freyja Compilation Pipeline

## Odin's Compilation Flow

Based on investigation of Odin's source code, the compilation pipeline follows this structure:

```
Source Files (.odin)
         ↓
    [FRONTEND]
         ↓
    Parser (parser.cpp)
    - Tokenization (tokenizer.cpp)
    - AST Generation (uses core/odin/ast structures)
    - Package management
    - Import resolution
         ↓
    Checker (checker.cpp)
    - Type checking (check_type.cpp)
    - Expression checking (check_expr.cpp)  
    - Statement checking (check_stmt.cpp)
    - Declaration checking (check_decl.cpp)
    - Semantic analysis
    - Name resolution
    - Constant evaluation
         ↓
    [MIDDLE-END]
         ↓
    IR Generation (Two backends)
    1. LLVM Backend (default):
       - llvm_backend.cpp (main coordinator)
       - llvm_backend_expr.cpp (expressions)
       - llvm_backend_stmt.cpp (statements)
       - llvm_backend_proc.cpp (procedures)
       - llvm_backend_type.cpp (types)
       - llvm_backend_const.cpp (constants)
    2. Tilde Backend (experimental):
       - tilde.cpp
       - Custom IR format
         ↓
    [BACKEND]
         ↓
    Optimization
    - LLVM optimization passes
    - Dead code elimination
    - Inlining
         ↓
    Code Generation
    - Target-specific machine code
    - Object file generation
         ↓
    Linking (linker.cpp)
    - System linker invocation
    - Library linking
    - Executable generation
```

## Key Data Structures

### Parser Phase
- `Parser` struct: Manages parsing state
- `ast::File`: Represents a source file
- `ast::Package`: Package container
- `ast::Node`: Base AST node with position info
- `ast::Expr`, `ast::Stmt`, `ast::Decl`: AST node types

### Checker Phase  
- `Checker` struct: Type checking context
- `Entity`: Semantic representation of declarations
- `Type`: Type system representation
- `ExactValue`: Compile-time constant values

### LLVM Backend
- `lbGenerator`: LLVM code generator context
- `lbModule`: LLVM module wrapper
- `lbProcedure`: Procedure generation context
- `lbValue`: LLVM value with Odin type info
- `lbAddr`: Address representation for complex types

## Freyja's Simplified Pipeline

For our initial implementation, we're creating a simplified version:

```
Source Files (.odin syntax)
         ↓
    Odin Parser (reused)
    - Leverage existing tokenizer
    - Use Odin AST structures
         ↓
    Type Checker (stub → full implementation)
    - Basic type inference
    - Fortran-style semantics
    - No-aliasing analysis
         ↓
    LLVM IR Generation (direct)
    - Simple LLVM-C API usage
    - Focus on scientific computing patterns
    - Optimization hints (restrict, SIMD, etc.)
         ↓
    LLVM Tools
    - llc for assembly generation
    - System linker
```

## Next Steps for Proper LLVM Integration

1. **Replace String-based LLVM Generation**
   - Use LLVM-C API directly (like Odin does)
   - Create proper lbGenerator equivalent
   - Handle types properly through LLVM type system

2. **Implement Type System**
   - Create Type representation
   - Type inference for literals
   - Function type checking
   - Array dimension tracking

3. **Memory Management**
   - Use proper allocators (temporary, permanent)
   - Track lifetimes correctly
   - Handle string interning

4. **Scientific Computing Semantics**
   - Implement restrict/no-alias by default
   - Add array operation lowering
   - SIMD pattern recognition
   - Fortran-style array descriptors

## Code Flow in Our Current Implementation

```odin
compile_file()
    ↓
parse_file() // Using Odin's parser
    ↓
type_check() // Stub - just walks AST
    ↓
generate_llvm_ir() // String-based (needs replacement)
    ↓
Write to .ll file
    ↓
Shell out to llc/gcc
```

## Proper LLVM Integration Example

Instead of string generation, we should use LLVM-C API:

```c
// Current (hacky) approach:
llvm_emit(g, "define i32 @main() {")

// Proper approach (using LLVM-C):
LLVMTypeRef func_type = LLVMFunctionType(LLVMInt32Type(), NULL, 0, false);
LLVMValueRef func = LLVMAddFunction(module, "main", func_type);
LLVMBasicBlockRef entry = LLVMAppendBasicBlock(func, "entry");
LLVMPositionBuilderAtEnd(builder, entry);
```

## Dependencies for Full Implementation

1. Link against LLVM libraries
2. Set up LLVM context, module, builder
3. Implement type mapping (Odin types → LLVM types)
4. Create value tracking system
5. Handle debug information generation

## Scientific Computing Optimizations to Add

1. **Restrict Pointers by Default**
   - All array parameters get `noalias` attribute
   - Explicit `#alias` annotation for exceptions

2. **Array Operations**
   - Recognize patterns for vectorization
   - Generate optimal LLVM IR for loops
   - Use LLVM vector types where applicable

3. **Memory Layout**
   - Column-major layout support
   - Contiguous memory guarantees
   - Alignment hints for SIMD

4. **Parallelization Hints**
   - OpenMP-style pragmas → LLVM metadata
   - Reduction pattern recognition
   - Loop dependency analysis