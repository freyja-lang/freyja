// How Freyja could handle matrix builtins in the compiler
package freyja_builtins

// Option 1: Make `mul` a compiler intrinsic that dispatches based on types
// The Freyja compiler would recognize these patterns and generate optimized code

// In the compiler, `mul` would be an intrinsic that maps to:
// - scalar * scalar -> regular multiplication
// - matrix * matrix -> dgemm
// - matrix * vector -> dgemv  
// - scalar * matrix -> dscal on each element
// - scalar * vector -> dscal

// The compiler would generate these as builtins:
@(builtin)
mul_mat_mat :: proc(A: Matrix[$M,$K,$T], B: Matrix[$K,$N,$T]) -> Matrix[M,N,T] {
    // Compiler generates optimized dgemm call
    // with cache blocking, SIMD, etc.
}

@(builtin)
mul_mat_vec :: proc(A: Matrix[$M,$N,$T], x: Vector[$N,$T]) -> Vector[M,T] {
    // Compiler generates optimized dgemv call
}

@(builtin)
mul_vec_mat :: proc(x: Vector[$M,$T], A: Matrix[$M,$N,$T]) -> Vector[N,T] {
    // Compiler generates optimized dgemv with transpose
}

@(builtin)
mul_scalar_mat :: proc(alpha: $T, A: Matrix[$M,$N,$T]) -> Matrix[M,N,T] {
    // Compiler generates SIMD scaling loop
}

@(builtin)
mul_scalar_vec :: proc(alpha: $T, x: Vector[$N,$T]) -> Vector[N,T] {
    // Compiler generates dscal or SIMD loop
}

// Option 2: Make them all separate builtins with a proc group in the runtime
// This is more like what Odin does with `dot`

@(builtin, link_name="__freyja_dgemm")
dgemm :: proc(A: Matrix, B: Matrix) -> Matrix

@(builtin, link_name="__freyja_dgemv") 
dgemv :: proc(A: Matrix, x: Vector) -> Vector

@(builtin, link_name="__freyja_dscal_mat")
dscal_mat :: proc(alpha: f64, A: Matrix) -> Matrix

@(builtin, link_name="__freyja_dscal_vec")
dscal_vec :: proc(alpha: f64, x: Vector) -> Vector

// Then in the generated bindings:
mul :: proc{dgemm, dgemv, dscal_mat, dscal_vec}

// Option 3: Operator overloading in the compiler
// The * operator could be overloaded for matrix types directly

// In Freyja source:
/*
    C := A * B        // Calls dgemm
    y := A * x        // Calls dgemv  
    B := 2.0 * A      // Calls dscal_mat
    y := alpha * x    // Calls dscal_vec
*/

// The compiler would lower these to the appropriate BLAS calls

// Option 4: Template-like approach with compiler magic
// Similar to C++ expression templates but resolved at compile time

@(builtin)
matrix_expr :: proc($Expr: typeid) -> result_type(Expr) {
    // Compiler analyzes the expression tree and generates optimal code
    // e.g., A * B * C could be reordered for efficiency
    // or alpha * A * x could be fused into a single dgemv with alpha
}

// Usage in Freyja:
/*
    // These would all be optimized:
    D := mul(A, B, C)           // Optimal parenthesization
    y := mul(2.0, A, x)         // Fused scaling and multiply
    z := mul(A + B, x)          // Fused addition and multiply
*/

// IMPLEMENTATION STRATEGY FOR FREYJA:

// 1. Start with explicit builtins (Option 2)
//    - Easy to implement
//    - Clear mapping to BLAS
//    - Can add proc groups in generated Odin

// 2. Add operator overloading (Option 3)
//    - Better ergonomics
//    - Compiler handles dispatch
//    - Still maps to same builtins

// 3. Eventually add expression optimization (Option 4)
//    - Advanced optimization
//    - Fusion of operations
//    - Optimal evaluation order

// The generated Odin bindings would look like:

foreign freyja_lib {
    // Low-level BLAS-style (hidden)
    @(link_name="__freyja_dgemm")
    _dgemm :: proc(transA, transB: u8, m, n, k: i32, 
                   alpha: f64, A: ^f64, lda: i32,
                   B: ^f64, ldb: i32,
                   beta: f64, C: ^f64, ldc: i32) ---
    
    @(link_name="__freyja_dgemv")
    _dgemv :: proc(trans: u8, m, n: i32,
                   alpha: f64, A: ^f64, lda: i32,
                   x: ^f64, incx: i32,
                   beta: f64, y: ^f64, incy: i32) ---
}

// High-level wrappers with proc groups
mul_mat_mat :: proc(A, B: Matrix) -> Matrix {
    C := matrix(A.rows, B.cols)
    _dgemm('N', 'N', cast(i32)A.rows, cast(i32)B.cols, cast(i32)A.cols,
           1.0, A.data, cast(i32)A.stride,
           B.data, cast(i32)B.stride,
           0.0, C.data, cast(i32)C.stride)
    return C
}

mul_mat_vec :: proc(A: Matrix, x: Vector) -> Vector {
    y := vector(A.rows)
    _dgemv('N', cast(i32)A.rows, cast(i32)A.cols,
           1.0, A.data, cast(i32)A.stride,
           x.data, cast(i32)x.stride,
           0.0, y.data, cast(i32)y.stride)
    return y
}

// ... more overloads ...

mul :: proc{mul_mat_mat, mul_mat_vec, mul_scalar_mat, mul_scalar_vec}