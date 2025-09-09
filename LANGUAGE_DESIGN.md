# Freyja Language Design

## Core Philosophy
Freyja combines Odin's clean, modern syntax with Fortran's performance-oriented semantics for scientific computing.

## Key Semantic Features

### 1. No-Aliasing Semantics
- All function parameters are `restrict` by default
- Compiler assumes no pointer/array aliasing unless explicitly marked
- Enables aggressive loop optimizations and vectorization

### 2. Array-Centric Type System
- Multidimensional arrays as first-class types
- Shape information in type system where possible
- Element-wise operations by default
- Efficient slicing with copy-on-write semantics

### 3. Memory Layout Control
- Column-major layout by default (Fortran compatibility)
- Explicit layout attributes: `#col_major`, `#row_major`
- Guaranteed contiguous memory for arrays
- Structure-of-arrays (SoA) and array-of-structures (AoS) control

### 4. Function Purity & Effects
- `#pure` functions with no side effects
- `#elemental` functions that vectorize automatically
- Explicit effect system for I/O and mutation
- Compiler can reorder/parallelize pure computations

### 5. Parallel Execution Model
- `#parallel` loops with automatic work distribution
- `#simd` hints for vectorization
- Built-in reduction operations
- No data races by construction

### 6. Floating-Point Control
- Strict IEEE-754 mode by default
- Explicit relaxation for performance
- Control over FMA, associativity, and contraction
- Interval arithmetic support

### 7. Compile-Time Computation
- Compile-time array shapes and dimensions
- Static dispatch based on array properties
- Const-generics for scientific algorithms

## Type System Extensions

```odin
// Static arrays with compile-time dimensions
Vector3 :: [3]f64
Matrix3x3 :: [3][3]f64

// Dynamic arrays with runtime dimensions
DynVector :: [dynamic]f64
DynMatrix :: [dynamic][dynamic]f64

// Shaped arrays (dimension known, size runtime)
Matrix2D :: [?, ?]f64  // 2D matrix, size determined at creation

// Units of measurement (optional)
Length :: distinct f64 #unit("m")
Time :: distinct f64 #unit("s")
Velocity :: Length / Time
```

## Memory Model

1. **No Implicit Aliasing**: Pointers to same type cannot alias by default
2. **Explicit Sharing**: Use `#shared` or `#alias` for aliasing
3. **Thread-Local by Default**: Global data is thread-local unless marked `#shared`
4. **Atomic Operations**: Explicit atomic types for concurrent access

## Optimization Guarantees

The compiler guarantees:
- Loop fusion for compatible pure operations
- Automatic vectorization for elemental functions
- No aliasing optimizations enabled by default
- Constant propagation through pure functions
- Dead code elimination for unused pure computations

## Interoperability

- Direct Fortran interop (matching calling conventions)
- C ABI compatibility with explicit marking
- BLAS/LAPACK can be called directly
- Python extension module generation

## Example Program

```odin
package main

import "core:math"
import "core:parallel"

// Matrix multiplication with no-aliasing guarantee
matmul :: proc(C: [?, ?]f64, A: [?, ?]f64, B: [?, ?]f64) #pure {
    m, n, k := dim(A, 0), dim(B, 1), dim(A, 1)
    
    #parallel for i in 0..<m {
        for j in 0..<n {
            sum := 0.0
            #simd for l in 0..<k {
                sum += A[i, l] * B[l, j]
            }
            C[i, j] = sum
        }
    }
}

// Elemental function - works on scalars and arrays
@(elemental)
compute :: proc(x: f64) -> f64 #pure {
    return math.sin(x) * math.exp(-x*x)
}

main :: proc() {
    // Stack-allocated with known size
    A: [100][100]f64
    B: [100][100]f64
    C: [100][100]f64
    
    // Initialize arrays
    #parallel for i, j in indices(A) {
        A[i, j] = f64(i + j)
        B[i, j] = f64(i - j)
    }
    
    // Matrix multiplication
    matmul(C, A, B)
    
    // Apply elemental function to entire array
    result := compute(C)  // Automatically vectorized
    
    // Reduction
    total := #reduce(+) result[:]
}
```