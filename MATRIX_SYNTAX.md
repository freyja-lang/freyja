 Fortran Array Declaration Syntax:

  ! Static shape
  real :: A(3,4)              ! 3x4 matrix, fixed size

  ! Allocatable (heap, runtime size)
  real, allocatable :: B(:,:)  ! Shape determined at runtime
  allocate(B(m,n))

  ! Assumed-shape (procedure parameters)
  subroutine foo(C)
    real :: C(:,:)            ! Accepts any 2D array, knows bounds

  ! Deferred-shape pointer
  real, pointer :: D(:,:)     ! Can point to sections/slices

  ! Assumed-size (legacy)
  real :: E(3,*)              ! Last dimension size from caller

  Key Fortran Features Missing from Odin:

  1. Array Sections with Strides

  A(2:8:2, 1:5)      ! Rows 2,4,6,8, columns 1-5
  B(10:1:-1)         ! Reverse order (negative stride)
  C(::3, 2)          ! Every 3rd row, column 2

  2. Non-contiguous Views (No Copy)

  real, pointer :: view(:,:)
  view => A(1:10:2, 3:8)  ! Non-contiguous view of A
  ! This is just updating the descriptor, not copying data

  3. Runtime Shape Information (Dope Vectors)

  ! Every array carries metadata:
  ! - base_addr
  ! - element_size
  ! - rank (number of dimensions)
  ! - For each dimension:
  !   - lower_bound
  !   - upper_bound/extent
  !   - stride (in elements or bytes)

  4. Column-Major Storage

  ! Fortran is column-major by default
  ! A(i,j) means column i, row j in memory layout
  ! Better for BLAS/LAPACK operations

  5. Reshaping Without Copying

  real :: A(2,3), B(6)
  B = reshape(A, [6])  ! Can be just descriptor manipulation

  Potential Freyja Syntax Options:

  // Option 1: Similar to Odin but with 'matrix' keyword
  matrix[f32]           // Fully dynamic, rank unknown
  matrix[f32, :, :]     // 2D dynamic matrix
  matrix[f32, 3, 4]     // Static 3x4 matrix
  matrix[f32, 3, :]     // First dim static, second dynamic

  // Option 2: More Fortran-like
  matrix(f32)           // Dynamic
  matrix(3,4)f32        // Static shape
  matrix(:,:)f32        // Dynamic 2D

  // Option 3: Attribute-based
  [matrix, column_major] [dynamic]f32     // 1D dynamic
  [matrix(2), column_major] [3][4]f32     // 2D static

  // Slicing syntax options:
  A[2:8:2, 1:5]        // Fortran-style
  A[2..8 by 2, 1..5]   // More Odin-like
  A[2..=8:2, 1..=5]    // Rust-inspired

  // View/pointer syntax:
  view := &A[::2, 3:5]  // Non-contiguous view
  ptr := matrix_view(A, stride=[2,1], offset=[0,3])

  What We Need to Support:

  1. Descriptor-based arrays (not just pointer + length)
  2. Stride information per dimension
  3. Runtime bounds checking with full bound info
  4. Non-contiguous views without copying
  5. Column-major as first-class (not just row-major)
  6. Reshaping operations that just manipulate descriptors
  7. Automatic interface with BLAS/LAPACK (they expect Fortran-style)



  ---------
  Great thinking! Fortran arrays are definitely not limited to 2D - they support up to 15 dimensions (though 7 is the minimum required by the standard).
   Let's refine the syntax:

  Proposed Freyja Matrix Syntax:

  // Basic syntax
  matrix[f32]              // Fully dynamic, rank determined at runtime
  matrix[:, :]f32          // 2D dynamic matrix
  matrix[:, :, :]f32       // 3D dynamic matrix
  matrix[3, 4]f32          // Static 3x4 matrix
  matrix[3, :]f32          // Mixed: first dim static, second dynamic
  matrix[:10, :]f32        // Upper bound constraint (0..9)
  matrix[1:, :]f32         // Lower bound specified (1-indexed!)

  // Slicing/striding inside declarations
  matrix[::2, :]f32        // Every 2nd element in first dim (stride=2)
  matrix[2:8:2, :]f32      // Start:end:stride syntax

  // Layout control
  #row_major matrix[3, 4]f32     // Override default column-major
  #column_major matrix[3, 4]f32  // Explicit (default)

  What This Doesn't Cover Yet:

  1. Assumed-Shape Parameters

  In Fortran, procedure parameters can accept any shape:
  subroutine process(A)
    real :: A(:,:)  ! Accepts any 2D array, runtime bounds
  Maybe in Freyja:
  proc(A: matrix[?, ?]f32)  // ? means assumed from caller

  2. Pointer/View Syntax

  Creating non-contiguous views without copying:
  // How to express taking a view?
  view := matrix_view(A, [2:8:2, 1:5])  // Function call?
  view := &A[2:8:2, 1:5]                // Pointer-like?
  view := A.[2:8:2, 1:5]                // Special syntax?

  3. Reshaping

  Changing dimensions without copying:
  B := reshape(A, [6, 2])           // Function?
  B := A as matrix[6, 2]f32         // Cast-like?
  B := matrix[6, 2]f32(A)           // Conversion?

  4. Allocatable vs Fixed

  Fortran distinguishes between:
  - Fixed arrays (stack, size at compile time)
  - Allocatable (heap, size at runtime, can be reallocated)
  - Pointer arrays (can point to sections)

  Maybe:
  A: matrix[3, 4]f32              // Stack, fixed
  B: ^matrix[:, :]f32             // Heap, allocatable
  C: matrix_ptr[:, :]f32          // Can point to sections

  5. Broadcasting/Conformability

  // Should these work automatically?
  C = A + B  // Where A is [3,4] and B is [3,1] - broadcast B?
  D = A + 5  // Scalar broadcast?

  6. Fortran's WHERE Construct

  Masked operations:
  WHERE (A > 0)
    B = sqrt(A)
  ELSEWHERE
    B = 0
  END WHERE

  Possible Freyja:
  where A > 0 {
    B = sqrt(A)
  } else {
    B = 0
  }

  7. Array Intrinsics

  Fortran has many builtin array operations:
  size(A, dim=2)     ! Size of dimension 2
  shape(A)           ! Array of dimensions
  lbound(A, 1)       ! Lower bound of dimension 1
  ubound(A, 2)       ! Upper bound of dimension 2
  transpose(A)       ! Matrix transpose
  matmul(A, B)       ! Matrix multiplication
  dot_product(v, w)  ! Dot product

  Implementation Considerations:

  The descriptor structure would need:
  typedef struct {
      void* base_addr;
      size_t elem_size;
      int rank;
      struct {
          int64_t lower_bound;
          int64_t extent;      // upper - lower + 1
          int64_t stride;      // in elements
      } dims[];  // Flexible array member
  } MatrixDescriptor;

  What aspects are most important to you? Should we prioritize:
  1. Full slicing/striding in the type system?
  2. Fortran-BLAS compatibility?
  3. Simplicity for common cases?

  ------

  Excellent point! A 10000x10000 dense f32 matrix would be ~400MB - definitely not stack material. You're right that the syntax should be clean and
  unified. Here's a design approach:

  Unified Syntax with Smart Allocation:

  // Syntax is the same, allocation strategy differs
  A: matrix[3, 4]f32           // Small, stack allocated
  B: matrix[1000, 1000]f32     // Large, heap allocated automatically
  C: matrix[:, :]f32           // Dynamic, always heap

  // Explicit control when needed
  D: matrix[100, 100]f32 #force_heap    // Override heuristic
  E: matrix[10, 10]f32 #force_stack     // Override (compiler can reject if too large)

  // In procedures, all are descriptors pointing to data
  proc(M: matrix[?, ?]f32) {    // M is just a descriptor
      // Don't care if M's data is stack or heap
      delete(M)  // Safe - warns/no-ops if stack allocated
  }

  Internal Representation:

  typedef struct {
      void* base_addr;
      size_t elem_size;
      uint8_t rank;
      uint8_t flags;        // Bit 0: heap_allocated, Bit 1: col_major, etc.
      struct {
          int64_t lower_bound;
          int64_t extent;
          int64_t stride;
      } dims[];
  } MatrixDescriptor;

  Allocation Heuristic:

  // Compiler decides based on:
  // 1. Total size (e.g., > 64KB goes to heap)
  // 2. Whether dimensions are static
  // 3. Scope lifetime analysis

  matrix[10, 10]f32        // 400 bytes - stack
  matrix[100, 100]f32      // 40KB - probably stack
  matrix[1000, 1000]f32    // 4MB - heap
  matrix[:, :]f32          // Dynamic - heap
  matrix[N, M]f32          // Runtime dims - heap

  View/Slice Semantics:

  // Views never allocate, just update descriptor
  A: matrix[1000, 1000]f32            // Heap allocated
  B := A[::2, ::2]                     // B is view, no allocation
  C := A[100:200, :]                   // C is view
  D := copy(A[100:200, :])             // D is new allocation

  // The descriptor knows if it owns the data
  delete(A)  // Frees memory
  delete(B)  // Warning: view doesn't own data
  delete(D)  // Frees D's copy

  Lifetime Management:

  foo :: proc() -> matrix[?, ?]f32 {
      A: matrix[100, 100]f32          // Stack allocated
      return A                         // Compiler promotes to heap!
  }

  bar :: proc() {
      A: matrix[100, 100]f32          // Stack
      B := A[10:20, :]                // View of stack data

      defer {
          delete(A)  // No-op, stack allocated
          delete(B)  // No-op, just a view
      }
  }

  Alternative: Explicit but Clean:

  // If we want more explicit control:
  A: matrix[100, 100]f32              // Stack by default if small
  B: new(matrix[1000, 1000]f32)       // Explicitly heap
  C: auto matrix[N, M]f32             // Let compiler decide

  // Or using a builtin proc
  A := make_matrix(f32, 1000, 1000)   // Always heap
  B := stack_matrix(f32, 10, 10)      // Always stack (compile error if too large)

  For the Implementation:

  We could start simple:
  1. Static small matrices (<64KB) → stack
  2. Everything else → heap
  3. All matrices carry the allocation flag
  4. delete() checks the flag

  This gives us:
  - Clean, unified syntax
  - Automatic safety (no stack overflow)
  - Explicit control when needed
  - Fortran-style descriptors for compatibility
  - Zero-cost views/slices

  What do you think? Should we go with implicit smart allocation or make heap/stack more explicit?