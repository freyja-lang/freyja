#!/bin/bash

# Build Freyja kernel library strategy

echo "==================================="
echo "Freyja Kernel Library Build Options"
echo "==================================="
echo ""

# Option 1: Thin static library (requires BLAS at link time)
echo "Option 1: Thin Static Library"
echo "------------------------------"
echo "Build:"
echo "  llc -filetype=obj out/output.ll -o out/freyja_kernels.o"
echo "  ar rcs out/libfreyja_kernels.a out/freyja_kernels.o"
echo ""
echo "User links with:"
echo "  gcc main.c libfreyja_kernels.a -lopenblas"
echo "  # Size: ~10KB library + user provides BLAS"
echo ""

# Option 2: Fat static library (includes BLAS)
echo "Option 2: Fat Static Library (Self-Contained)"
echo "----------------------------------------------"
echo "Build:"
echo "  llc -filetype=obj out/output.ll -o out/freyja_kernels.o"
echo "  # Combine with BLAS into single archive"
echo "  mkdir -p out/tmp && cd out/tmp"
echo "  ar x ../../vendor/linalg/linux-x64/libopenblas.a"
echo "  ar x ../freyja_kernels.o"
echo "  ar rcs ../libfreyja_complete.a *.o"
echo "  cd ../.."
echo "  # Size: ~60MB but completely self-contained"
echo ""

# Option 3: Shared library with BLAS statically linked
echo "Option 3: Shared Library with Static BLAS"
echo "-----------------------------------------"
echo "Build:"
echo "  clang -shared out/output.ll \\"
echo "    -Wl,--whole-archive vendor/linalg/linux-x64/libopenblas.a \\"
echo "    -Wl,--no-whole-archive \\"
echo "    -o out/libfreyja.so"
echo "  # Size: ~30MB shared library"
echo ""

# Option 4: Smart linking (only include used BLAS functions)
echo "Option 4: Optimized Static Library"
echo "-----------------------------------"
echo "Build:"
echo "  # Use LTO and gc-sections to minimize size"
echo "  clang -flto -c out/output.ll -o out/freyja_kernels.o"
echo "  clang -flto -Wl,--gc-sections \\"
echo "    out/freyja_kernels.o \\"
echo "    vendor/linalg/linux-x64/libopenblas.a \\"
echo "    -o out/libfreyja_opt.a"
echo "  # Size: Only includes USED BLAS functions"
echo ""