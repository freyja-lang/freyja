package llvm
/*===-- llvm-c/BitWriter.h - BitWriter Library C Interface ------*- C++ -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMBitWriter.a, which          *|
|* implements output of the LLVM bitcode format.                              *|
|*                                                                            *|
|* Many exotic languages can interoperate with C code but have a harder time  *|
|* with C++ due to name mangling. So in addition to C, this interface enables *|
|* tools written in such languages.                                           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/
import "core:c"

_ :: c

when ODIN_OS == .Linux {
	foreign import lib "system:LLVM-18"
} else when ODIN_OS == .Windows {
	foreign import lib "LLVM-C.lib"
} else when ODIN_OS == .Darwin {
	foreign import lib "system:LLVM"
}


// LLVM_C_BITWRITER_H ::

@(default_calling_convention = "c", link_prefix = "")
foreign lib {
	/** Writes a module to the specified path. Returns 0 on success. */
	LLVMWriteBitcodeToFile :: proc(M: LLVMModuleRef, Path: cstring) -> c.int ---

	/** Writes a module to an open file descriptor. Returns 0 on success. */
	LLVMWriteBitcodeToFD :: proc(M: c.int, FD: c.int, ShouldClose: c.int, Unbuffered: c.int) -> c.int ---

	/** Deprecated for LLVMWriteBitcodeToFD. Writes a module to an open file
	descriptor. Returns 0 on success. Closes the Handle. */
	LLVMWriteBitcodeToFileHandle :: proc(M: c.int, Handle: c.int) -> c.int ---

	/** Writes a module to a new memory buffer and returns it. */
	LLVMWriteBitcodeToMemoryBuffer :: proc(M: c.int) -> c.int ---
}
