package llvm
/*===-- llvm-c/Analysis.h - Analysis Library C Interface --------*- C++ -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMAnalysis.a, which           *|
|* implements various analyses of the LLVM IR.                                *|
|*                                                                            *|
|* Many exotic languages can interoperate with C code but have a harder time  *|
|* with C++ due to name mangling. So in addition to C, this interface enables *|
|* tools written in such languages.                                           *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/
import "core:c"


when ODIN_OS == .Linux {
	foreign import lib "system:LLVM-18"
} else when ODIN_OS == .Windows {
	foreign import lib "LLVM-C.lib"
} else when ODIN_OS == .Darwin {
	foreign import lib "system:LLVM"
}


// LLVM_C_ANALYSIS_H ::

LLVMVerifierFailureAction :: c.int

@(default_calling_convention = "c", link_prefix = "")
foreign lib {
	/* Verifies that a module is valid, taking the specified action if not.
	Optionally returns a human-readable description of any invalid constructs.
	OutMessage must be disposed with LLVMDisposeMessage. */
	LLVMVerifyModule :: proc(M: c.int, Action: LLVMVerifierFailureAction, OutMessage: [^]cstring) -> c.int ---

	/* Verifies that a single function is valid, taking the specified action. Useful
	for debugging. */
	LLVMVerifyFunction :: proc(Fn: c.int, Action: LLVMVerifierFailureAction) -> c.int ---

	/* Open up a ghostview window that displays the CFG of the current function.
	Useful for debugging. */
	LLVMViewFunctionCFG :: proc(Fn: c.int) ---
	LLVMViewFunctionCFGOnly :: proc(Fn: c.int) ---
}
