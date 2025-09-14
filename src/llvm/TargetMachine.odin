package llvm
/*===-- llvm-c/TargetMachine.h - Target Machine Library C Interface - C++ -*-=*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to the Target and TargetMachine       *|
|* classes, which can be used to generate assembly or object files.           *|
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


// LLVM_C_TARGETMACHINE_H ::

LLVMTargetMachineOptionsRef :: distinct rawptr

LLVMTargetMachineRef :: distinct rawptr

LLVMTargetRef :: distinct rawptr

LLVMCodeGenOptLevel :: enum c.uint {
	None,
	Less,
	Default,
	Aggressive,
}

LLVMRelocMode :: enum c.uint {
	Default,
	Static,
	PIC,
	DynamicNoPic,
	ROPI,
	RWPI,
	ROPI_RWPI,
}

LLVMCodeModel :: enum c.uint {
	Default,
	JITDefault,
	Tiny,
	Small,
	Kernel,
	Medium,
	Large,
}

LLVMCodeGenFileType :: enum c.uint {
	AssemblyFile,
	ObjectFile,
}

LLVMGlobalISelAbortMode :: enum c.uint {
	Enable,
	Disable,
	DisableWithDiag,
}

@(default_calling_convention = "c", link_prefix = "")
foreign lib {
	/** Returns the first llvm::Target in the registered targets list. */
	LLVMGetFirstTarget :: proc() -> LLVMTargetRef ---

	/** Returns the next llvm::Target given a previous one (or null if there's none) */
	LLVMGetNextTarget :: proc(T: LLVMTargetRef) -> LLVMTargetRef ---

	/*===-- Target ------------------------------------------------------------===*/
	/** Finds the target corresponding to the given name and stores it in \p T.
	Returns 0 on success. */
	LLVMGetTargetFromName :: proc(Name: cstring) -> LLVMTargetRef ---

	/** Finds the target corresponding to the given triple and stores it in \p T.
	Returns 0 on success. Optionally returns any error in ErrorMessage.
	Use LLVMDisposeMessage to dispose the message. */
	LLVMGetTargetFromTriple :: proc(Triple: cstring, T: ^LLVMTargetRef, ErrorMessage: [^]cstring) -> c.int ---

	/** Returns the name of a target. See llvm::Target::getName */
	LLVMGetTargetName :: proc(T: LLVMTargetRef) -> cstring ---

	/** Returns the description  of a target. See llvm::Target::getDescription */
	LLVMGetTargetDescription :: proc(T: LLVMTargetRef) -> cstring ---

	/** Returns if the target has a JIT */
	LLVMTargetHasJIT :: proc(T: LLVMTargetRef) -> c.int ---

	/** Returns if the target has a TargetMachine associated */
	LLVMTargetHasTargetMachine :: proc(T: LLVMTargetRef) -> c.int ---

	/** Returns if the target as an ASM backend (required for emitting output) */
	LLVMTargetHasAsmBackend :: proc(T: LLVMTargetRef) -> c.int ---

	/*===-- Target Machine ----------------------------------------------------===*/
	/**
	* Create a new set of options for an llvm::TargetMachine.
	*
	* The returned option structure must be released with
	* LLVMDisposeTargetMachineOptions() after the call to
	* LLVMCreateTargetMachineWithOptions().
	*/
	LLVMCreateTargetMachineOptions :: proc() -> LLVMTargetMachineOptionsRef ---

	/**
	* Dispose of an LLVMTargetMachineOptionsRef instance.
	*/
	LLVMDisposeTargetMachineOptions :: proc(Options: LLVMTargetMachineOptionsRef) ---
	LLVMTargetMachineOptionsSetCPU :: proc(Options: LLVMTargetMachineOptionsRef, CPU: cstring) ---

	/**
	* Set the list of features for the target machine.
	*
	* \param Features a comma-separated list of features.
	*/
	LLVMTargetMachineOptionsSetFeatures :: proc(Options: LLVMTargetMachineOptionsRef, Features: cstring) ---
	LLVMTargetMachineOptionsSetABI :: proc(Options: LLVMTargetMachineOptionsRef, ABI: cstring) ---
	LLVMTargetMachineOptionsSetCodeGenOptLevel :: proc(Options: LLVMTargetMachineOptionsRef, Level: LLVMCodeGenOptLevel) ---
	LLVMTargetMachineOptionsSetRelocMode :: proc(Options: LLVMTargetMachineOptionsRef, Reloc: LLVMRelocMode) ---
	LLVMTargetMachineOptionsSetCodeModel :: proc(Options: LLVMTargetMachineOptionsRef, CodeModel: LLVMCodeModel) ---

	/**
	* Create a new llvm::TargetMachine.
	*
	* \param T the target to create a machine for.
	* \param Triple a triple describing the target machine.
	* \param Options additional configuration (see
	*                LLVMCreateTargetMachineOptions()).
	*/
	LLVMCreateTargetMachineWithOptions :: proc(T: LLVMTargetRef, Triple: cstring, Options: LLVMTargetMachineOptionsRef) -> LLVMTargetMachineRef ---

	/** Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine */
	LLVMCreateTargetMachine :: proc(T: LLVMTargetRef, Triple: cstring, CPU: cstring, Features: cstring, Level: LLVMCodeGenOptLevel, Reloc: LLVMRelocMode, CodeModel: LLVMCodeModel) -> LLVMTargetMachineRef ---

	/** Dispose the LLVMTargetMachineRef instance generated by
	LLVMCreateTargetMachine. */
	LLVMDisposeTargetMachine :: proc(T: LLVMTargetMachineRef) ---

	/** Returns the Target used in a TargetMachine */
	LLVMGetTargetMachineTarget :: proc(T: LLVMTargetMachineRef) -> LLVMTargetRef ---

	/** Returns the triple used creating this target machine. See
	llvm::TargetMachine::getTriple. The result needs to be disposed with
	LLVMDisposeMessage. */
	LLVMGetTargetMachineTriple :: proc(T: LLVMTargetMachineRef) -> cstring ---

	/** Returns the cpu used creating this target machine. See
	llvm::TargetMachine::getCPU. The result needs to be disposed with
	LLVMDisposeMessage. */
	LLVMGetTargetMachineCPU :: proc(T: LLVMTargetMachineRef) -> cstring ---

	/** Returns the feature string used creating this target machine. See
	llvm::TargetMachine::getFeatureString. The result needs to be disposed with
	LLVMDisposeMessage. */
	LLVMGetTargetMachineFeatureString :: proc(T: LLVMTargetMachineRef) -> cstring ---

	/** Create a DataLayout based on the targetMachine. */
	LLVMCreateTargetDataLayout :: proc(T: LLVMTargetMachineRef) -> c.int ---

	/** Set the target machine's ASM verbosity. */
	LLVMSetTargetMachineAsmVerbosity :: proc(T: LLVMTargetMachineRef, VerboseAsm: c.int) ---

	/** Enable fast-path instruction selection. */
	LLVMSetTargetMachineFastISel :: proc(T: LLVMTargetMachineRef, Enable: c.int) ---

	/** Enable global instruction selection. */
	LLVMSetTargetMachineGlobalISel :: proc(T: LLVMTargetMachineRef, Enable: c.int) ---

	/** Set abort behaviour when global instruction selection fails to lower/select
	* an instruction. */
	LLVMSetTargetMachineGlobalISelAbort :: proc(T: LLVMTargetMachineRef, Mode: LLVMGlobalISelAbortMode) ---

	/** Enable the MachineOutliner pass. */
	LLVMSetTargetMachineMachineOutliner :: proc(T: LLVMTargetMachineRef, Enable: c.int) ---

	/** Emits an asm or object file for the given module to the filename. This
	wraps several c++ only classes (among them a file stream). Returns any
	error in ErrorMessage. Use LLVMDisposeMessage to dispose the message. */
	LLVMTargetMachineEmitToFile :: proc(T: LLVMTargetMachineRef, M: c.int, Filename: cstring, codegen: LLVMCodeGenFileType, ErrorMessage: [^]cstring) -> c.int ---

	/** Compile the LLVM IR stored in \p M and store the result in \p OutMemBuf. */
	LLVMTargetMachineEmitToMemoryBuffer :: proc(T: LLVMTargetMachineRef, M: c.int, codegen: LLVMCodeGenFileType, ErrorMessage: [^]cstring, OutMemBuf: ^c.int) -> c.int ---

	/*===-- Triple ------------------------------------------------------------===*/
	/** Get a triple for the host machine as a string. The result needs to be
	disposed with LLVMDisposeMessage. */
	LLVMGetDefaultTargetTriple :: proc() -> cstring ---

	/** Normalize a target triple. The result needs to be disposed with
	LLVMDisposeMessage. */
	LLVMNormalizeTargetTriple :: proc(triple: cstring) -> cstring ---

	/** Get the host CPU as a string. The result needs to be disposed with
	LLVMDisposeMessage. */
	LLVMGetHostCPUName :: proc() -> cstring ---

	/** Get the host CPU's features as a string. The result needs to be disposed
	with LLVMDisposeMessage. */
	LLVMGetHostCPUFeatures :: proc() -> cstring ---

	/** Adds the target-specific analysis passes to the pass manager. */
	LLVMAddAnalysisPasses :: proc(T: LLVMTargetMachineRef, PM: c.int) ---
}
