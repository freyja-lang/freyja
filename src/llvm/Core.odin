package llvm
/*===-- llvm-c/Core.h - Core Library C Interface ------------------*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This header declares the C interface to libLLVMCore.a, which implements    *|
|* the LLVM intermediate representation.                                      *|
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


// LLVM_C_CORE_H ::

LLVMOpcode :: c.int

LLVMTypeKind :: enum c.uint {
	VoidTypeKind, /**< type with no size */
	HalfTypeKind, /**< 16 bit floating point type */
	FloatTypeKind, /**< 32 bit floating point type */
	DoubleTypeKind, /**< 64 bit floating point type */
	X86_FP80TypeKind, /**< 80 bit floating point type (X87) */
	FP128TypeKind, /**< 128 bit floating point type (112-bit mantissa)*/
	PPC_FP128TypeKind, /**< 128 bit floating point type (two 64-bits) */
	LabelTypeKind, /**< Labels */
	IntegerTypeKind, /**< Arbitrary bit width integers */
	FunctionTypeKind, /**< Functions */
	StructTypeKind, /**< Structures */
	ArrayTypeKind, /**< Arrays */
	PointerTypeKind, /**< Pointers */
	VectorTypeKind, /**< Fixed width SIMD vector type */
	MetadataTypeKind, /**< Metadata */
	X86_MMXTypeKind, /**< X86 MMX */
	TokenTypeKind, /**< Tokens */
	ScalableVectorTypeKind, /**< Scalable SIMD vector type */
	BFloatTypeKind, /**< 16 bit brain floating point type */
	X86_AMXTypeKind, /**< X86 AMX */
	TargetExtTypeKind, /**< Target extension type */
}

LLVMLinkage :: enum c.uint {
	ExternalLinkage, /**< Externally visible function */
	AvailableExternallyLinkage,
	LinkOnceAnyLinkage, /**< Keep one copy of function when linking (inline)*/
	LinkOnceODRLinkage, /**< Same, but only replaced by something
                            equivalent. */
	LinkOnceODRAutoHideLinkage, /**< Obsolete */
	WeakAnyLinkage, /**< Keep one copy of function when linking (weak) */
	WeakODRLinkage, /**< Same, but only replaced by something
                            equivalent. */
	AppendingLinkage, /**< Special purpose, only applies to global arrays */
	InternalLinkage, /**< Rename collisions when linking (static
                               functions) */
	PrivateLinkage, /**< Like Internal, but omit from symbol table */
	DLLImportLinkage, /**< Obsolete */
	DLLExportLinkage, /**< Obsolete */
	ExternalWeakLinkage, /**< ExternalWeak linkage description */
	GhostLinkage, /**< Obsolete */
	CommonLinkage, /**< Tentative definitions */
	LinkerPrivateLinkage, /**< Like Private, but linker removes. */
	LinkerPrivateWeakLinkage, /**< Like LinkerPrivate, but is weak. */
}

LLVMVisibility :: enum c.uint {
	DefaultVisibility, /**< The GV is visible */
	HiddenVisibility, /**< The GV is hidden */
	ProtectedVisibility, /**< The GV is protected */
}

LLVMUnnamedAddr :: enum c.uint {
	NoUnnamedAddr, /**< Address of the GV is significant. */
	LocalUnnamedAddr, /**< Address of the GV is locally insignificant. */
	GlobalUnnamedAddr, /**< Address of the GV is globally insignificant. */
}

LLVMDLLStorageClass :: enum c.uint {
	efaultStorageClass,
	LLImportStorageClass, /**< Function to be imported from DLL. */
	LLExportStorageClass, /**< Function to be accessible from DLL. */
}

LLVMCallConv :: enum c.uint {
	CCallConv             = 0,
	FastCallConv          = 8,
	ColdCallConv          = 9,
	GHCCallConv           = 10,
	HiPECallConv          = 11,
	AnyRegCallConv        = 13,
	PreserveMostCallConv  = 14,
	PreserveAllCallConv   = 15,
	SwiftCallConv         = 16,
	CXXFASTTLSCallConv    = 17,
	X86StdcallCallConv    = 64,
	X86FastcallCallConv   = 65,
	ARMAPCSCallConv       = 66,
	ARMAAPCSCallConv      = 67,
	ARMAAPCSVFPCallConv   = 68,
	MSP430INTRCallConv    = 69,
	X86ThisCallCallConv   = 70,
	PTXKernelCallConv     = 71,
	PTXDeviceCallConv     = 72,
	SPIRFUNCCallConv      = 75,
	SPIRKERNELCallConv    = 76,
	IntelOCLBICallConv    = 77,
	X8664SysVCallConv     = 78,
	Win64CallConv         = 79,
	X86VectorCallCallConv = 80,
	HHVMCallConv          = 81,
	HHVMCCallConv         = 82,
	X86INTRCallConv       = 83,
	AVRINTRCallConv       = 84,
	AVRSIGNALCallConv     = 85,
	AVRBUILTINCallConv    = 86,
	AMDGPUVSCallConv      = 87,
	AMDGPUGSCallConv      = 88,
	AMDGPUPSCallConv      = 89,
	AMDGPUCSCallConv      = 90,
	AMDGPUKERNELCallConv  = 91,
	X86RegCallCallConv    = 92,
	AMDGPUHSCallConv      = 93,
	MSP430BUILTINCallConv = 94,
	AMDGPULSCallConv      = 95,
	AMDGPUESCallConv      = 96,
}

LLVMValueKind :: enum c.uint {
	ArgumentValueKind,
	BasicBlockValueKind,
	MemoryUseValueKind,
	MemoryDefValueKind,
	MemoryPhiValueKind,
	FunctionValueKind,
	GlobalAliasValueKind,
	GlobalIFuncValueKind,
	GlobalVariableValueKind,
	BlockAddressValueKind,
	ConstantExprValueKind,
	ConstantArrayValueKind,
	ConstantStructValueKind,
	ConstantVectorValueKind,
	UndefValueValueKind,
	ConstantAggregateZeroValueKind,
	ConstantDataArrayValueKind,
	ConstantDataVectorValueKind,
	ConstantIntValueKind,
	ConstantFPValueKind,
	ConstantPointerNullValueKind,
	ConstantTokenNoneValueKind,
	MetadataAsValueValueKind,
	InlineAsmValueKind,
	InstructionValueKind,
	PoisonValueValueKind,
	ConstantTargetNoneValueKind,
}

LLVMIntPredicate :: enum c.uint {
	EQ  = 32, /**< equal */
	NE  = 33, /**< not equal */
	UGT = 34, /**< unsigned greater than */
	UGE = 35, /**< unsigned greater or equal */
	ULT = 36, /**< unsigned less than */
	ULE = 37, /**< unsigned less or equal */
	SGT = 38, /**< signed greater than */
	SGE = 39, /**< signed greater or equal */
	SLT = 40, /**< signed less than */
	SLE = 41, /**< signed less or equal */
}

LLVMRealPredicate :: enum c.uint {
	PredicateFalse, /**< Always false (always folded) */
	OEQ, /**< True if ordered and equal */
	OGT, /**< True if ordered and greater than */
	OGE, /**< True if ordered and greater than or equal */
	OLT, /**< True if ordered and less than */
	OLE, /**< True if ordered and less than or equal */
	ONE, /**< True if ordered and operands are unequal */
	ORD, /**< True if ordered (no nans) */
	UNO, /**< True if unordered: isnan(X) | isnan(Y) */
	UEQ, /**< True if unordered or equal */
	UGT, /**< True if unordered or greater than */
	UGE, /**< True if unordered, greater than, or equal */
	ULT, /**< True if unordered or less than */
	ULE, /**< True if unordered, less than, or equal */
	UNE, /**< True if unordered or not equal */
	PredicateTrue, /**< Always true (always folded) */
}

LLVMLandingPadClauseTy :: enum c.uint {
	Catch, /**< A catch clause   */
	Filter, /**< A filter clause  */
}

LLVMThreadLocalMode :: enum c.uint {
	NotThreadLocal,
	GeneralDynamicTLSModel,
	LocalDynamicTLSModel,
	InitialExecTLSModel,
	LocalExecTLSModel,
}

LLVMAtomicOrdering :: enum c.uint {
	NotAtomic              = 0, /**< A load or store which is not atomic */
	Unordered              = 1, /**< Lowest level of atomicity, guarantees
                                     somewhat sane results, lock free. */
	Monotonic              = 2, /**< guarantees that if you take all the
                                     operations affecting a specific address,
                                     a consistent ordering exists */
	Acquire                = 4, /**< Acquire provides a barrier of the sort
                                   necessary to acquire a lock to access other
                                   memory with normal loads and stores. */
	Release                = 5, /**< Release is similar to Acquire, but with
                                   a barrier of the sort necessary to release
                                   a lock. */
	AcquireRelease         = 6, /**< provides both an Acquire and a
                                          Release barrier (for fences and
                                          operations which both read and write
                                           memory). */
	SequentiallyConsistent = 7, /**< provides Acquire semantics
                                                 for loads and Release
                                                 semantics for stores.
                                                 Additionally, it guarantees
                                                 that a total ordering exists
                                                 between all
                                                 SequentiallyConsistent
                                                 operations. */
}

LLVMAtomicRMWBinOp :: enum c.uint {
	Xchg, /**< Set the new value and return the one old */
	Add, /**< Add a value and return the old one */
	Sub, /**< Subtract a value and return the old one */
	And, /**< And a value and return the old one */
	Nand, /**< Not-And a value and return the old one */
	Or, /**< OR a value and return the old one */
	Xor, /**< Xor a value and return the old one */
	Max, /**< Sets the value if it's greater than the
                             original using a signed comparison and return
                             the old one */
	Min, /**< Sets the value if it's Smaller than the
                             original using a signed comparison and return
                             the old one */
	UMax, /**< Sets the value if it's greater than the
                             original using an unsigned comparison and return
                             the old one */
	UMin, /**< Sets the value if it's greater than the
                              original using an unsigned comparison and return
                              the old one */
	FAdd, /**< Add a floating point value and return the
                              old one */
	FSub, /**< Subtract a floating point value and return the
                            old one */
	FMax, /**< Sets the value if it's greater than the
                             original using an floating point comparison and
                             return the old one */
	FMin, /**< Sets the value if it's smaller than the
                             original using an floating point comparison and
                             return the old one */
}

LLVMDiagnosticSeverity :: enum c.uint {
	Error,
	Warning,
	Remark,
	Note,
}

LLVMInlineAsmDialect :: enum c.uint {
	ATT,
	Intel,
}

LLVMModuleFlagBehavior :: enum c.uint {
	/**
	* Emits an error if two values disagree, otherwise the resulting value is
	* that of the operands.
	*
	* @see Module::ModFlagBehavior::Error
	*/
	Error,

	/**
	* Emits a warning if two values disagree. The result value will be the
	* operand for the flag from the first module being linked.
	*
	* @see Module::ModFlagBehavior::Warning
	*/
	Warning,

	/**
	* Adds a requirement that another module flag be present and have a
	* specified value after linking is performed. The value must be a metadata
	* pair, where the first element of the pair is the ID of the module flag
	* to be restricted, and the second element of the pair is the value the
	* module flag should be restricted to. This behavior can be used to
	* restrict the allowable results (via triggering of an error) of linking
	* IDs with the **Override** behavior.
	*
	* @see Module::ModFlagBehavior::Require
	*/
	Require,

	/**
	* Uses the specified value, regardless of the behavior or value of the
	* other module. If both modules specify **Override**, but the values
	* differ, an error will be emitted.
	*
	* @see Module::ModFlagBehavior::Override
	*/
	Override,

	/**
	* Appends the two values, which are required to be metadata nodes.
	*
	* @see Module::ModFlagBehavior::Append
	*/
	Append,

	/**
	* Appends the two values, which are required to be metadata
	* nodes. However, duplicate entries in the second list are dropped
	* during the append operation.
	*
	* @see Module::ModFlagBehavior::AppendUnique
	*/
	AppendUnique,
}

/**
* Attribute index are either LLVMAttributeReturnIndex,
* LLVMAttributeFunctionIndex or a parameter number from 1 to N.
*/
LLVMAttributeReturnIndex :: 0

LLVMAttributeFunctionIndex :: -1

/**
* Tail call kind for LLVMSetTailCallKind and LLVMGetTailCallKind.
*
* Note that 'musttail' implies 'tail'.
*
* @see CallInst::TailCallKind
*/
LLVMTailCallKind :: enum c.uint {
	None,
	Tail,
	MustTail,
	NoTail,
}

LLVMAttributeIndex :: c.uint

LLVMFastMathAllowReassoc :: 1

LLVMFastMathNoNaNs :: 2

LLVMFastMathNoInfs :: 4

LLVMFastMathNoSignedZeros :: 8

LLVMFastMathAllowReciprocal :: 16

LLVMFastMathAllowContract :: 32

LLVMFastMathApproxFunc :: 64

LLVMFastMathNone :: 0

LLVMFastMathAll :: 127

/**
* Flags to indicate what fast-math-style optimizations are allowed
* on operations.
*
* See https://llvm.org/docs/LangRef.html#fast-math-flags
*/
LLVMFastMathFlags :: c.uint

/**
* @defgroup LLVMCCoreContext Contexts
*
* Contexts are execution states for the core LLVM IR system.
*
* Most types are tied to a context instance. Multiple contexts can
* exist simultaneously. A single context is not thread safe. However,
* different contexts can execute on different threads simultaneously.
*
* @{
*/
LLVMDiagnosticHandler :: proc "c" ()

LLVMYieldCallback :: proc "c" ()

@(default_calling_convention = "c", link_prefix = "")
foreign lib {
	/** Deallocate and destroy all ManagedStatic variables.
	@see llvm::llvm_shutdown
	@see ManagedStatic */
	LLVMShutdown :: proc() ---

	/**
	* Return the major, minor, and patch version of LLVM
	*
	* The version components are returned via the function's three output
	* parameters or skipped if a NULL pointer was supplied.
	*/
	LLVMGetVersion :: proc(Major: ^c.uint, Minor: ^c.uint, Patch: ^c.uint) ---

	/*===-- Error handling ----------------------------------------------------===*/
	LLVMCreateMessage :: proc(Message: cstring) -> cstring ---
	LLVMDisposeMessage :: proc(Message: cstring) ---

	/**
	* Create a new context.
	*
	* Every call to this function should be paired with a call to
	* LLVMContextDispose() or the context will leak memory.
	*/
	LLVMContextCreate :: proc() -> LLVMContextRef ---

	/**
	* Obtain the global context instance.
	*/
	LLVMGetGlobalContext :: proc() -> LLVMContextRef ---

	/**
	* Set the diagnostic handler for this context.
	*/
	LLVMContextSetDiagnosticHandler :: proc(C: c.int, Handler: LLVMDiagnosticHandler, DiagnosticContext: rawptr) ---

	/**
	* Get the diagnostic handler of this context.
	*/
	LLVMContextGetDiagnosticHandler :: proc(C: c.int) -> LLVMDiagnosticHandler ---

	/**
	* Get the diagnostic context of this context.
	*/
	LLVMContextGetDiagnosticContext :: proc(C: c.int) -> rawptr ---

	/**
	* Set the yield callback function for this context.
	*
	* @see LLVMContext::setYieldCallback()
	*/
	LLVMContextSetYieldCallback :: proc(C: c.int, Callback: LLVMYieldCallback, OpaqueHandle: rawptr) ---

	/**
	* Retrieve whether the given context is set to discard all value names.
	*
	* @see LLVMContext::shouldDiscardValueNames()
	*/
	LLVMContextShouldDiscardValueNames :: proc(C: c.int) -> c.int ---

	/**
	* Set whether the given context discards all value names.
	*
	* If true, only the names of GlobalValue objects will be available in the IR.
	* This can be used to save memory and runtime, especially in release mode.
	*
	* @see LLVMContext::setDiscardValueNames()
	*/
	LLVMContextSetDiscardValueNames :: proc(C: c.int, Discard: c.int) ---

	/**
	* Destroy a context instance.
	*
	* This should be called for every call to LLVMContextCreate() or memory
	* will be leaked.
	*/
	LLVMContextDispose :: proc(C: LLVMContextRef) ---

	/**
	* Return a string representation of the DiagnosticInfo. Use
	* LLVMDisposeMessage to free the string.
	*
	* @see DiagnosticInfo::print()
	*/
	LLVMGetDiagInfoDescription :: proc(DI: c.int) -> cstring ---

	/**
	* Return an enum LLVMDiagnosticSeverity.
	*
	* @see DiagnosticInfo::getSeverity()
	*/
	LLVMGetDiagInfoSeverity :: proc(DI: c.int) -> LLVMDiagnosticSeverity ---
	LLVMGetMDKindIDInContext :: proc(C: c.int, Name: cstring, SLen: c.uint) -> c.uint ---
	LLVMGetMDKindID :: proc(Name: cstring, SLen: c.uint) -> c.uint ---

	/**
	* Return an unique id given the name of a enum attribute,
	* or 0 if no attribute by that name exists.
	*
	* See http://llvm.org/docs/LangRef.html#parameter-attributes
	* and http://llvm.org/docs/LangRef.html#function-attributes
	* for the list of available attributes.
	*
	* NB: Attribute names and/or id are subject to change without
	* going through the C API deprecation cycle.
	*/
	LLVMGetEnumAttributeKindForName :: proc(Name: cstring, SLen: c.int) -> c.uint ---
	LLVMGetLastEnumAttributeKind :: proc() -> c.uint ---

	/**
	* Create an enum attribute.
	*/
	LLVMCreateEnumAttribute :: proc(C: LLVMContextRef, KindID: c.uint, Val: c.uint64_t) -> LLVMAttributeRef ---

	/**
	* Get the unique id corresponding to the enum attribute
	* passed as argument.
	*/
	LLVMGetEnumAttributeKind :: proc(A: c.int) -> c.uint ---

	/**
	* Get the enum attribute's value. 0 is returned if none exists.
	*/
	LLVMGetEnumAttributeValue :: proc(A: c.int) -> c.int ---

	/**
	* Create a type attribute
	*/
	LLVMCreateTypeAttribute :: proc(C: c.int, KindID: c.uint, type_ref: c.int) -> c.int ---

	/**
	* Get the type attribute's value.
	*/
	LLVMGetTypeAttributeValue :: proc(A: c.int) -> c.int ---

	/**
	* Create a string attribute.
	*/
	LLVMCreateStringAttribute :: proc(C: c.int, K: cstring, KLength: c.uint, V: cstring, VLength: c.uint) -> c.int ---

	/**
	* Get the string attribute's kind.
	*/
	LLVMGetStringAttributeKind :: proc(A: c.int, Length: ^c.uint) -> cstring ---

	/**
	* Get the string attribute's value.
	*/
	LLVMGetStringAttributeValue :: proc(A: c.int, Length: ^c.uint) -> cstring ---

	/**
	* Check for the different types of attributes.
	*/
	LLVMIsEnumAttribute :: proc(A: c.int) -> c.int ---
	LLVMIsStringAttribute :: proc(A: c.int) -> c.int ---
	LLVMIsTypeAttribute :: proc(A: c.int) -> c.int ---

	/**
	* Obtain a Type from a context by its registered name.
	*/
	LLVMGetTypeByName2 :: proc(C: c.int, Name: cstring) -> c.int ---

	/**
	* Create a new, empty module in the global context.
	*
	* This is equivalent to calling LLVMModuleCreateWithNameInContext with
	* LLVMGetGlobalContext() as the context parameter.
	*
	* Every invocation should be paired with LLVMDisposeModule() or memory
	* will be leaked.
	*/
	LLVMModuleCreateWithName :: proc(ModuleID: cstring) -> c.int ---

	/**
	* Create a new, empty module in a specific context.
	*
	* Every invocation should be paired with LLVMDisposeModule() or memory
	* will be leaked.
	*/
	LLVMModuleCreateWithNameInContext :: proc(ModuleID: cstring, C: LLVMContextRef) -> LLVMModuleRef ---

	/**
	* Return an exact copy of the specified module.
	*/
	LLVMCloneModule :: proc(M: LLVMModuleRef) -> LLVMModuleRef ---

	/**
	* Destroy a module instance.
	*
	* This must be called for every created module or memory will be
	* leaked.
	*/
	LLVMDisposeModule :: proc(M: LLVMModuleRef) ---

	/**
	* Obtain the identifier of a module.
	*
	* @param M Module to obtain identifier of
	* @param Len Out parameter which holds the length of the returned string.
	* @return The identifier of M.
	* @see Module::getModuleIdentifier()
	*/
	LLVMGetModuleIdentifier :: proc(M: c.int, Len: ^c.int) -> cstring ---

	/**
	* Set the identifier of a module to a string Ident with length Len.
	*
	* @param M The module to set identifier
	* @param Ident The string to set M's identifier to
	* @param Len Length of Ident
	* @see Module::setModuleIdentifier()
	*/
	LLVMSetModuleIdentifier :: proc(M: c.int, Ident: cstring, Len: c.int) ---

	/**
	* Obtain the module's original source file name.
	*
	* @param M Module to obtain the name of
	* @param Len Out parameter which holds the length of the returned string
	* @return The original source file name of M
	* @see Module::getSourceFileName()
	*/
	LLVMGetSourceFileName :: proc(M: c.int, Len: ^c.int) -> cstring ---

	/**
	* Set the original source file name of a module to a string Name with length
	* Len.
	*
	* @param M The module to set the source file name of
	* @param Name The string to set M's source file name to
	* @param Len Length of Name
	* @see Module::setSourceFileName()
	*/
	LLVMSetSourceFileName :: proc(M: c.int, Name: cstring, Len: c.int) ---

	/**
	* Obtain the data layout for a module.
	*
	* @see Module::getDataLayoutStr()
	*
	* LLVMGetDataLayout is DEPRECATED, as the name is not only incorrect,
	* but match the name of another method on the module. Prefer the use
	* of LLVMGetDataLayoutStr, which is not ambiguous.
	*/
	LLVMGetDataLayoutStr :: proc(M: c.int) -> cstring ---
	LLVMGetDataLayout :: proc(M: c.int) -> cstring ---

	/**
	* Set the data layout for a module.
	*
	* @see Module::setDataLayout()
	*/
	LLVMSetDataLayout :: proc(M: c.int, DataLayoutStr: cstring) ---

	/**
	* Obtain the target triple for a module.
	*
	* @see Module::getTargetTriple()
	*/
	LLVMGetTarget :: proc(M: c.int) -> cstring ---

	/**
	* Set the target triple for a module.
	*
	* @see Module::setTargetTriple()
	*/
	LLVMSetTarget :: proc(M: c.int, Triple: cstring) ---

	/**
	* Returns the module flags as an array of flag-key-value triples.  The caller
	* is responsible for freeing this array by calling
	* \c LLVMDisposeModuleFlagsMetadata.
	*
	* @see Module::getModuleFlagsMetadata()
	*/
	LLVMCopyModuleFlagsMetadata :: proc(M: c.int, Len: ^c.int) -> ^c.int ---

	/**
	* Destroys module flags metadata entries.
	*/
	LLVMDisposeModuleFlagsMetadata :: proc(Entries: ^c.int) ---

	/**
	* Returns the flag behavior for a module flag entry at a specific index.
	*
	* @see Module::ModuleFlagEntry::Behavior
	*/
	LLVMModuleFlagEntriesGetFlagBehavior :: proc(Entries: ^c.int, Index: c.uint) -> LLVMModuleFlagBehavior ---

	/**
	* Returns the key for a module flag entry at a specific index.
	*
	* @see Module::ModuleFlagEntry::Key
	*/
	LLVMModuleFlagEntriesGetKey :: proc(Entries: ^c.int, Index: c.uint, Len: ^c.int) -> cstring ---

	/**
	* Returns the metadata for a module flag entry at a specific index.
	*
	* @see Module::ModuleFlagEntry::Val
	*/
	LLVMModuleFlagEntriesGetMetadata :: proc(Entries: ^c.int, Index: c.uint) -> c.int ---

	/**
	* Add a module-level flag to the module-level flags metadata if it doesn't
	* already exist.
	*
	* @see Module::getModuleFlag()
	*/
	LLVMGetModuleFlag :: proc(M: c.int, Key: cstring, KeyLen: c.int) -> c.int ---

	/**
	* Add a module-level flag to the module-level flags metadata if it doesn't
	* already exist.
	*
	* @see Module::addModuleFlag()
	*/
	LLVMAddModuleFlag :: proc(M: c.int, Behavior: LLVMModuleFlagBehavior, Key: cstring, KeyLen: c.int, Val: c.int) ---

	/**
	* Dump a representation of a module to stderr.
	*
	* @see Module::dump()
	*/
	LLVMDumpModule :: proc(M: c.int) ---

	/**
	* Print a representation of a module to a file. The ErrorMessage needs to be
	* disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
	*
	* @see Module::print()
	*/
	LLVMPrintModuleToFile :: proc(M: c.int, Filename: cstring, ErrorMessage: [^]cstring) -> c.int ---

	/**
	* Return a string representation of the module. Use
	* LLVMDisposeMessage to free the string.
	*
	* @see Module::print()
	*/
	LLVMPrintModuleToString :: proc(M: LLVMModuleRef) -> cstring ---

	/**
	* Get inline assembly for a module.
	*
	* @see Module::getModuleInlineAsm()
	*/
	LLVMGetModuleInlineAsm :: proc(M: c.int, Len: ^c.int) -> cstring ---

	/**
	* Set inline assembly for a module.
	*
	* @see Module::setModuleInlineAsm()
	*/
	LLVMSetModuleInlineAsm2 :: proc(M: c.int, Asm: cstring, Len: c.int) ---

	/**
	* Append inline assembly to a module.
	*
	* @see Module::appendModuleInlineAsm()
	*/
	LLVMAppendModuleInlineAsm :: proc(M: c.int, Asm: cstring, Len: c.int) ---

	/**
	* Create the specified uniqued inline asm string.
	*
	* @see InlineAsm::get()
	*/
	LLVMGetInlineAsm :: proc(Ty: c.int, AsmString: cstring, AsmStringSize: c.int, Constraints: cstring, ConstraintsSize: c.int, HasSideEffects: c.int, IsAlignStack: c.int, Dialect: LLVMInlineAsmDialect, CanThrow: c.int) -> c.int ---

	/**
	* Get the template string used for an inline assembly snippet
	*
	*/
	LLVMGetInlineAsmAsmString :: proc(InlineAsmVal: c.int, Len: ^c.int) -> cstring ---

	/**
	* Get the raw constraint string for an inline assembly snippet
	*
	*/
	LLVMGetInlineAsmConstraintString :: proc(InlineAsmVal: c.int, Len: ^c.int) -> cstring ---

	/**
	* Get the dialect used by the inline asm snippet
	*
	*/
	LLVMGetInlineAsmDialect :: proc(InlineAsmVal: c.int) -> LLVMInlineAsmDialect ---

	/**
	* Get the function type of the inline assembly snippet. The same type that
	* was passed into LLVMGetInlineAsm originally
	*
	* @see LLVMGetInlineAsm
	*
	*/
	LLVMGetInlineAsmFunctionType :: proc(InlineAsmVal: c.int) -> c.int ---

	/**
	* Get if the inline asm snippet has side effects
	*
	*/
	LLVMGetInlineAsmHasSideEffects :: proc(InlineAsmVal: c.int) -> c.int ---

	/**
	* Get if the inline asm snippet needs an aligned stack
	*
	*/
	LLVMGetInlineAsmNeedsAlignedStack :: proc(InlineAsmVal: c.int) -> c.int ---

	/**
	* Get if the inline asm snippet may unwind the stack
	*
	*/
	LLVMGetInlineAsmCanUnwind :: proc(InlineAsmVal: c.int) -> c.int ---

	/**
	* Obtain the context to which this module is associated.
	*
	* @see Module::getContext()
	*/
	LLVMGetModuleContext :: proc(M: c.int) -> c.int ---

	/** Deprecated: Use LLVMGetTypeByName2 instead. */
	LLVMGetTypeByName :: proc(M: c.int, Name: cstring) -> c.int ---

	/**
	* Obtain an iterator to the first NamedMDNode in a Module.
	*
	* @see llvm::Module::named_metadata_begin()
	*/
	LLVMGetFirstNamedMetadata :: proc(M: c.int) -> c.int ---

	/**
	* Obtain an iterator to the last NamedMDNode in a Module.
	*
	* @see llvm::Module::named_metadata_end()
	*/
	LLVMGetLastNamedMetadata :: proc(M: c.int) -> c.int ---

	/**
	* Advance a NamedMDNode iterator to the next NamedMDNode.
	*
	* Returns NULL if the iterator was already at the end and there are no more
	* named metadata nodes.
	*/
	LLVMGetNextNamedMetadata :: proc(NamedMDNode: c.int) -> c.int ---

	/**
	* Decrement a NamedMDNode iterator to the previous NamedMDNode.
	*
	* Returns NULL if the iterator was already at the beginning and there are
	* no previous named metadata nodes.
	*/
	LLVMGetPreviousNamedMetadata :: proc(NamedMDNode: c.int) -> c.int ---

	/**
	* Retrieve a NamedMDNode with the given name, returning NULL if no such
	* node exists.
	*
	* @see llvm::Module::getNamedMetadata()
	*/
	LLVMGetNamedMetadata :: proc(M: c.int, Name: cstring, NameLen: c.int) -> c.int ---

	/**
	* Retrieve a NamedMDNode with the given name, creating a new node if no such
	* node exists.
	*
	* @see llvm::Module::getOrInsertNamedMetadata()
	*/
	LLVMGetOrInsertNamedMetadata :: proc(M: c.int, Name: cstring, NameLen: c.int) -> c.int ---

	/**
	* Retrieve the name of a NamedMDNode.
	*
	* @see llvm::NamedMDNode::getName()
	*/
	LLVMGetNamedMetadataName :: proc(NamedMD: c.int, NameLen: ^c.int) -> cstring ---

	/**
	* Obtain the number of operands for named metadata in a module.
	*
	* @see llvm::Module::getNamedMetadata()
	*/
	LLVMGetNamedMetadataNumOperands :: proc(M: c.int, Name: cstring) -> c.uint ---

	/**
	* Obtain the named metadata operands for a module.
	*
	* The passed LLVMValueRef pointer should refer to an array of
	* LLVMValueRef at least LLVMGetNamedMetadataNumOperands long. This
	* array will be populated with the LLVMValueRef instances. Each
	* instance corresponds to a llvm::MDNode.
	*
	* @see llvm::Module::getNamedMetadata()
	* @see llvm::MDNode::getOperand()
	*/
	LLVMGetNamedMetadataOperands :: proc(M: c.int, Name: cstring, Dest: ^c.int) ---

	/**
	* Add an operand to named metadata.
	*
	* @see llvm::Module::getNamedMetadata()
	* @see llvm::MDNode::addOperand()
	*/
	LLVMAddNamedMetadataOperand :: proc(M: c.int, Name: cstring, Val: c.int) ---

	/**
	* Return the directory of the debug location for this value, which must be
	* an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
	*
	* @see llvm::Instruction::getDebugLoc()
	* @see llvm::GlobalVariable::getDebugInfo()
	* @see llvm::Function::getSubprogram()
	*/
	LLVMGetDebugLocDirectory :: proc(Val: c.int, Length: ^c.uint) -> cstring ---

	/**
	* Return the filename of the debug location for this value, which must be
	* an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
	*
	* @see llvm::Instruction::getDebugLoc()
	* @see llvm::GlobalVariable::getDebugInfo()
	* @see llvm::Function::getSubprogram()
	*/
	LLVMGetDebugLocFilename :: proc(Val: c.int, Length: ^c.uint) -> cstring ---

	/**
	* Return the line number of the debug location for this value, which must be
	* an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
	*
	* @see llvm::Instruction::getDebugLoc()
	* @see llvm::GlobalVariable::getDebugInfo()
	* @see llvm::Function::getSubprogram()
	*/
	LLVMGetDebugLocLine :: proc(Val: c.int) -> c.uint ---

	/**
	* Return the column number of the debug location for this value, which must be
	* an llvm::Instruction.
	*
	* @see llvm::Instruction::getDebugLoc()
	*/
	LLVMGetDebugLocColumn :: proc(Val: c.int) -> c.uint ---

	/**
	* Add a function to a module under a specified name.
	*
	* @see llvm::Function::Create()
	*/
	LLVMAddFunction :: proc(M: LLVMModuleRef, Name: cstring, FunctionTy: LLVMTypeRef) -> LLVMValueRef ---

	/**
	* Obtain a Function value from a Module by its name.
	*
	* The returned value corresponds to a llvm::Function value.
	*
	* @see llvm::Module::getFunction()
	*/
	LLVMGetNamedFunction :: proc(M: LLVMModuleRef, Name: cstring) -> LLVMValueRef ---

	/**
	* Obtain an iterator to the first Function in a Module.
	*
	* @see llvm::Module::begin()
	*/
	LLVMGetFirstFunction :: proc(M: c.int) -> c.int ---

	/**
	* Obtain an iterator to the last Function in a Module.
	*
	* @see llvm::Module::end()
	*/
	LLVMGetLastFunction :: proc(M: c.int) -> c.int ---

	/**
	* Advance a Function iterator to the next Function.
	*
	* Returns NULL if the iterator was already at the end and there are no more
	* functions.
	*/
	LLVMGetNextFunction :: proc(Fn: c.int) -> c.int ---

	/**
	* Decrement a Function iterator to the previous Function.
	*
	* Returns NULL if the iterator was already at the beginning and there are
	* no previous functions.
	*/
	LLVMGetPreviousFunction :: proc(Fn: c.int) -> c.int ---

	/** Deprecated: Use LLVMSetModuleInlineAsm2 instead. */
	LLVMSetModuleInlineAsm :: proc(M: c.int, Asm: cstring) ---

	/**
	* Obtain the enumerated type of a Type instance.
	*
	* @see llvm::Type:getTypeID()
	*/
	LLVMGetTypeKind :: proc(Ty: LLVMTypeRef) -> LLVMTypeKind ---

	/**
	* Whether the type has a known size.
	*
	* Things that don't have a size are abstract types, labels, and void.a
	*
	* @see llvm::Type::isSized()
	*/
	LLVMTypeIsSized :: proc(Ty: c.int) -> c.int ---

	/**
	* Obtain the context to which this type instance is associated.
	*
	* @see llvm::Type::getContext()
	*/
	LLVMGetTypeContext :: proc(Ty: c.int) -> c.int ---

	/**
	* Dump a representation of a type to stderr.
	*
	* @see llvm::Type::dump()
	*/
	LLVMDumpType :: proc(Val: c.int) ---

	/**
	* Return a string representation of the type. Use
	* LLVMDisposeMessage to free the string.
	*
	* @see llvm::Type::print()
	*/
	LLVMPrintTypeToString :: proc(Val: c.int) -> cstring ---

	/**
	* Obtain an integer type from a context with specified bit width.
	*/
	LLVMInt1TypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---
	LLVMInt8TypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---
	LLVMInt16TypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---
	LLVMInt32TypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---
	LLVMInt64TypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---
	LLVMInt128TypeInContext :: proc(C: c.int) -> c.int ---
	LLVMIntTypeInContext :: proc(C: c.int, NumBits: c.uint) -> c.int ---

	/**
	* Obtain an integer type from the global context with a specified bit
	* width.
	*/
	LLVMInt1Type :: proc() -> c.int ---
	LLVMInt8Type :: proc() -> c.int ---
	LLVMInt16Type :: proc() -> c.int ---
	LLVMInt32Type :: proc() -> c.int ---
	LLVMInt64Type :: proc() -> c.int ---
	LLVMInt128Type :: proc() -> c.int ---
	LLVMIntType :: proc(NumBits: c.uint) -> c.int ---
	LLVMGetIntTypeWidth :: proc(IntegerTy: c.int) -> c.uint ---

	/**
	* Obtain a 16-bit floating point type from a context.
	*/
	LLVMHalfTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Obtain a 16-bit brain floating point type from a context.
	*/
	LLVMBFloatTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Obtain a 32-bit floating point type from a context.
	*/
	LLVMFloatTypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---

	/**
	* Obtain a 64-bit floating point type from a context.
	*/
	LLVMDoubleTypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---

	/**
	* Obtain a 80-bit floating point type (X87) from a context.
	*/
	LLVMX86FP80TypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Obtain a 128-bit floating point type (112-bit mantissa) from a
	* context.
	*/
	LLVMFP128TypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Obtain a 128-bit floating point type (two 64-bits) from a context.
	*/
	LLVMPPCFP128TypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Obtain a floating point type from the global context.
	*
	* These map to the functions in this group of the same name.
	*/
	LLVMHalfType :: proc() -> c.int ---
	LLVMBFloatType :: proc() -> c.int ---
	LLVMFloatType :: proc() -> c.int ---
	LLVMDoubleType :: proc() -> c.int ---
	LLVMX86FP80Type :: proc() -> c.int ---
	LLVMFP128Type :: proc() -> c.int ---
	LLVMPPCFP128Type :: proc() -> c.int ---

	/**
	* Obtain a function type consisting of a specified signature.
	*
	* The function is defined as a tuple of a return Type, a list of
	* parameter types, and whether the function is variadic.
	*/
	LLVMFunctionType :: proc(ReturnType: LLVMTypeRef, ParamTypes: ^LLVMTypeRef, ParamCount: c.uint, IsVarArg: LLVMBool) -> LLVMTypeRef ---

	/**
	* Returns whether a function type is variadic.
	*/
	LLVMIsFunctionVarArg :: proc(FunctionTy: c.int) -> c.int ---

	/**
	* Obtain the Type this function Type returns.
	*/
	LLVMGetReturnType :: proc(FunctionTy: LLVMTypeRef) -> LLVMTypeRef ---

	/**
	* Obtain the number of parameters this function accepts.
	*/
	LLVMCountParamTypes :: proc(FunctionTy: c.int) -> c.uint ---

	/**
	* Obtain the types of a function's parameters.
	*
	* The Dest parameter should point to a pre-allocated array of
	* LLVMTypeRef at least LLVMCountParamTypes() large. On return, the
	* first LLVMCountParamTypes() entries in the array will be populated
	* with LLVMTypeRef instances.
	*
	* @param FunctionTy The function type to operate on.
	* @param Dest Memory address of an array to be filled with result.
	*/
	LLVMGetParamTypes :: proc(FunctionTy: c.int, Dest: ^c.int) ---

	/**
	* Create a new structure type in a context.
	*
	* A structure is specified by a list of inner elements/types and
	* whether these can be packed together.
	*
	* @see llvm::StructType::create()
	*/
	LLVMStructTypeInContext :: proc(C: LLVMContextRef, ElementTypes: ^LLVMTypeRef, ElementCount: c.uint, Packed: c.int) -> LLVMTypeRef ---

	/**
	* Create a new structure type in the global context.
	*
	* @see llvm::StructType::create()
	*/
	LLVMStructType :: proc(ElementTypes: ^c.int, ElementCount: c.uint, Packed: c.int) -> c.int ---

	/**
	* Create an empty structure in a context having a specified name.
	*
	* @see llvm::StructType::create()
	*/
	LLVMStructCreateNamed :: proc(C: c.int, Name: cstring) -> c.int ---

	/**
	* Obtain the name of a structure.
	*
	* @see llvm::StructType::getName()
	*/
	LLVMGetStructName :: proc(Ty: c.int) -> cstring ---

	/**
	* Set the contents of a structure type.
	*
	* @see llvm::StructType::setBody()
	*/
	LLVMStructSetBody :: proc(StructTy: c.int, ElementTypes: ^c.int, ElementCount: c.uint, Packed: c.int) ---

	/**
	* Get the number of elements defined inside the structure.
	*
	* @see llvm::StructType::getNumElements()
	*/
	LLVMCountStructElementTypes :: proc(StructTy: c.int) -> c.uint ---

	/**
	* Get the elements within a structure.
	*
	* The function is passed the address of a pre-allocated array of
	* LLVMTypeRef at least LLVMCountStructElementTypes() long. After
	* invocation, this array will be populated with the structure's
	* elements. The objects in the destination array will have a lifetime
	* of the structure type itself, which is the lifetime of the context it
	* is contained in.
	*/
	LLVMGetStructElementTypes :: proc(StructTy: c.int, Dest: ^c.int) ---

	/**
	* Get the type of the element at a given index in the structure.
	*
	* @see llvm::StructType::getTypeAtIndex()
	*/
	LLVMStructGetTypeAtIndex :: proc(StructTy: c.int, i: c.uint) -> c.int ---

	/**
	* Determine whether a structure is packed.
	*
	* @see llvm::StructType::isPacked()
	*/
	LLVMIsPackedStruct :: proc(StructTy: c.int) -> c.int ---

	/**
	* Determine whether a structure is opaque.
	*
	* @see llvm::StructType::isOpaque()
	*/
	LLVMIsOpaqueStruct :: proc(StructTy: c.int) -> c.int ---

	/**
	* Determine whether a structure is literal.
	*
	* @see llvm::StructType::isLiteral()
	*/
	LLVMIsLiteralStruct :: proc(StructTy: c.int) -> c.int ---

	/**
	* Obtain the element type of an array or vector type.
	*
	* @see llvm::SequentialType::getElementType()
	*/
	LLVMGetElementType :: proc(Ty: c.int) -> c.int ---

	/**
	* Returns type's subtypes
	*
	* @see llvm::Type::subtypes()
	*/
	LLVMGetSubtypes :: proc(Tp: c.int, Arr: ^c.int) ---

	/**
	*  Return the number of types in the derived type.
	*
	* @see llvm::Type::getNumContainedTypes()
	*/
	LLVMGetNumContainedTypes :: proc(Tp: c.int) -> c.uint ---

	/**
	* Create a fixed size array type that refers to a specific type.
	*
	* The created type will exist in the context that its element type
	* exists in.
	*
	* @deprecated LLVMArrayType is deprecated in favor of the API accurate
	* LLVMArrayType2
	* @see llvm::ArrayType::get()
	*/
	LLVMArrayType :: proc(ElementType: LLVMTypeRef, ElementCount: c.uint) -> LLVMTypeRef ---

	/**
	* Create a fixed size array type that refers to a specific type.
	*
	* The created type will exist in the context that its element type
	* exists in.
	*
	* @see llvm::ArrayType::get()
	*/
	LLVMArrayType2 :: proc(ElementType: c.int, ElementCount: c.int) -> c.int ---

	/**
	* Obtain the length of an array type.
	*
	* This only works on types that represent arrays.
	*
	* @deprecated LLVMGetArrayLength is deprecated in favor of the API accurate
	* LLVMGetArrayLength2
	* @see llvm::ArrayType::getNumElements()
	*/
	LLVMGetArrayLength :: proc(ArrayTy: c.int) -> c.uint ---

	/**
	* Obtain the length of an array type.
	*
	* This only works on types that represent arrays.
	*
	* @see llvm::ArrayType::getNumElements()
	*/
	LLVMGetArrayLength2 :: proc(ArrayTy: c.int) -> c.int ---

	/**
	* Create a pointer type that points to a defined type.
	*
	* The created type will exist in the context that its pointee type
	* exists in.
	*
	* @see llvm::PointerType::get()
	*/
	LLVMPointerType :: proc(ElementType: LLVMTypeRef, AddressSpace: c.uint) -> LLVMTypeRef ---

	/**
	* Determine whether a pointer is opaque.
	*
	* True if this is an instance of an opaque PointerType.
	*
	* @see llvm::Type::isOpaquePointerTy()
	*/
	LLVMPointerTypeIsOpaque :: proc(Ty: c.int) -> c.int ---

	/**
	* Create an opaque pointer type in a context.
	*
	* @see llvm::PointerType::get()
	*/
	LLVMPointerTypeInContext :: proc(C: c.int, AddressSpace: c.uint) -> c.int ---

	/**
	* Obtain the address space of a pointer type.
	*
	* This only works on types that represent pointers.
	*
	* @see llvm::PointerType::getAddressSpace()
	*/
	LLVMGetPointerAddressSpace :: proc(PointerTy: c.int) -> c.uint ---

	/**
	* Create a vector type that contains a defined type and has a specific
	* number of elements.
	*
	* The created type will exist in the context thats its element type
	* exists in.
	*
	* @see llvm::VectorType::get()
	*/
	LLVMVectorType :: proc(ElementType: c.int, ElementCount: c.uint) -> c.int ---

	/**
	* Create a vector type that contains a defined type and has a scalable
	* number of elements.
	*
	* The created type will exist in the context thats its element type
	* exists in.
	*
	* @see llvm::ScalableVectorType::get()
	*/
	LLVMScalableVectorType :: proc(ElementType: c.int, ElementCount: c.uint) -> c.int ---

	/**
	* Obtain the (possibly scalable) number of elements in a vector type.
	*
	* This only works on types that represent vectors (fixed or scalable).
	*
	* @see llvm::VectorType::getNumElements()
	*/
	LLVMGetVectorSize :: proc(VectorTy: c.int) -> c.uint ---

	/**
	* Create a void type in a context.
	*/
	LLVMVoidTypeInContext :: proc(C: LLVMContextRef) -> LLVMTypeRef ---

	/**
	* Create a label type in a context.
	*/
	LLVMLabelTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Create a X86 MMX type in a context.
	*/
	LLVMX86MMXTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Create a X86 AMX type in a context.
	*/
	LLVMX86AMXTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Create a token type in a context.
	*/
	LLVMTokenTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* Create a metadata type in a context.
	*/
	LLVMMetadataTypeInContext :: proc(C: c.int) -> c.int ---

	/**
	* These are similar to the above functions except they operate on the
	* global context.
	*/
	LLVMVoidType :: proc() -> c.int ---
	LLVMLabelType :: proc() -> c.int ---
	LLVMX86MMXType :: proc() -> c.int ---
	LLVMX86AMXType :: proc() -> c.int ---

	/**
	* Create a target extension type in LLVM context.
	*/
	LLVMTargetExtTypeInContext :: proc(C: c.int, Name: cstring, TypeParams: ^c.int, TypeParamCount: c.uint, IntParams: ^c.uint, IntParamCount: c.uint) -> c.int ---

	/**
	* Obtain the type of a value.
	*
	* @see llvm::Value::getType()
	*/
	LLVMTypeOf :: proc(Val: c.int) -> c.int ---

	/**
	* Obtain the enumerated type of a Value instance.
	*
	* @see llvm::Value::getValueID()
	*/
	LLVMGetValueKind :: proc(Val: LLVMValueRef) -> LLVMValueKind ---

	/**
	* Obtain the string name of a value.
	*
	* @see llvm::Value::getName()
	*/
	LLVMGetValueName2 :: proc(Val: c.int, Length: ^c.int) -> cstring ---

	/**
	* Set the string name of a value.
	*
	* @see llvm::Value::setName()
	*/
	LLVMSetValueName2 :: proc(Val: c.int, Name: cstring, NameLen: c.int) ---

	/**
	* Dump a representation of a value to stderr.
	*
	* @see llvm::Value::dump()
	*/
	LLVMDumpValue :: proc(Val: c.int) ---

	/**
	* Return a string representation of the value. Use
	* LLVMDisposeMessage to free the string.
	*
	* @see llvm::Value::print()
	*/
	LLVMPrintValueToString :: proc(Val: c.int) -> cstring ---

	/**
	* Replace all uses of a value with another one.
	*
	* @see llvm::Value::replaceAllUsesWith()
	*/
	LLVMReplaceAllUsesWith :: proc(OldVal: c.int, NewVal: c.int) ---

	/**
	* Determine whether the specified value instance is constant.
	*/
	LLVMIsConstant :: proc(Val: c.int) -> c.int ---

	/**
	* Determine whether a value instance is undefined.
	*/
	LLVMIsUndef :: proc(Val: c.int) -> c.int ---

	/**
	* Determine whether a value instance is poisonous.
	*/
	LLVMIsPoison :: proc(Val: c.int) -> c.int ---
	LLVMIsACallInst :: proc(Val: c.int) -> c.int ---
	LLVMIsABasicBlock :: proc(Val: c.int) -> c.int ---
	LLVMIsAInlineAsm :: proc(Val: c.int) -> c.int ---
	LLVMIsAUser :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstant :: proc(Val: c.int) -> c.int ---
	LLVMIsABlockAddress :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantAggregateZero :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantArray :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantDataSequential :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantDataArray :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantDataVector :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantExpr :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantFP :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantInt :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantPointerNull :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantStruct :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantTokenNone :: proc(Val: c.int) -> c.int ---
	LLVMIsAConstantVector :: proc(Val: c.int) -> c.int ---
	LLVMIsAGlobalValue :: proc(Val: c.int) -> c.int ---
	LLVMIsAGlobalAlias :: proc(Val: c.int) -> c.int ---
	LLVMIsAGlobalObject :: proc(Val: c.int) -> c.int ---
	LLVMIsAFunction :: proc(Val: c.int) -> c.int ---
	LLVMIsAGlobalVariable :: proc(Val: c.int) -> c.int ---
	LLVMIsAGlobalIFunc :: proc(Val: c.int) -> c.int ---
	LLVMIsAUndefValue :: proc(Val: c.int) -> c.int ---
	LLVMIsAPoisonValue :: proc(Val: c.int) -> c.int ---
	LLVMIsAInstruction :: proc(Val: c.int) -> c.int ---
	LLVMIsAUnaryOperator :: proc(Val: c.int) -> c.int ---
	LLVMIsABinaryOperator :: proc(Val: c.int) -> c.int ---
	LLVMIsAArgument :: proc(Val: c.int) -> c.int ---
	LLVMIsAIntrinsicInst :: proc(Val: c.int) -> c.int ---
	LLVMIsADbgInfoIntrinsic :: proc(Val: c.int) -> c.int ---
	LLVMIsADbgVariableIntrinsic :: proc(Val: c.int) -> c.int ---
	LLVMIsADbgDeclareInst :: proc(Val: c.int) -> c.int ---
	LLVMIsADbgLabelInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAMemIntrinsic :: proc(Val: c.int) -> c.int ---
	LLVMIsAMemCpyInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAMemMoveInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAMemSetInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACmpInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFCmpInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAICmpInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAExtractElementInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAGetElementPtrInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAInsertElementInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAInsertValueInst :: proc(Val: c.int) -> c.int ---
	LLVMIsALandingPadInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAPHINode :: proc(Val: c.int) -> c.int ---
	LLVMIsASelectInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAShuffleVectorInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAStoreInst :: proc(Val: c.int) -> c.int ---
	LLVMIsABranchInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAIndirectBrInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAInvokeInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAReturnInst :: proc(Val: c.int) -> c.int ---
	LLVMIsASwitchInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAUnreachableInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAResumeInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACleanupReturnInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACatchReturnInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACatchSwitchInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACallBrInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFuncletPadInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACatchPadInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACleanupPadInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAUnaryInstruction :: proc(Val: c.int) -> c.int ---
	LLVMIsAFenceInst :: proc(Val: c.int) -> c.int ---
	LLVMIsACastInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAAddrSpaceCastInst :: proc(Val: c.int) -> c.int ---
	LLVMIsABitCastInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFPExtInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFPToSIInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFPToUIInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFPTruncInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAIntToPtrInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAPtrToIntInst :: proc(Val: c.int) -> c.int ---
	LLVMIsASExtInst :: proc(Val: c.int) -> c.int ---
	LLVMIsASIToFPInst :: proc(Val: c.int) -> c.int ---
	LLVMIsATruncInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAUIToFPInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAZExtInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAExtractValueInst :: proc(Val: c.int) -> c.int ---
	LLVMIsALoadInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAVAArgInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAFreezeInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAAtomicCmpXchgInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAAtomicRMWInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAAllocaInst :: proc(Val: c.int) -> c.int ---
	LLVMIsAMDNode :: proc(Val: c.int) -> c.int ---
	LLVMIsAValueAsMetadata :: proc(Val: c.int) -> c.int ---
	LLVMIsAMDString :: proc(Val: c.int) -> c.int ---

	/** Deprecated: Use LLVMGetValueName2 instead. */
	LLVMGetValueName :: proc(Val: c.int) -> cstring ---

	/** Deprecated: Use LLVMSetValueName2 instead. */
	LLVMSetValueName :: proc(Val: c.int, Name: cstring) ---

	/**
	* Obtain the first use of a value.
	*
	* Uses are obtained in an iterator fashion. First, call this function
	* to obtain a reference to the first use. Then, call LLVMGetNextUse()
	* on that instance and all subsequently obtained instances until
	* LLVMGetNextUse() returns NULL.
	*
	* @see llvm::Value::use_begin()
	*/
	LLVMGetFirstUse :: proc(Val: c.int) -> c.int ---

	/**
	* Obtain the next use of a value.
	*
	* This effectively advances the iterator. It returns NULL if you are on
	* the final use and no more are available.
	*/
	LLVMGetNextUse :: proc(U: c.int) -> c.int ---

	/**
	* Obtain the user value for a user.
	*
	* The returned value corresponds to a llvm::User type.
	*
	* @see llvm::Use::getUser()
	*/
	LLVMGetUser :: proc(U: c.int) -> c.int ---

	/**
	* Obtain the value this use corresponds to.
	*
	* @see llvm::Use::get().
	*/
	LLVMGetUsedValue :: proc(U: c.int) -> c.int ---

	/**
	* Obtain an operand at a specific index in a llvm::User value.
	*
	* @see llvm::User::getOperand()
	*/
	LLVMGetOperand :: proc(Val: c.int, Index: c.uint) -> c.int ---

	/**
	* Obtain the use of an operand at a specific index in a llvm::User value.
	*
	* @see llvm::User::getOperandUse()
	*/
	LLVMGetOperandUse :: proc(Val: c.int, Index: c.uint) -> c.int ---

	/**
	* Set an operand at a specific index in a llvm::User value.
	*
	* @see llvm::User::setOperand()
	*/
	LLVMSetOperand :: proc(User: c.int, Index: c.uint, Val: c.int) ---

	/**
	* Obtain the number of operands in a llvm::User value.
	*
	* @see llvm::User::getNumOperands()
	*/
	LLVMGetNumOperands :: proc(Val: c.int) -> c.int ---

	/**
	* Obtain a constant value referring to the null instance of a type.
	*
	* @see llvm::Constant::getNullValue()
	*/
	LLVMConstNull :: proc(Ty: c.int) -> c.int --- /* all zeroes */

	/**
	* Obtain a constant value referring to the instance of a type
	* consisting of all ones.
	*
	* This is only valid for integer types.
	*
	* @see llvm::Constant::getAllOnesValue()
	*/
	LLVMConstAllOnes :: proc(Ty: c.int) -> c.int ---

	/**
	* Obtain a constant value referring to an undefined value of a type.
	*
	* @see llvm::UndefValue::get()
	*/
	LLVMGetUndef :: proc(Ty: c.int) -> c.int ---

	/**
	* Obtain a constant value referring to a poison value of a type.
	*
	* @see llvm::PoisonValue::get()
	*/
	LLVMGetPoison :: proc(Ty: c.int) -> c.int ---

	/**
	* Determine whether a value instance is null.
	*
	* @see llvm::Constant::isNullValue()
	*/
	LLVMIsNull :: proc(Val: c.int) -> c.int ---

	/**
	* Obtain a constant that is a constant pointer pointing to NULL for a
	* specified type.
	*/
	LLVMConstPointerNull :: proc(Ty: c.int) -> c.int ---

	/**
	* Obtain a constant value for an integer type.
	*
	* The returned value corresponds to a llvm::ConstantInt.
	*
	* @see llvm::ConstantInt::get()
	*
	* @param IntTy Integer type to obtain value of.
	* @param N The value the returned instance should refer to.
	* @param SignExtend Whether to sign extend the produced value.
	*/
	LLVMConstInt :: proc(IntTy: LLVMTypeRef, N: c.ulonglong, SignExtend: LLVMBool) -> LLVMValueRef ---

	/**
	* Obtain a constant value for an integer of arbitrary precision.
	*
	* @see llvm::ConstantInt::get()
	*/
	LLVMConstIntOfArbitraryPrecision :: proc(IntTy: c.int, NumWords: c.uint, Words: []c.int) -> c.int ---

	/**
	* Obtain a constant value for an integer parsed from a string.
	*
	* A similar API, LLVMConstIntOfStringAndSize is also available. If the
	* string's length is available, it is preferred to call that function
	* instead.
	*
	* @see llvm::ConstantInt::get()
	*/
	LLVMConstIntOfString :: proc(IntTy: c.int, Text: cstring, Radix: c.int) -> c.int ---

	/**
	* Obtain a constant value for an integer parsed from a string with
	* specified length.
	*
	* @see llvm::ConstantInt::get()
	*/
	LLVMConstIntOfStringAndSize :: proc(IntTy: c.int, Text: cstring, SLen: c.uint, Radix: c.int) -> c.int ---

	/**
	* Obtain a constant value referring to a double floating point value.
	*/
	LLVMConstReal :: proc(RealTy: LLVMTypeRef, N: f64) -> LLVMValueRef ---

	/**
	* Obtain a constant for a floating point value parsed from a string.
	*
	* A similar API, LLVMConstRealOfStringAndSize is also available. It
	* should be used if the input string's length is known.
	*/
	LLVMConstRealOfString :: proc(RealTy: c.int, Text: cstring) -> c.int ---

	/**
	* Obtain a constant for a floating point value parsed from a string.
	*/
	LLVMConstRealOfStringAndSize :: proc(RealTy: c.int, Text: cstring, SLen: c.uint) -> c.int ---

	/**
	* Obtain the zero extended value for an integer constant value.
	*
	* @see llvm::ConstantInt::getZExtValue()
	*/
	LLVMConstIntGetZExtValue :: proc(ConstantVal: c.int) -> c.ulonglong ---

	/**
	* Obtain the sign extended value for an integer constant value.
	*
	* @see llvm::ConstantInt::getSExtValue()
	*/
	LLVMConstIntGetSExtValue :: proc(ConstantVal: c.int) -> c.longlong ---

	/**
	* Obtain the double value for an floating point constant value.
	* losesInfo indicates if some precision was lost in the conversion.
	*
	* @see llvm::ConstantFP::getDoubleValue
	*/
	LLVMConstRealGetDouble :: proc(ConstantVal: c.int, losesInfo: ^c.int) -> f64 ---

	/**
	* Create a ConstantDataSequential and initialize it with a string.
	*
	* @see llvm::ConstantDataArray::getString()
	*/
	LLVMConstStringInContext :: proc(C: c.int, Str: cstring, Length: c.uint, DontNullTerminate: c.int) -> c.int ---

	/**
	* Create a ConstantDataSequential with string content in the global context.
	*
	* This is the same as LLVMConstStringInContext except it operates on the
	* global context.
	*
	* @see LLVMConstStringInContext()
	* @see llvm::ConstantDataArray::getString()
	*/
	LLVMConstString :: proc(Str: cstring, Length: c.uint, DontNullTerminate: c.int) -> c.int ---

	/**
	* Returns true if the specified constant is an array of i8.
	*
	* @see ConstantDataSequential::getAsString()
	*/
	LLVMIsConstantString :: proc(_c: c.int) -> c.int ---

	/**
	* Get the given constant data sequential as a string.
	*
	* @see ConstantDataSequential::getAsString()
	*/
	LLVMGetAsString :: proc(_c: c.int, Length: ^c.int) -> cstring ---

	/**
	* Create an anonymous ConstantStruct with the specified values.
	*
	* @see llvm::ConstantStruct::getAnon()
	*/
	LLVMConstStructInContext :: proc(C: c.int, ConstantVals: ^c.int, Count: c.uint, Packed: c.int) -> c.int ---

	/**
	* Create a ConstantStruct in the global Context.
	*
	* This is the same as LLVMConstStructInContext except it operates on the
	* global Context.
	*
	* @see LLVMConstStructInContext()
	*/
	LLVMConstStruct :: proc(ConstantVals: ^c.int, Count: c.uint, Packed: c.int) -> c.int ---

	/**
	* Create a ConstantArray from values.
	*
	* @deprecated LLVMConstArray is deprecated in favor of the API accurate
	* LLVMConstArray2
	* @see llvm::ConstantArray::get()
	*/
	LLVMConstArray :: proc(ElementTy: LLVMTypeRef, ConstantVals: ^LLVMValueRef, Length: c.uint) -> LLVMValueRef ---

	/**
	* Create a ConstantArray from values.
	*
	* @see llvm::ConstantArray::get()
	*/
	LLVMConstArray2 :: proc(ElementTy: c.int, ConstantVals: ^c.int, Length: c.int) -> c.int ---

	/**
	* Create a non-anonymous ConstantStruct from values.
	*
	* @see llvm::ConstantStruct::get()
	*/
	LLVMConstNamedStruct :: proc(StructTy: c.int, ConstantVals: ^c.int, Count: c.uint) -> c.int ---

	/**
	* Get element of a constant aggregate (struct, array or vector) at the
	* specified index. Returns null if the index is out of range, or it's not
	* possible to determine the element (e.g., because the constant is a
	* constant expression.)
	*
	* @see llvm::Constant::getAggregateElement()
	*/
	LLVMGetAggregateElement :: proc(C: c.int, Idx: c.uint) -> c.int ---

	/**
	* Get an element at specified index as a constant.
	*
	* @see ConstantDataSequential::getElementAsConstant()
	*/
	LLVM_ATTRIBUTE_C_DEPRECATED :: proc(LLVMGetElementAsConstant: proc "c" (_: c.int, _: c.uint) -> c.int) -> c.int ---

	/**
	* Create a ConstantVector from values.
	*
	* @see llvm::ConstantVector::get()
	*/
	LLVMConstVector :: proc(ScalarConstantVals: ^c.int, Size: c.uint) -> c.int ---

	/**
	* @defgroup LLVMCCoreValueConstantExpressions Constant Expressions
	*
	* Functions in this group correspond to APIs on llvm::ConstantExpr.
	*
	* @see llvm::ConstantExpr.
	*
	* @{
	*/
	LLVMGetConstOpcode :: proc(ConstantVal: c.int) -> LLVMOpcode ---
	LLVMAlignOf :: proc(Ty: c.int) -> c.int ---
	LLVMSizeOf :: proc(Ty: c.int) -> c.int ---
	LLVMConstNeg :: proc(ConstantVal: c.int) -> c.int ---
	LLVMConstNSWNeg :: proc(ConstantVal: c.int) -> c.int ---
	LLVMConstNUWNeg :: proc(ConstantVal: c.int) -> c.int ---
	LLVMConstNot :: proc(ConstantVal: c.int) -> c.int ---
	LLVMConstAdd :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstNSWAdd :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstNUWAdd :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstSub :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstNSWSub :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstNUWSub :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstMul :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstNSWMul :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstNUWMul :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstXor :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstICmp :: proc(Predicate: LLVMIntPredicate, LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstFCmp :: proc(Predicate: LLVMRealPredicate, LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstShl :: proc(LHSConstant: c.int, RHSConstant: c.int) -> c.int ---
	LLVMConstGEP2 :: proc(Ty: c.int, ConstantVal: c.int, ConstantIndices: ^c.int, NumIndices: c.uint) -> c.int ---
	LLVMConstInBoundsGEP2 :: proc(Ty: c.int, ConstantVal: c.int, ConstantIndices: ^c.int, NumIndices: c.uint) -> c.int ---
	LLVMConstTrunc :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstPtrToInt :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstIntToPtr :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstBitCast :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstAddrSpaceCast :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstTruncOrBitCast :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstPointerCast :: proc(ConstantVal: c.int, ToType: c.int) -> c.int ---
	LLVMConstExtractElement :: proc(VectorConstant: c.int, IndexConstant: c.int) -> c.int ---
	LLVMConstInsertElement :: proc(VectorConstant: c.int, ElementValueConstant: c.int, IndexConstant: c.int) -> c.int ---
	LLVMConstShuffleVector :: proc(VectorAConstant: c.int, VectorBConstant: c.int, MaskConstant: c.int) -> c.int ---
	LLVMBlockAddress :: proc(F: c.int, BB: c.int) -> c.int ---

	/** Deprecated: Use LLVMGetInlineAsm instead. */
	LLVMConstInlineAsm :: proc(Ty: c.int, AsmString: cstring, Constraints: cstring, HasSideEffects: c.int, IsAlignStack: c.int) -> c.int ---

	/**
	* @defgroup LLVMCCoreValueConstantGlobals Global Values
	*
	* This group contains functions that operate on global values. Functions in
	* this group relate to functions in the llvm::GlobalValue class tree.
	*
	* @see llvm::GlobalValue
	*
	* @{
	*/
	LLVMGetGlobalParent :: proc(Global: c.int) -> c.int ---
	LLVMIsDeclaration :: proc(Global: c.int) -> c.int ---
	LLVMGetLinkage :: proc(Global: c.int) -> LLVMLinkage ---
	LLVMSetLinkage :: proc(Global: LLVMValueRef, Linkage: LLVMLinkage) ---
	LLVMGetSection :: proc(Global: c.int) -> cstring ---
	LLVMSetSection :: proc(Global: c.int, Section: cstring) ---
	LLVMGetVisibility :: proc(Global: c.int) -> LLVMVisibility ---
	LLVMSetVisibility :: proc(Global: c.int, Viz: LLVMVisibility) ---
	LLVMGetDLLStorageClass :: proc(Global: c.int) -> LLVMDLLStorageClass ---
	LLVMSetDLLStorageClass :: proc(Global: c.int, Class: LLVMDLLStorageClass) ---
	LLVMGetUnnamedAddress :: proc(Global: c.int) -> LLVMUnnamedAddr ---
	LLVMSetUnnamedAddress :: proc(Global: c.int, UnnamedAddr: LLVMUnnamedAddr) ---

	/**
	* Returns the "value type" of a global value.  This differs from the formal
	* type of a global value which is always a pointer type.
	*
	* @see llvm::GlobalValue::getValueType()
	*/
	LLVMGlobalGetValueType :: proc(Global: c.int) -> c.int ---

	/** Deprecated: Use LLVMGetUnnamedAddress instead. */
	LLVMHasUnnamedAddr :: proc(Global: c.int) -> c.int ---

	/** Deprecated: Use LLVMSetUnnamedAddress instead. */
	LLVMSetUnnamedAddr :: proc(Global: c.int, HasUnnamedAddr: c.int) ---

	/**
	* Obtain the preferred alignment of the value.
	* @see llvm::AllocaInst::getAlignment()
	* @see llvm::LoadInst::getAlignment()
	* @see llvm::StoreInst::getAlignment()
	* @see llvm::AtomicRMWInst::setAlignment()
	* @see llvm::AtomicCmpXchgInst::setAlignment()
	* @see llvm::GlobalValue::getAlignment()
	*/
	LLVMGetAlignment :: proc(V: c.int) -> c.uint ---

	/**
	* Set the preferred alignment of the value.
	* @see llvm::AllocaInst::setAlignment()
	* @see llvm::LoadInst::setAlignment()
	* @see llvm::StoreInst::setAlignment()
	* @see llvm::AtomicRMWInst::setAlignment()
	* @see llvm::AtomicCmpXchgInst::setAlignment()
	* @see llvm::GlobalValue::setAlignment()
	*/
	LLVMSetAlignment :: proc(V: c.int, Bytes: c.uint) ---

	/**
	* Sets a metadata attachment, erasing the existing metadata attachment if
	* it already exists for the given kind.
	*
	* @see llvm::GlobalObject::setMetadata()
	*/
	LLVMGlobalSetMetadata :: proc(Global: c.int, Kind: c.uint, MD: c.int) ---

	/**
	* Erases a metadata attachment of the given kind if it exists.
	*
	* @see llvm::GlobalObject::eraseMetadata()
	*/
	LLVMGlobalEraseMetadata :: proc(Global: c.int, Kind: c.uint) ---

	/**
	* Removes all metadata attachments from this value.
	*
	* @see llvm::GlobalObject::clearMetadata()
	*/
	LLVMGlobalClearMetadata :: proc(Global: c.int) ---

	/**
	* Retrieves an array of metadata entries representing the metadata attached to
	* this value. The caller is responsible for freeing this array by calling
	* \c LLVMDisposeValueMetadataEntries.
	*
	* @see llvm::GlobalObject::getAllMetadata()
	*/
	LLVMGlobalCopyAllMetadata :: proc(Value: c.int, NumEntries: ^c.int) -> ^c.int ---

	/**
	* Destroys value metadata entries.
	*/
	LLVMDisposeValueMetadataEntries :: proc(Entries: ^c.int) ---

	/**
	* Returns the kind of a value metadata entry at a specific index.
	*/
	LLVMValueMetadataEntriesGetKind :: proc(Entries: ^c.int, Index: c.uint) -> c.uint ---

	/**
	* Returns the underlying metadata node of a value metadata entry at a
	* specific index.
	*/
	LLVMValueMetadataEntriesGetMetadata :: proc(Entries: ^c.int, Index: c.uint) -> c.int ---

	/**
	* @defgroup LLVMCoreValueConstantGlobalVariable Global Variables
	*
	* This group contains functions that operate on global variable values.
	*
	* @see llvm::GlobalVariable
	*
	* @{
	*/
	LLVMAddGlobal :: proc(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: cstring) -> LLVMValueRef ---
	LLVMAddGlobalInAddressSpace :: proc(M: c.int, Ty: c.int, Name: cstring, AddressSpace: c.uint) -> c.int ---
	LLVMGetNamedGlobal :: proc(M: c.int, Name: cstring) -> c.int ---
	LLVMGetFirstGlobal :: proc(M: c.int) -> c.int ---
	LLVMGetLastGlobal :: proc(M: c.int) -> c.int ---
	LLVMGetNextGlobal :: proc(GlobalVar: c.int) -> c.int ---
	LLVMGetPreviousGlobal :: proc(GlobalVar: c.int) -> c.int ---
	LLVMDeleteGlobal :: proc(GlobalVar: c.int) ---
	LLVMGetInitializer :: proc(GlobalVar: c.int) -> c.int ---
	LLVMSetInitializer :: proc(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef) ---
	LLVMIsThreadLocal :: proc(GlobalVar: c.int) -> c.int ---
	LLVMSetThreadLocal :: proc(GlobalVar: c.int, IsThreadLocal: c.int) ---
	LLVMIsGlobalConstant :: proc(GlobalVar: c.int) -> c.int ---
	LLVMSetGlobalConstant :: proc(GlobalVar: c.int, IsConstant: c.int) ---
	LLVMGetThreadLocalMode :: proc(GlobalVar: c.int) -> LLVMThreadLocalMode ---
	LLVMSetThreadLocalMode :: proc(GlobalVar: c.int, Mode: LLVMThreadLocalMode) ---
	LLVMIsExternallyInitialized :: proc(GlobalVar: c.int) -> c.int ---
	LLVMSetExternallyInitialized :: proc(GlobalVar: c.int, IsExtInit: c.int) ---

	/**
	* Add a GlobalAlias with the given value type, address space and aliasee.
	*
	* @see llvm::GlobalAlias::create()
	*/
	LLVMAddAlias2 :: proc(M: c.int, ValueTy: c.int, AddrSpace: c.uint, Aliasee: c.int, Name: cstring) -> c.int ---

	/**
	* Obtain a GlobalAlias value from a Module by its name.
	*
	* The returned value corresponds to a llvm::GlobalAlias value.
	*
	* @see llvm::Module::getNamedAlias()
	*/
	LLVMGetNamedGlobalAlias :: proc(M: c.int, Name: cstring, NameLen: c.int) -> c.int ---

	/**
	* Obtain an iterator to the first GlobalAlias in a Module.
	*
	* @see llvm::Module::alias_begin()
	*/
	LLVMGetFirstGlobalAlias :: proc(M: c.int) -> c.int ---

	/**
	* Obtain an iterator to the last GlobalAlias in a Module.
	*
	* @see llvm::Module::alias_end()
	*/
	LLVMGetLastGlobalAlias :: proc(M: c.int) -> c.int ---

	/**
	* Advance a GlobalAlias iterator to the next GlobalAlias.
	*
	* Returns NULL if the iterator was already at the end and there are no more
	* global aliases.
	*/
	LLVMGetNextGlobalAlias :: proc(GA: c.int) -> c.int ---

	/**
	* Decrement a GlobalAlias iterator to the previous GlobalAlias.
	*
	* Returns NULL if the iterator was already at the beginning and there are
	* no previous global aliases.
	*/
	LLVMGetPreviousGlobalAlias :: proc(GA: c.int) -> c.int ---

	/**
	* Retrieve the target value of an alias.
	*/
	LLVMAliasGetAliasee :: proc(Alias: c.int) -> c.int ---

	/**
	* Set the target value of an alias.
	*/
	LLVMAliasSetAliasee :: proc(Alias: c.int, Aliasee: c.int) ---

	/**
	* Remove a function from its containing module and deletes it.
	*
	* @see llvm::Function::eraseFromParent()
	*/
	LLVMDeleteFunction :: proc(Fn: c.int) ---

	/**
	* Check whether the given function has a personality function.
	*
	* @see llvm::Function::hasPersonalityFn()
	*/
	LLVMHasPersonalityFn :: proc(Fn: c.int) -> c.int ---

	/**
	* Obtain the personality function attached to the function.
	*
	* @see llvm::Function::getPersonalityFn()
	*/
	LLVMGetPersonalityFn :: proc(Fn: c.int) -> c.int ---

	/**
	* Set the personality function attached to the function.
	*
	* @see llvm::Function::setPersonalityFn()
	*/
	LLVMSetPersonalityFn :: proc(Fn: c.int, PersonalityFn: c.int) ---

	/**
	* Obtain the intrinsic ID number which matches the given function name.
	*
	* @see llvm::Function::lookupIntrinsicID()
	*/
	LLVMLookupIntrinsicID :: proc(Name: cstring, NameLen: c.int) -> c.uint ---

	/**
	* Obtain the ID number from a function instance.
	*
	* @see llvm::Function::getIntrinsicID()
	*/
	LLVMGetIntrinsicID :: proc(Fn: c.int) -> c.uint ---

	/**
	* Create or insert the declaration of an intrinsic.  For overloaded intrinsics,
	* parameter types must be provided to uniquely identify an overload.
	*
	* @see llvm::Intrinsic::getDeclaration()
	*/
	LLVMGetIntrinsicDeclaration :: proc(Mod: c.int, ID: c.uint, ParamTypes: ^c.int, ParamCount: c.int) -> c.int ---

	/**
	* Retrieves the type of an intrinsic.  For overloaded intrinsics, parameter
	* types must be provided to uniquely identify an overload.
	*
	* @see llvm::Intrinsic::getType()
	*/
	LLVMIntrinsicGetType :: proc(Ctx: c.int, ID: c.uint, ParamTypes: ^c.int, ParamCount: c.int) -> c.int ---

	/**
	* Retrieves the name of an intrinsic.
	*
	* @see llvm::Intrinsic::getName()
	*/
	LLVMIntrinsicGetName :: proc(ID: c.uint, NameLength: ^c.int) -> cstring ---

	/** Deprecated: Use LLVMIntrinsicCopyOverloadedName2 instead. */
	LLVMIntrinsicCopyOverloadedName :: proc(ID: c.uint, ParamTypes: ^c.int, ParamCount: c.int, NameLength: ^c.int) -> cstring ---

	/**
	* Copies the name of an overloaded intrinsic identified by a given list of
	* parameter types.
	*
	* Unlike LLVMIntrinsicGetName, the caller is responsible for freeing the
	* returned string.
	*
	* This version also supports unnamed types.
	*
	* @see llvm::Intrinsic::getName()
	*/
	LLVMIntrinsicCopyOverloadedName2 :: proc(Mod: c.int, ID: c.uint, ParamTypes: ^c.int, ParamCount: c.int, NameLength: ^c.int) -> cstring ---

	/**
	* Obtain if the intrinsic identified by the given ID is overloaded.
	*
	* @see llvm::Intrinsic::isOverloaded()
	*/
	LLVMIntrinsicIsOverloaded :: proc(ID: c.uint) -> c.int ---

	/**
	* Obtain the calling function of a function.
	*
	* The returned value corresponds to the LLVMCallConv enumeration.
	*
	* @see llvm::Function::getCallingConv()
	*/
	LLVMGetFunctionCallConv :: proc(Fn: c.int) -> c.uint ---

	/**
	* Set the calling convention of a function.
	*
	* @see llvm::Function::setCallingConv()
	*
	* @param Fn Function to operate on
	* @param CC LLVMCallConv to set calling convention to
	*/
	LLVMSetFunctionCallConv :: proc(Fn: c.int, CC: c.uint) ---

	/**
	* Obtain the name of the garbage collector to use during code
	* generation.
	*
	* @see llvm::Function::getGC()
	*/
	LLVMGetGC :: proc(Fn: c.int) -> cstring ---

	/**
	* Define the garbage collector to use during code generation.
	*
	* @see llvm::Function::setGC()
	*/
	LLVMSetGC :: proc(Fn: c.int, Name: cstring) ---

	/**
	* Add an attribute to a function.
	*
	* @see llvm::Function::addAttribute()
	*/
	LLVMAddAttributeAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex, A: c.int) ---
	LLVMGetAttributeCountAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex) -> c.uint ---
	LLVMGetAttributesAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex, Attrs: ^c.int) ---
	LLVMGetEnumAttributeAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex, KindID: c.uint) -> c.int ---
	LLVMGetStringAttributeAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex, K: cstring, KLen: c.uint) -> c.int ---
	LLVMRemoveEnumAttributeAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex, KindID: c.uint) ---
	LLVMRemoveStringAttributeAtIndex :: proc(F: c.int, Idx: LLVMAttributeIndex, K: cstring, KLen: c.uint) ---

	/**
	* Add a target-dependent attribute to a function
	* @see llvm::AttrBuilder::addAttribute()
	*/
	LLVMAddTargetDependentFunctionAttr :: proc(Fn: c.int, A: cstring, V: cstring) ---

	/**
	* Obtain the number of parameters in a function.
	*
	* @see llvm::Function::arg_size()
	*/
	LLVMCountParams :: proc(Fn: c.int) -> c.uint ---

	/**
	* Obtain the parameters in a function.
	*
	* The takes a pointer to a pre-allocated array of LLVMValueRef that is
	* at least LLVMCountParams() long. This array will be filled with
	* LLVMValueRef instances which correspond to the parameters the
	* function receives. Each LLVMValueRef corresponds to a llvm::Argument
	* instance.
	*
	* @see llvm::Function::arg_begin()
	*/
	LLVMGetParams :: proc(Fn: c.int, Params: ^c.int) ---

	/**
	* Obtain the parameter at the specified index.
	*
	* Parameters are indexed from 0.
	*
	* @see llvm::Function::arg_begin()
	*/
	LLVMGetParam :: proc(Fn: LLVMValueRef, Index: c.uint) -> LLVMValueRef ---

	/**
	* Obtain the function to which this argument belongs.
	*
	* Unlike other functions in this group, this one takes an LLVMValueRef
	* that corresponds to a llvm::Attribute.
	*
	* The returned LLVMValueRef is the llvm::Function to which this
	* argument belongs.
	*/
	LLVMGetParamParent :: proc(Inst: c.int) -> c.int ---

	/**
	* Obtain the first parameter to a function.
	*
	* @see llvm::Function::arg_begin()
	*/
	LLVMGetFirstParam :: proc(Fn: c.int) -> c.int ---

	/**
	* Obtain the last parameter to a function.
	*
	* @see llvm::Function::arg_end()
	*/
	LLVMGetLastParam :: proc(Fn: c.int) -> c.int ---

	/**
	* Obtain the next parameter to a function.
	*
	* This takes an LLVMValueRef obtained from LLVMGetFirstParam() (which is
	* actually a wrapped iterator) and obtains the next parameter from the
	* underlying iterator.
	*/
	LLVMGetNextParam :: proc(Arg: c.int) -> c.int ---

	/**
	* Obtain the previous parameter to a function.
	*
	* This is the opposite of LLVMGetNextParam().
	*/
	LLVMGetPreviousParam :: proc(Arg: c.int) -> c.int ---

	/**
	* Set the alignment for a function parameter.
	*
	* @see llvm::Argument::addAttr()
	* @see llvm::AttrBuilder::addAlignmentAttr()
	*/
	LLVMSetParamAlignment :: proc(Arg: c.int, Align: c.uint) ---

	/**
	* Add a global indirect function to a module under a specified name.
	*
	* @see llvm::GlobalIFunc::create()
	*/
	LLVMAddGlobalIFunc :: proc(M: c.int, Name: cstring, NameLen: c.int, Ty: c.int, AddrSpace: c.uint, Resolver: c.int) -> c.int ---

	/**
	* Obtain a GlobalIFunc value from a Module by its name.
	*
	* The returned value corresponds to a llvm::GlobalIFunc value.
	*
	* @see llvm::Module::getNamedIFunc()
	*/
	LLVMGetNamedGlobalIFunc :: proc(M: c.int, Name: cstring, NameLen: c.int) -> c.int ---

	/**
	* Obtain an iterator to the first GlobalIFunc in a Module.
	*
	* @see llvm::Module::ifunc_begin()
	*/
	LLVMGetFirstGlobalIFunc :: proc(M: c.int) -> c.int ---

	/**
	* Obtain an iterator to the last GlobalIFunc in a Module.
	*
	* @see llvm::Module::ifunc_end()
	*/
	LLVMGetLastGlobalIFunc :: proc(M: c.int) -> c.int ---

	/**
	* Advance a GlobalIFunc iterator to the next GlobalIFunc.
	*
	* Returns NULL if the iterator was already at the end and there are no more
	* global aliases.
	*/
	LLVMGetNextGlobalIFunc :: proc(IFunc: c.int) -> c.int ---

	/**
	* Decrement a GlobalIFunc iterator to the previous GlobalIFunc.
	*
	* Returns NULL if the iterator was already at the beginning and there are
	* no previous global aliases.
	*/
	LLVMGetPreviousGlobalIFunc :: proc(IFunc: c.int) -> c.int ---

	/**
	* Retrieves the resolver function associated with this indirect function, or
	* NULL if it doesn't not exist.
	*
	* @see llvm::GlobalIFunc::getResolver()
	*/
	LLVMGetGlobalIFuncResolver :: proc(IFunc: c.int) -> c.int ---

	/**
	* Sets the resolver function associated with this indirect function.
	*
	* @see llvm::GlobalIFunc::setResolver()
	*/
	LLVMSetGlobalIFuncResolver :: proc(IFunc: c.int, Resolver: c.int) ---

	/**
	* Remove a global indirect function from its parent module and delete it.
	*
	* @see llvm::GlobalIFunc::eraseFromParent()
	*/
	LLVMEraseGlobalIFunc :: proc(IFunc: c.int) ---

	/**
	* Remove a global indirect function from its parent module.
	*
	* This unlinks the global indirect function from its containing module but
	* keeps it alive.
	*
	* @see llvm::GlobalIFunc::removeFromParent()
	*/
	LLVMRemoveGlobalIFunc :: proc(IFunc: c.int) ---

	/**
	* Create an MDString value from a given string value.
	*
	* The MDString value does not take ownership of the given string, it remains
	* the responsibility of the caller to free it.
	*
	* @see llvm::MDString::get()
	*/
	LLVMMDStringInContext2 :: proc(C: c.int, Str: cstring, SLen: c.int) -> c.int ---

	/**
	* Create an MDNode value with the given array of operands.
	*
	* @see llvm::MDNode::get()
	*/
	LLVMMDNodeInContext2 :: proc(C: c.int, MDs: ^c.int, Count: c.int) -> c.int ---

	/**
	* Obtain a Metadata as a Value.
	*/
	LLVMMetadataAsValue :: proc(C: c.int, MD: c.int) -> c.int ---

	/**
	* Obtain a Value as a Metadata.
	*/
	LLVMValueAsMetadata :: proc(Val: c.int) -> c.int ---

	/**
	* Obtain the underlying string from a MDString value.
	*
	* @param V Instance to obtain string from.
	* @param Length Memory address which will hold length of returned string.
	* @return String data in MDString.
	*/
	LLVMGetMDString :: proc(V: c.int, Length: ^c.uint) -> cstring ---

	/**
	* Obtain the number of operands from an MDNode value.
	*
	* @param V MDNode to get number of operands from.
	* @return Number of operands of the MDNode.
	*/
	LLVMGetMDNodeNumOperands :: proc(V: c.int) -> c.uint ---

	/**
	* Obtain the given MDNode's operands.
	*
	* The passed LLVMValueRef pointer should point to enough memory to hold all of
	* the operands of the given MDNode (see LLVMGetMDNodeNumOperands) as
	* LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the
	* MDNode's operands.
	*
	* @param V MDNode to get the operands from.
	* @param Dest Destination array for operands.
	*/
	LLVMGetMDNodeOperands :: proc(V: c.int, Dest: ^c.int) ---

	/**
	* Replace an operand at a specific index in a llvm::MDNode value.
	*
	* @see llvm::MDNode::replaceOperandWith()
	*/
	LLVMReplaceMDNodeOperandWith :: proc(V: c.int, Index: c.uint, Replacement: c.int) ---

	/** Deprecated: Use LLVMMDStringInContext2 instead. */
	LLVMMDStringInContext :: proc(C: c.int, Str: cstring, SLen: c.uint) -> c.int ---

	/** Deprecated: Use LLVMMDStringInContext2 instead. */
	LLVMMDString :: proc(Str: cstring, SLen: c.uint) -> c.int ---

	/** Deprecated: Use LLVMMDNodeInContext2 instead. */
	LLVMMDNodeInContext :: proc(C: c.int, Vals: ^c.int, Count: c.uint) -> c.int ---

	/** Deprecated: Use LLVMMDNodeInContext2 instead. */
	LLVMMDNode :: proc(Vals: ^c.int, Count: c.uint) -> c.int ---

	/**
	* Create a new operand bundle.
	*
	* Every invocation should be paired with LLVMDisposeOperandBundle() or memory
	* will be leaked.
	*
	* @param Tag Tag name of the operand bundle
	* @param TagLen Length of Tag
	* @param Args Memory address of an array of bundle operands
	* @param NumArgs Length of Args
	*/
	LLVMCreateOperandBundle :: proc(Tag: cstring, TagLen: c.int, Args: ^c.int, NumArgs: c.uint) -> c.int ---

	/**
	* Destroy an operand bundle.
	*
	* This must be called for every created operand bundle or memory will be
	* leaked.
	*/
	LLVMDisposeOperandBundle :: proc(Bundle: c.int) ---

	/**
	* Obtain the tag of an operand bundle as a string.
	*
	* @param Bundle Operand bundle to obtain tag of.
	* @param Len Out parameter which holds the length of the returned string.
	* @return The tag name of Bundle.
	* @see OperandBundleDef::getTag()
	*/
	LLVMGetOperandBundleTag :: proc(Bundle: c.int, Len: ^c.int) -> cstring ---

	/**
	* Obtain the number of operands for an operand bundle.
	*
	* @param Bundle Operand bundle to obtain operand count of.
	* @return The number of operands.
	* @see OperandBundleDef::input_size()
	*/
	LLVMGetNumOperandBundleArgs :: proc(Bundle: c.int) -> c.uint ---

	/**
	* Obtain the operand for an operand bundle at the given index.
	*
	* @param Bundle Operand bundle to obtain operand of.
	* @param Index An operand index, must be less than
	* LLVMGetNumOperandBundleArgs().
	* @return The operand.
	*/
	LLVMGetOperandBundleArgAtIndex :: proc(Bundle: c.int, Index: c.uint) -> c.int ---

	/**
	* Convert a basic block instance to a value type.
	*/
	LLVMBasicBlockAsValue :: proc(BB: c.int) -> c.int ---

	/**
	* Determine whether an LLVMValueRef is itself a basic block.
	*/
	LLVMValueIsBasicBlock :: proc(Val: c.int) -> c.int ---

	/**
	* Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
	*/
	LLVMValueAsBasicBlock :: proc(Val: c.int) -> c.int ---

	/**
	* Obtain the string name of a basic block.
	*/
	LLVMGetBasicBlockName :: proc(BB: c.int) -> cstring ---

	/**
	* Obtain the function to which a basic block belongs.
	*
	* @see llvm::BasicBlock::getParent()
	*/
	LLVMGetBasicBlockParent :: proc(BB: c.int) -> c.int ---

	/**
	* Obtain the terminator instruction for a basic block.
	*
	* If the basic block does not have a terminator (it is not well-formed
	* if it doesn't), then NULL is returned.
	*
	* The returned LLVMValueRef corresponds to an llvm::Instruction.
	*
	* @see llvm::BasicBlock::getTerminator()
	*/
	LLVMGetBasicBlockTerminator :: proc(BB: c.int) -> c.int ---

	/**
	* Obtain the number of basic blocks in a function.
	*
	* @param Fn Function value to operate on.
	*/
	LLVMCountBasicBlocks :: proc(Fn: c.int) -> c.uint ---

	/**
	* Obtain all of the basic blocks in a function.
	*
	* This operates on a function value. The BasicBlocks parameter is a
	* pointer to a pre-allocated array of LLVMBasicBlockRef of at least
	* LLVMCountBasicBlocks() in length. This array is populated with
	* LLVMBasicBlockRef instances.
	*/
	LLVMGetBasicBlocks :: proc(Fn: c.int, BasicBlocks: ^c.int) ---

	/**
	* Obtain the first basic block in a function.
	*
	* The returned basic block can be used as an iterator. You will likely
	* eventually call into LLVMGetNextBasicBlock() with it.
	*
	* @see llvm::Function::begin()
	*/
	LLVMGetFirstBasicBlock :: proc(Fn: c.int) -> c.int ---

	/**
	* Obtain the last basic block in a function.
	*
	* @see llvm::Function::end()
	*/
	LLVMGetLastBasicBlock :: proc(Fn: c.int) -> c.int ---

	/**
	* Advance a basic block iterator.
	*/
	LLVMGetNextBasicBlock :: proc(BB: c.int) -> c.int ---

	/**
	* Go backwards in a basic block iterator.
	*/
	LLVMGetPreviousBasicBlock :: proc(BB: c.int) -> c.int ---

	/**
	* Obtain the basic block that corresponds to the entry point of a
	* function.
	*
	* @see llvm::Function::getEntryBlock()
	*/
	LLVMGetEntryBasicBlock :: proc(Fn: c.int) -> c.int ---

	/**
	* Insert the given basic block after the insertion point of the given builder.
	*
	* The insertion point must be valid.
	*
	* @see llvm::Function::BasicBlockListType::insertAfter()
	*/
	LLVMInsertExistingBasicBlockAfterInsertBlock :: proc(Builder: c.int, BB: c.int) ---

	/**
	* Append the given basic block to the basic block list of the given function.
	*
	* @see llvm::Function::BasicBlockListType::push_back()
	*/
	LLVMAppendExistingBasicBlock :: proc(Fn: c.int, BB: c.int) ---

	/**
	* Create a new basic block without inserting it into a function.
	*
	* @see llvm::BasicBlock::Create()
	*/
	LLVMCreateBasicBlockInContext :: proc(C: c.int, Name: cstring) -> c.int ---

	/**
	* Append a basic block to the end of a function.
	*
	* @see llvm::BasicBlock::Create()
	*/
	LLVMAppendBasicBlockInContext :: proc(C: LLVMContextRef, Fn: LLVMValueRef, Name: cstring) -> LLVMBasicBlockRef ---

	/**
	* Append a basic block to the end of a function using the global
	* context.
	*
	* @see llvm::BasicBlock::Create()
	*/
	LLVMAppendBasicBlock :: proc(Fn: c.int, Name: cstring) -> c.int ---

	/**
	* Insert a basic block in a function before another basic block.
	*
	* The function to add to is determined by the function of the
	* passed basic block.
	*
	* @see llvm::BasicBlock::Create()
	*/
	LLVMInsertBasicBlockInContext :: proc(C: c.int, BB: c.int, Name: cstring) -> c.int ---

	/**
	* Insert a basic block in a function using the global context.
	*
	* @see llvm::BasicBlock::Create()
	*/
	LLVMInsertBasicBlock :: proc(InsertBeforeBB: c.int, Name: cstring) -> c.int ---

	/**
	* Remove a basic block from a function and delete it.
	*
	* This deletes the basic block from its containing function and deletes
	* the basic block itself.
	*
	* @see llvm::BasicBlock::eraseFromParent()
	*/
	LLVMDeleteBasicBlock :: proc(BB: c.int) ---

	/**
	* Remove a basic block from a function.
	*
	* This deletes the basic block from its containing function but keep
	* the basic block alive.
	*
	* @see llvm::BasicBlock::removeFromParent()
	*/
	LLVMRemoveBasicBlockFromParent :: proc(BB: c.int) ---

	/**
	* Move a basic block to before another one.
	*
	* @see llvm::BasicBlock::moveBefore()
	*/
	LLVMMoveBasicBlockBefore :: proc(BB: c.int, MovePos: c.int) ---

	/**
	* Move a basic block to after another one.
	*
	* @see llvm::BasicBlock::moveAfter()
	*/
	LLVMMoveBasicBlockAfter :: proc(BB: c.int, MovePos: c.int) ---

	/**
	* Obtain the first instruction in a basic block.
	*
	* The returned LLVMValueRef corresponds to a llvm::Instruction
	* instance.
	*/
	LLVMGetFirstInstruction :: proc(BB: c.int) -> c.int ---

	/**
	* Obtain the last instruction in a basic block.
	*
	* The returned LLVMValueRef corresponds to an LLVM:Instruction.
	*/
	LLVMGetLastInstruction :: proc(BB: LLVMBasicBlockRef) -> LLVMValueRef ---

	/**
	* Determine whether an instruction has any metadata attached.
	*/
	LLVMHasMetadata :: proc(Val: c.int) -> c.int ---

	/**
	* Return metadata associated with an instruction value.
	*/
	LLVMGetMetadata :: proc(Val: c.int, KindID: c.uint) -> c.int ---

	/**
	* Set metadata associated with an instruction value.
	*/
	LLVMSetMetadata :: proc(Val: c.int, KindID: c.uint, Node: c.int) ---

	/**
	* Returns the metadata associated with an instruction value, but filters out
	* all the debug locations.
	*
	* @see llvm::Instruction::getAllMetadataOtherThanDebugLoc()
	*/
	LLVMInstructionGetAllMetadataOtherThanDebugLoc :: proc(Instr: c.int, NumEntries: ^c.int) -> ^c.int ---

	/**
	* Obtain the basic block to which an instruction belongs.
	*
	* @see llvm::Instruction::getParent()
	*/
	LLVMGetInstructionParent :: proc(Inst: c.int) -> c.int ---

	/**
	* Obtain the instruction that occurs after the one specified.
	*
	* The next instruction will be from the same basic block.
	*
	* If this is the last instruction in a basic block, NULL will be
	* returned.
	*/
	LLVMGetNextInstruction :: proc(Inst: c.int) -> c.int ---

	/**
	* Obtain the instruction that occurred before this one.
	*
	* If the instruction is the first instruction in a basic block, NULL
	* will be returned.
	*/
	LLVMGetPreviousInstruction :: proc(Inst: c.int) -> c.int ---

	/**
	* Remove an instruction.
	*
	* The instruction specified is removed from its containing building
	* block but is kept alive.
	*
	* @see llvm::Instruction::removeFromParent()
	*/
	LLVMInstructionRemoveFromParent :: proc(Inst: c.int) ---

	/**
	* Remove and delete an instruction.
	*
	* The instruction specified is removed from its containing building
	* block and then deleted.
	*
	* @see llvm::Instruction::eraseFromParent()
	*/
	LLVMInstructionEraseFromParent :: proc(Inst: c.int) ---

	/**
	* Delete an instruction.
	*
	* The instruction specified is deleted. It must have previously been
	* removed from its containing building block.
	*
	* @see llvm::Value::deleteValue()
	*/
	LLVMDeleteInstruction :: proc(Inst: c.int) ---

	/**
	* Obtain the code opcode for an individual instruction.
	*
	* @see llvm::Instruction::getOpCode()
	*/
	LLVMGetInstructionOpcode :: proc(Inst: c.int) -> LLVMOpcode ---

	/**
	* Obtain the predicate of an instruction.
	*
	* This is only valid for instructions that correspond to llvm::ICmpInst
	* or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
	*
	* @see llvm::ICmpInst::getPredicate()
	*/
	LLVMGetICmpPredicate :: proc(Inst: c.int) -> LLVMIntPredicate ---

	/**
	* Obtain the float predicate of an instruction.
	*
	* This is only valid for instructions that correspond to llvm::FCmpInst
	* or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
	*
	* @see llvm::FCmpInst::getPredicate()
	*/
	LLVMGetFCmpPredicate :: proc(Inst: c.int) -> LLVMRealPredicate ---

	/**
	* Create a copy of 'this' instruction that is identical in all ways
	* except the following:
	*   * The instruction has no parent
	*   * The instruction has no name
	*
	* @see llvm::Instruction::clone()
	*/
	LLVMInstructionClone :: proc(Inst: c.int) -> c.int ---

	/**
	* Determine whether an instruction is a terminator. This routine is named to
	* be compatible with historical functions that did this by querying the
	* underlying C++ type.
	*
	* @see llvm::Instruction::isTerminator()
	*/
	LLVMIsATerminatorInst :: proc(Inst: LLVMValueRef) -> LLVMValueRef ---

	/**
	* Obtain the argument count for a call instruction.
	*
	* This expects an LLVMValueRef that corresponds to a llvm::CallInst,
	* llvm::InvokeInst, or llvm:FuncletPadInst.
	*
	* @see llvm::CallInst::getNumArgOperands()
	* @see llvm::InvokeInst::getNumArgOperands()
	* @see llvm::FuncletPadInst::getNumArgOperands()
	*/
	LLVMGetNumArgOperands :: proc(Instr: c.int) -> c.uint ---

	/**
	* Set the calling convention for a call instruction.
	*
	* This expects an LLVMValueRef that corresponds to a llvm::CallInst or
	* llvm::InvokeInst.
	*
	* @see llvm::CallInst::setCallingConv()
	* @see llvm::InvokeInst::setCallingConv()
	*/
	LLVMSetInstructionCallConv :: proc(Instr: c.int, CC: c.uint) ---

	/**
	* Obtain the calling convention for a call instruction.
	*
	* This is the opposite of LLVMSetInstructionCallConv(). Reads its
	* usage.
	*
	* @see LLVMSetInstructionCallConv()
	*/
	LLVMGetInstructionCallConv :: proc(Instr: c.int) -> c.uint ---
	LLVMSetInstrParamAlignment :: proc(Instr: c.int, Idx: LLVMAttributeIndex, Align: c.uint) ---
	LLVMAddCallSiteAttribute :: proc(C: LLVMValueRef, Idx: LLVMAttributeIndex, A: LLVMAttributeRef) ---
	LLVMGetCallSiteAttributeCount :: proc(C: c.int, Idx: LLVMAttributeIndex) -> c.uint ---
	LLVMGetCallSiteAttributes :: proc(C: c.int, Idx: LLVMAttributeIndex, Attrs: ^c.int) ---
	LLVMGetCallSiteEnumAttribute :: proc(C: c.int, Idx: LLVMAttributeIndex, KindID: c.uint) -> c.int ---
	LLVMGetCallSiteStringAttribute :: proc(C: c.int, Idx: LLVMAttributeIndex, K: cstring, KLen: c.uint) -> c.int ---
	LLVMRemoveCallSiteEnumAttribute :: proc(C: c.int, Idx: LLVMAttributeIndex, KindID: c.uint) ---
	LLVMRemoveCallSiteStringAttribute :: proc(C: c.int, Idx: LLVMAttributeIndex, K: cstring, KLen: c.uint) ---

	/**
	* Obtain the function type called by this instruction.
	*
	* @see llvm::CallBase::getFunctionType()
	*/
	LLVMGetCalledFunctionType :: proc(C: c.int) -> c.int ---

	/**
	* Obtain the pointer to the function invoked by this instruction.
	*
	* This expects an LLVMValueRef that corresponds to a llvm::CallInst or
	* llvm::InvokeInst.
	*
	* @see llvm::CallInst::getCalledOperand()
	* @see llvm::InvokeInst::getCalledOperand()
	*/
	LLVMGetCalledValue :: proc(Instr: c.int) -> c.int ---

	/**
	* Obtain the number of operand bundles attached to this instruction.
	*
	* This only works on llvm::CallInst and llvm::InvokeInst instructions.
	*
	* @see llvm::CallBase::getNumOperandBundles()
	*/
	LLVMGetNumOperandBundles :: proc(C: c.int) -> c.uint ---

	/**
	* Obtain the operand bundle attached to this instruction at the given index.
	* Use LLVMDisposeOperandBundle to free the operand bundle.
	*
	* This only works on llvm::CallInst and llvm::InvokeInst instructions.
	*/
	LLVMGetOperandBundleAtIndex :: proc(C: c.int, Index: c.uint) -> c.int ---

	/**
	* Obtain whether a call instruction is a tail call.
	*
	* This only works on llvm::CallInst instructions.
	*
	* @see llvm::CallInst::isTailCall()
	*/
	LLVMIsTailCall :: proc(CallInst: c.int) -> c.int ---

	/**
	* Set whether a call instruction is a tail call.
	*
	* This only works on llvm::CallInst instructions.
	*
	* @see llvm::CallInst::setTailCall()
	*/
	LLVMSetTailCall :: proc(CallInst: c.int, IsTailCall: c.int) ---

	/**
	* Obtain a tail call kind of the call instruction.
	*
	* @see llvm::CallInst::setTailCallKind()
	*/
	LLVMGetTailCallKind :: proc(CallInst: c.int) -> LLVMTailCallKind ---

	/**
	* Set the call kind of the call instruction.
	*
	* @see llvm::CallInst::getTailCallKind()
	*/
	LLVMSetTailCallKind :: proc(CallInst: c.int, kind: LLVMTailCallKind) ---

	/**
	* Return the normal destination basic block.
	*
	* This only works on llvm::InvokeInst instructions.
	*
	* @see llvm::InvokeInst::getNormalDest()
	*/
	LLVMGetNormalDest :: proc(InvokeInst: c.int) -> c.int ---

	/**
	* Return the unwind destination basic block.
	*
	* Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
	* llvm::CatchSwitchInst instructions.
	*
	* @see llvm::InvokeInst::getUnwindDest()
	* @see llvm::CleanupReturnInst::getUnwindDest()
	* @see llvm::CatchSwitchInst::getUnwindDest()
	*/
	LLVMGetUnwindDest :: proc(InvokeInst: c.int) -> c.int ---

	/**
	* Set the normal destination basic block.
	*
	* This only works on llvm::InvokeInst instructions.
	*
	* @see llvm::InvokeInst::setNormalDest()
	*/
	LLVMSetNormalDest :: proc(InvokeInst: c.int, B: c.int) ---

	/**
	* Set the unwind destination basic block.
	*
	* Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
	* llvm::CatchSwitchInst instructions.
	*
	* @see llvm::InvokeInst::setUnwindDest()
	* @see llvm::CleanupReturnInst::setUnwindDest()
	* @see llvm::CatchSwitchInst::setUnwindDest()
	*/
	LLVMSetUnwindDest :: proc(InvokeInst: c.int, B: c.int) ---

	/**
	* Return the number of successors that this terminator has.
	*
	* @see llvm::Instruction::getNumSuccessors
	*/
	LLVMGetNumSuccessors :: proc(Term: c.int) -> c.uint ---

	/**
	* Return the specified successor.
	*
	* @see llvm::Instruction::getSuccessor
	*/
	LLVMGetSuccessor :: proc(Term: c.int, i: c.uint) -> c.int ---

	/**
	* Update the specified successor to point at the provided block.
	*
	* @see llvm::Instruction::setSuccessor
	*/
	LLVMSetSuccessor :: proc(Term: c.int, i: c.uint, block: c.int) ---

	/**
	* Return if a branch is conditional.
	*
	* This only works on llvm::BranchInst instructions.
	*
	* @see llvm::BranchInst::isConditional
	*/
	LLVMIsConditional :: proc(Branch: c.int) -> c.int ---

	/**
	* Return the condition of a branch instruction.
	*
	* This only works on llvm::BranchInst instructions.
	*
	* @see llvm::BranchInst::getCondition
	*/
	LLVMGetCondition :: proc(Branch: c.int) -> c.int ---

	/**
	* Set the condition of a branch instruction.
	*
	* This only works on llvm::BranchInst instructions.
	*
	* @see llvm::BranchInst::setCondition
	*/
	LLVMSetCondition :: proc(Branch: c.int, Cond: c.int) ---

	/**
	* Obtain the default destination basic block of a switch instruction.
	*
	* This only works on llvm::SwitchInst instructions.
	*
	* @see llvm::SwitchInst::getDefaultDest()
	*/
	LLVMGetSwitchDefaultDest :: proc(SwitchInstr: c.int) -> c.int ---

	/**
	* Obtain the type that is being allocated by the alloca instruction.
	*/
	LLVMGetAllocatedType :: proc(Alloca: c.int) -> c.int ---

	/**
	* Check whether the given GEP operator is inbounds.
	*/
	LLVMIsInBounds :: proc(GEP: c.int) -> c.int ---

	/**
	* Set the given GEP instruction to be inbounds or not.
	*/
	LLVMSetIsInBounds :: proc(GEP: c.int, InBounds: c.int) ---

	/**
	* Get the source element type of the given GEP operator.
	*/
	LLVMGetGEPSourceElementType :: proc(GEP: c.int) -> c.int ---

	/**
	* Add an incoming value to the end of a PHI list.
	*/
	LLVMAddIncoming :: proc(PhiNode: c.int, IncomingValues: ^c.int, IncomingBlocks: ^c.int, Count: c.uint) ---

	/**
	* Obtain the number of incoming basic blocks to a PHI node.
	*/
	LLVMCountIncoming :: proc(PhiNode: c.int) -> c.uint ---

	/**
	* Obtain an incoming value to a PHI node as an LLVMValueRef.
	*/
	LLVMGetIncomingValue :: proc(PhiNode: c.int, Index: c.uint) -> c.int ---

	/**
	* Obtain an incoming value to a PHI node as an LLVMBasicBlockRef.
	*/
	LLVMGetIncomingBlock :: proc(PhiNode: c.int, Index: c.uint) -> c.int ---

	/**
	* Obtain the number of indices.
	* NB: This also works on GEP operators.
	*/
	LLVMGetNumIndices :: proc(Inst: c.int) -> c.uint ---

	/**
	* Obtain the indices as an array.
	*/
	LLVMGetIndices :: proc(Inst: c.int) -> ^c.uint ---

	/**
	* @defgroup LLVMCCoreInstructionBuilder Instruction Builders
	*
	* An instruction builder represents a point within a basic block and is
	* the exclusive means of building instructions using the C interface.
	*
	* @{
	*/
	LLVMCreateBuilderInContext :: proc(C: LLVMContextRef) -> LLVMBuilderRef ---
	LLVMCreateBuilder :: proc() -> c.int ---
	LLVMPositionBuilder :: proc(Builder: c.int, Block: c.int, Instr: c.int) ---
	LLVMPositionBuilderBefore :: proc(Builder: c.int, Instr: c.int) ---
	LLVMPositionBuilderAtEnd :: proc(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef) ---
	LLVMGetInsertBlock :: proc(Builder: LLVMBuilderRef) -> LLVMBasicBlockRef ---
	LLVMClearInsertionPosition :: proc(Builder: c.int) ---
	LLVMInsertIntoBuilder :: proc(Builder: c.int, Instr: c.int) ---
	LLVMInsertIntoBuilderWithName :: proc(Builder: c.int, Instr: c.int, Name: cstring) ---
	LLVMDisposeBuilder :: proc(Builder: LLVMBuilderRef) ---

	/**
	* Get location information used by debugging information.
	*
	* @see llvm::IRBuilder::getCurrentDebugLocation()
	*/
	LLVMGetCurrentDebugLocation2 :: proc(Builder: c.int) -> c.int ---

	/**
	* Set location information used by debugging information.
	*
	* To clear the location metadata of the given instruction, pass NULL to \p Loc.
	*
	* @see llvm::IRBuilder::SetCurrentDebugLocation()
	*/
	LLVMSetCurrentDebugLocation2 :: proc(Builder: c.int, Loc: c.int) ---

	/**
	* Attempts to set the debug location for the given instruction using the
	* current debug location for the given builder.  If the builder has no current
	* debug location, this function is a no-op.
	*
	* @deprecated LLVMSetInstDebugLocation is deprecated in favor of the more general
	*             LLVMAddMetadataToInst.
	*
	* @see llvm::IRBuilder::SetInstDebugLocation()
	*/
	LLVMSetInstDebugLocation :: proc(Builder: c.int, Inst: c.int) ---

	/**
	* Adds the metadata registered with the given builder to the given instruction.
	*
	* @see llvm::IRBuilder::AddMetadataToInst()
	*/
	LLVMAddMetadataToInst :: proc(Builder: c.int, Inst: c.int) ---

	/**
	* Get the dafult floating-point math metadata for a given builder.
	*
	* @see llvm::IRBuilder::getDefaultFPMathTag()
	*/
	LLVMBuilderGetDefaultFPMathTag :: proc(Builder: c.int) -> c.int ---

	/**
	* Set the default floating-point math metadata for the given builder.
	*
	* To clear the metadata, pass NULL to \p FPMathTag.
	*
	* @see llvm::IRBuilder::setDefaultFPMathTag()
	*/
	LLVMBuilderSetDefaultFPMathTag :: proc(Builder: c.int, FPMathTag: c.int) ---

	/**
	* Deprecated: Passing the NULL location will crash.
	* Use LLVMGetCurrentDebugLocation2 instead.
	*/
	LLVMSetCurrentDebugLocation :: proc(Builder: c.int, L: c.int) ---

	/**
	* Deprecated: Returning the NULL location will crash.
	* Use LLVMGetCurrentDebugLocation2 instead.
	*/
	LLVMGetCurrentDebugLocation :: proc(Builder: c.int) -> c.int ---

	/* Terminators */
	LLVMBuildRetVoid :: proc(Builder: LLVMBuilderRef) -> LLVMValueRef ---
	LLVMBuildRet :: proc(Builder: LLVMBuilderRef, V: LLVMValueRef) -> LLVMValueRef ---
	LLVMBuildAggregateRet :: proc(Builder: LLVMBuilderRef, RetVals: ^LLVMValueRef, N: c.uint) -> LLVMValueRef ---
	LLVMBuildBr :: proc(Builder: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef ---
	LLVMBuildCondBr :: proc(Builder: LLVMBuilderRef, If: LLVMValueRef, Then: LLVMBasicBlockRef, Else: LLVMBasicBlockRef) -> LLVMValueRef ---
	LLVMBuildSwitch :: proc(Builder: LLVMBuilderRef, V: LLVMValueRef, Else: LLVMBasicBlockRef, NumCases: c.uint) -> LLVMValueRef ---
	LLVMBuildIndirectBr :: proc(B: c.int, Addr: c.int, NumDests: c.uint) -> c.int ---
	LLVMBuildInvoke2 :: proc() -> c.int ---
	LLVMBuildInvokeWithOperandBundles :: proc() -> c.int ---
	LLVMBuildUnreachable :: proc() -> c.int ---

	/* Exception Handling */
	LLVMBuildResume :: proc(B: c.int, Exn: c.int) -> c.int ---
	LLVMBuildLandingPad :: proc(B: c.int, Ty: c.int, PersFn: c.int, NumClauses: c.uint, Name: cstring) -> c.int ---
	LLVMBuildCleanupRet :: proc(B: c.int, CatchPad: c.int, BB: c.int) -> c.int ---
	LLVMBuildCatchRet :: proc(B: c.int, CatchPad: c.int, BB: c.int) -> c.int ---
	LLVMBuildCatchPad :: proc(B: c.int, ParentPad: c.int, Args: ^c.int, NumArgs: c.uint, Name: cstring) -> c.int ---
	LLVMBuildCleanupPad :: proc(B: c.int, ParentPad: c.int, Args: ^c.int, NumArgs: c.uint, Name: cstring) -> c.int ---
	LLVMBuildCatchSwitch :: proc(B: c.int, ParentPad: c.int, UnwindBB: c.int, NumHandlers: c.uint, Name: cstring) -> c.int ---

	/* Add a case to the switch instruction */
	LLVMAddCase :: proc(Switch: c.int, OnVal: c.int, Dest: c.int) ---

	/* Add a destination to the indirectbr instruction */
	LLVMAddDestination :: proc(IndirectBr: c.int, Dest: c.int) ---

	/* Get the number of clauses on the landingpad instruction */
	LLVMGetNumClauses :: proc(LandingPad: c.int) -> c.uint ---

	/* Get the value of the clause at index Idx on the landingpad instruction */
	LLVMGetClause :: proc(LandingPad: c.int, Idx: c.uint) -> c.int ---

	/* Add a catch or filter clause to the landingpad instruction */
	LLVMAddClause :: proc(LandingPad: c.int, ClauseVal: c.int) ---

	/* Get the 'cleanup' flag in the landingpad instruction */
	LLVMIsCleanup :: proc(LandingPad: c.int) -> c.int ---

	/* Set the 'cleanup' flag in the landingpad instruction */
	LLVMSetCleanup :: proc(LandingPad: c.int, Val: c.int) ---

	/* Add a destination to the catchswitch instruction */
	LLVMAddHandler :: proc(CatchSwitch: c.int, Dest: c.int) ---

	/* Get the number of handlers on the catchswitch instruction */
	LLVMGetNumHandlers :: proc(CatchSwitch: c.int) -> c.uint ---

	/**
	* Obtain the basic blocks acting as handlers for a catchswitch instruction.
	*
	* The Handlers parameter should point to a pre-allocated array of
	* LLVMBasicBlockRefs at least LLVMGetNumHandlers() large. On return, the
	* first LLVMGetNumHandlers() entries in the array will be populated
	* with LLVMBasicBlockRef instances.
	*
	* @param CatchSwitch The catchswitch instruction to operate on.
	* @param Handlers Memory address of an array to be filled with basic blocks.
	*/
	LLVMGetHandlers :: proc(CatchSwitch: c.int, Handlers: ^c.int) ---

	/* Get the number of funcletpad arguments. */
	LLVMGetArgOperand :: proc(Funclet: c.int, i: c.uint) -> c.int ---

	/* Set a funcletpad argument at the given index. */
	LLVMSetArgOperand :: proc(Funclet: c.int, i: c.uint, value: c.int) ---

	/**
	* Get the parent catchswitch instruction of a catchpad instruction.
	*
	* This only works on llvm::CatchPadInst instructions.
	*
	* @see llvm::CatchPadInst::getCatchSwitch()
	*/
	LLVMGetParentCatchSwitch :: proc(CatchPad: c.int) -> c.int ---

	/**
	* Set the parent catchswitch instruction of a catchpad instruction.
	*
	* This only works on llvm::CatchPadInst instructions.
	*
	* @see llvm::CatchPadInst::setCatchSwitch()
	*/
	LLVMSetParentCatchSwitch :: proc(CatchPad: c.int, CatchSwitch: c.int) ---

	/* Arithmetic */
	LLVMBuildAdd :: proc(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildNSWAdd :: proc() -> c.int ---
	LLVMBuildNUWAdd :: proc() -> c.int ---
	LLVMBuildFAdd :: proc() -> c.int ---
	LLVMBuildSub :: proc(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildNSWSub :: proc() -> c.int ---
	LLVMBuildNUWSub :: proc() -> c.int ---
	LLVMBuildFSub :: proc() -> c.int ---
	LLVMBuildMul :: proc(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildNSWMul :: proc() -> c.int ---
	LLVMBuildNUWMul :: proc() -> c.int ---
	LLVMBuildFMul :: proc() -> c.int ---
	LLVMBuildUDiv :: proc() -> c.int ---
	LLVMBuildExactUDiv :: proc() -> c.int ---
	LLVMBuildSDiv :: proc(Builder: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildExactSDiv :: proc() -> c.int ---
	LLVMBuildFDiv :: proc() -> c.int ---
	LLVMBuildURem :: proc() -> c.int ---
	LLVMBuildSRem :: proc() -> c.int ---
	LLVMBuildFRem :: proc() -> c.int ---
	LLVMBuildShl :: proc() -> c.int ---
	LLVMBuildLShr :: proc() -> c.int ---
	LLVMBuildAShr :: proc() -> c.int ---
	LLVMBuildAnd :: proc() -> c.int ---
	LLVMBuildOr :: proc() -> c.int ---
	LLVMBuildXor :: proc() -> c.int ---
	LLVMBuildBinOp :: proc(B: c.int, Op: LLVMOpcode, LHS: c.int, RHS: c.int, Name: cstring) -> c.int ---
	LLVMBuildNeg :: proc() -> c.int ---
	LLVMBuildNSWNeg :: proc(B: c.int, V: c.int, Name: cstring) -> c.int ---
	LLVMBuildNUWNeg :: proc(B: c.int, V: c.int, Name: cstring) -> c.int ---
	LLVMBuildFNeg :: proc() -> c.int ---
	LLVMBuildNot :: proc() -> c.int ---
	LLVMGetNUW :: proc(ArithInst: c.int) -> c.int ---
	LLVMSetNUW :: proc(ArithInst: c.int, HasNUW: c.int) ---
	LLVMGetNSW :: proc(ArithInst: c.int) -> c.int ---
	LLVMSetNSW :: proc(ArithInst: c.int, HasNSW: c.int) ---
	LLVMGetExact :: proc(DivOrShrInst: c.int) -> c.int ---
	LLVMSetExact :: proc(DivOrShrInst: c.int, IsExact: c.int) ---

	/**
	* Gets if the instruction has the non-negative flag set.
	* Only valid for zext instructions.
	*/
	LLVMGetNNeg :: proc(NonNegInst: c.int) -> c.int ---

	/**
	* Sets the non-negative flag for the instruction.
	* Only valid for zext instructions.
	*/
	LLVMSetNNeg :: proc(NonNegInst: c.int, IsNonNeg: c.int) ---

	/**
	* Get the flags for which fast-math-style optimizations are allowed for this
	* value.
	*
	* Only valid on floating point instructions.
	* @see LLVMCanValueUseFastMathFlags
	*/
	LLVMGetFastMathFlags :: proc(FPMathInst: c.int) -> LLVMFastMathFlags ---

	/**
	* Sets the flags for which fast-math-style optimizations are allowed for this
	* value.
	*
	* Only valid on floating point instructions.
	* @see LLVMCanValueUseFastMathFlags
	*/
	LLVMSetFastMathFlags :: proc(FPMathInst: c.int, FMF: LLVMFastMathFlags) ---

	/**
	* Check if a given value can potentially have fast math flags.
	*
	* Will return true for floating point arithmetic instructions, and for select,
	* phi, and call instructions whose type is a floating point type, or a vector
	* or array thereof. See https://llvm.org/docs/LangRef.html#fast-math-flags
	*/
	LLVMCanValueUseFastMathFlags :: proc(Inst: c.int) -> c.int ---

	/**
	* Gets whether the instruction has the disjoint flag set.
	* Only valid for or instructions.
	*/
	LLVMGetIsDisjoint :: proc(Inst: c.int) -> c.int ---

	/**
	* Sets the disjoint flag for the instruction.
	* Only valid for or instructions.
	*/
	LLVMSetIsDisjoint :: proc(Inst: c.int, IsDisjoint: c.int) ---

	/* Memory */
	LLVMBuildMalloc :: proc() -> c.int ---
	LLVMBuildArrayMalloc :: proc() -> c.int ---

	/**
	* Creates and inserts a memset to the specified pointer and the
	* specified value.
	*
	* @see llvm::IRRBuilder::CreateMemSet()
	*/
	LLVMBuildMemSet :: proc(B: c.int, Ptr: c.int, Val: c.int, Len: c.int, Align: c.uint) -> c.int ---

	/**
	* Creates and inserts a memcpy between the specified pointers.
	*
	* @see llvm::IRRBuilder::CreateMemCpy()
	*/
	LLVMBuildMemCpy :: proc(B: c.int, Dst: c.int, DstAlign: c.uint, Src: c.int, SrcAlign: c.uint, Size: c.int) -> c.int ---

	/**
	* Creates and inserts a memmove between the specified pointers.
	*
	* @see llvm::IRRBuilder::CreateMemMove()
	*/
	LLVMBuildMemMove :: proc(B: c.int, Dst: c.int, DstAlign: c.uint, Src: c.int, SrcAlign: c.uint, Size: c.int) -> c.int ---
	LLVMBuildAlloca :: proc(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildArrayAlloca :: proc() -> c.int ---
	LLVMBuildFree :: proc() -> c.int ---
	LLVMBuildLoad2 :: proc(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, PointerVal: LLVMValueRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildStore :: proc(Builder: LLVMBuilderRef, Val: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef ---
	LLVMBuildGEP2 :: proc(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Indices: ^LLVMValueRef, NumIndices: c.uint, Name: cstring) -> LLVMValueRef ---
	LLVMBuildInBoundsGEP2 :: proc(B: c.int, Ty: c.int, Pointer: c.int, Indices: ^c.int, NumIndices: c.uint, Name: cstring) -> c.int ---
	LLVMBuildStructGEP2 :: proc(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Idx: c.uint, Name: cstring) -> LLVMValueRef ---
	LLVMBuildGlobalString :: proc(B: c.int, Str: cstring, Name: cstring) -> c.int ---
	LLVMBuildGlobalStringPtr :: proc(B: c.int, Str: cstring, Name: cstring) -> c.int ---
	LLVMGetVolatile :: proc(MemoryAccessInst: c.int) -> c.int ---
	LLVMSetVolatile :: proc(MemoryAccessInst: c.int, IsVolatile: c.int) ---
	LLVMGetWeak :: proc(CmpXchgInst: c.int) -> c.int ---
	LLVMSetWeak :: proc(CmpXchgInst: c.int, IsWeak: c.int) ---
	LLVMGetOrdering :: proc(MemoryAccessInst: c.int) -> LLVMAtomicOrdering ---
	LLVMSetOrdering :: proc(MemoryAccessInst: c.int, Ordering: LLVMAtomicOrdering) ---
	LLVMGetAtomicRMWBinOp :: proc(AtomicRMWInst: c.int) -> LLVMAtomicRMWBinOp ---
	LLVMSetAtomicRMWBinOp :: proc(AtomicRMWInst: c.int, BinOp: LLVMAtomicRMWBinOp) ---

	/* Casts */
	LLVMBuildTrunc :: proc() -> c.int ---
	LLVMBuildZExt :: proc() -> c.int ---
	LLVMBuildSExt :: proc() -> c.int ---
	LLVMBuildFPToUI :: proc() -> c.int ---
	LLVMBuildFPToSI :: proc() -> c.int ---
	LLVMBuildUIToFP :: proc() -> c.int ---
	LLVMBuildSIToFP :: proc() -> c.int ---
	LLVMBuildFPTrunc :: proc() -> c.int ---
	LLVMBuildFPExt :: proc() -> c.int ---
	LLVMBuildPtrToInt :: proc() -> c.int ---
	LLVMBuildIntToPtr :: proc() -> c.int ---
	LLVMBuildBitCast :: proc() -> c.int ---
	LLVMBuildAddrSpaceCast :: proc() -> c.int ---
	LLVMBuildZExtOrBitCast :: proc() -> c.int ---
	LLVMBuildSExtOrBitCast :: proc() -> c.int ---
	LLVMBuildTruncOrBitCast :: proc() -> c.int ---
	LLVMBuildCast :: proc(B: c.int, Op: LLVMOpcode, Val: c.int, DestTy: c.int, Name: cstring) -> c.int ---
	LLVMBuildPointerCast :: proc(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: cstring) -> LLVMValueRef ---
	LLVMBuildIntCast2 :: proc() -> c.int ---
	LLVMBuildFPCast :: proc() -> c.int ---

	/** Deprecated: This cast is always signed. Use LLVMBuildIntCast2 instead. */
	LLVMBuildIntCast :: proc() -> c.int --- /*Signed cast!*/
	LLVMGetCastOpcode :: proc(Src: c.int, SrcIsSigned: c.int, DestTy: c.int, DestIsSigned: c.int) -> LLVMOpcode ---

	/* Comparisons */
	LLVMBuildICmp :: proc() -> c.int ---
	LLVMBuildFCmp :: proc() -> c.int ---

	/* Miscellaneous instructions */
	LLVMBuildPhi :: proc() -> c.int ---
	LLVMBuildCall2 :: proc(Builder: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: ^LLVMValueRef, NumArgs: c.uint, Name: cstring) -> LLVMValueRef ---
	LLVMBuildCallWithOperandBundles :: proc() -> c.int ---
	LLVMBuildSelect :: proc() -> c.int ---
	LLVMBuildVAArg :: proc() -> c.int ---
	LLVMBuildExtractElement :: proc() -> c.int ---
	LLVMBuildInsertElement :: proc() -> c.int ---
	LLVMBuildShuffleVector :: proc() -> c.int ---
	LLVMBuildExtractValue :: proc() -> c.int ---
	LLVMBuildInsertValue :: proc() -> c.int ---
	LLVMBuildFreeze :: proc() -> c.int ---
	LLVMBuildIsNull :: proc() -> c.int ---
	LLVMBuildIsNotNull :: proc() -> c.int ---
	LLVMBuildPtrDiff2 :: proc() -> c.int ---
	LLVMBuildFence :: proc(B: c.int, ordering: LLVMAtomicOrdering, singleThread: c.int, Name: cstring) -> c.int ---
	LLVMBuildAtomicRMW :: proc(B: c.int, op: LLVMAtomicRMWBinOp, PTR: c.int, Val: c.int, ordering: LLVMAtomicOrdering, singleThread: c.int) -> c.int ---
	LLVMBuildAtomicCmpXchg :: proc(B: c.int, Ptr: c.int, Cmp: c.int, New: c.int, SuccessOrdering: LLVMAtomicOrdering, FailureOrdering: LLVMAtomicOrdering, SingleThread: c.int) -> c.int ---

	/**
	* Get the number of elements in the mask of a ShuffleVector instruction.
	*/
	LLVMGetNumMaskElements :: proc(ShuffleVectorInst: c.int) -> c.uint ---

	/**
	* \returns a constant that specifies that the result of a \c ShuffleVectorInst
	* is undefined.
	*/
	LLVMGetUndefMaskElem :: proc() -> c.int ---

	/**
	* Get the mask value at position Elt in the mask of a ShuffleVector
	* instruction.
	*
	* \Returns the result of \c LLVMGetUndefMaskElem() if the mask value is
	* poison at that position.
	*/
	LLVMGetMaskValue :: proc(ShuffleVectorInst: c.int, Elt: c.uint) -> c.int ---
	LLVMIsAtomicSingleThread :: proc(AtomicInst: c.int) -> c.int ---
	LLVMSetAtomicSingleThread :: proc(AtomicInst: c.int, SingleThread: c.int) ---
	LLVMGetCmpXchgSuccessOrdering :: proc(CmpXchgInst: c.int) -> LLVMAtomicOrdering ---
	LLVMSetCmpXchgSuccessOrdering :: proc(CmpXchgInst: c.int, Ordering: LLVMAtomicOrdering) ---
	LLVMGetCmpXchgFailureOrdering :: proc(CmpXchgInst: c.int) -> LLVMAtomicOrdering ---
	LLVMSetCmpXchgFailureOrdering :: proc(CmpXchgInst: c.int, Ordering: LLVMAtomicOrdering) ---

	/**
	* Changes the type of M so it can be passed to FunctionPassManagers and the
	* JIT.  They take ModuleProviders for historical reasons.
	*/
	LLVMCreateModuleProviderForExistingModule :: proc(M: c.int) -> c.int ---

	/**
	* Destroys the module M.
	*/
	LLVMDisposeModuleProvider :: proc(M: c.int) ---

	/**
	* @defgroup LLVMCCoreMemoryBuffers Memory Buffers
	*
	* @{
	*/
	LLVMCreateMemoryBufferWithContentsOfFile :: proc(Path: cstring, OutMemBuf: ^c.int, OutMessage: [^]cstring) -> c.int ---
	LLVMCreateMemoryBufferWithSTDIN :: proc(OutMemBuf: ^c.int, OutMessage: [^]cstring) -> c.int ---
	LLVMCreateMemoryBufferWithMemoryRange :: proc(InputData: cstring, InputDataLength: c.int, BufferName: cstring, RequiresNullTerminator: c.int) -> c.int ---
	LLVMCreateMemoryBufferWithMemoryRangeCopy :: proc(InputData: cstring, InputDataLength: c.int, BufferName: cstring) -> c.int ---
	LLVMGetBufferStart :: proc(MemBuf: c.int) -> cstring ---
	LLVMGetBufferSize :: proc(MemBuf: c.int) -> c.int ---
	LLVMDisposeMemoryBuffer :: proc(MemBuf: c.int) ---

	/** Constructs a new whole-module pass pipeline. This type of pipeline is
	suitable for link-time optimization and whole-module transformations.
	@see llvm::PassManager::PassManager */
	LLVMCreatePassManager :: proc() -> c.int ---

	/** Constructs a new function-by-function pass pipeline over the module
	provider. It does not take ownership of the module provider. This type of
	pipeline is suitable for code generation and JIT compilation tasks.
	@see llvm::FunctionPassManager::FunctionPassManager */
	LLVMCreateFunctionPassManagerForModule :: proc(M: c.int) -> c.int ---

	/** Deprecated: Use LLVMCreateFunctionPassManagerForModule instead. */
	LLVMCreateFunctionPassManager :: proc(MP: c.int) -> c.int ---

	/** Initializes, executes on the provided module, and finalizes all of the
	passes scheduled in the pass manager. Returns 1 if any of the passes
	modified the module, 0 otherwise.
	@see llvm::PassManager::run(Module&) */
	LLVMRunPassManager :: proc(PM: c.int, M: c.int) -> c.int ---

	/** Initializes all of the function passes scheduled in the function pass
	manager. Returns 1 if any of the passes modified the module, 0 otherwise.
	@see llvm::FunctionPassManager::doInitialization */
	LLVMInitializeFunctionPassManager :: proc(FPM: c.int) -> c.int ---

	/** Executes all of the function passes scheduled in the function pass manager
	on the provided function. Returns 1 if any of the passes modified the
	function, false otherwise.
	@see llvm::FunctionPassManager::run(Function&) */
	LLVMRunFunctionPassManager :: proc(FPM: c.int, F: c.int) -> c.int ---

	/** Finalizes all of the function passes scheduled in the function pass
	manager. Returns 1 if any of the passes modified the module, 0 otherwise.
	@see llvm::FunctionPassManager::doFinalization */
	LLVMFinalizeFunctionPassManager :: proc(FPM: c.int) -> c.int ---

	/** Frees the memory of a pass pipeline. For function pipelines, does not free
	the module provider.
	@see llvm::PassManagerBase::~PassManagerBase. */
	LLVMDisposePassManager :: proc(PM: c.int) ---

	/** Deprecated: Multi-threading can only be enabled/disabled with the compile
	time define LLVM_ENABLE_THREADS.  This function always returns
	LLVMIsMultithreaded(). */
	LLVMStartMultithreaded :: proc() -> c.int ---

	/** Deprecated: Multi-threading can only be enabled/disabled with the compile
	time define LLVM_ENABLE_THREADS. */
	LLVMStopMultithreaded :: proc() ---

	/** Check whether LLVM is executing in thread-safe mode or not.
	@see llvm::llvm_is_multithreaded */
	LLVMIsMultithreaded :: proc() -> c.int ---
}
