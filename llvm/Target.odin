package llvm
/*===-- llvm-c/Target.h - Target Lib C Iface --------------------*- C++ -*-===*/
/*                                                                            */
/* Part of the LLVM Project, under the Apache License v2.0 with LLVM          */
/* Exceptions.                                                                */
/* See https://llvm.org/LICENSE.txt for license information.                  */
/* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    */
/*                                                                            */
/*===----------------------------------------------------------------------===*/
/*                                                                            */
/* This header declares the C interface to libLLVMTarget.a, which             */
/* implements target information.                                             */
/*                                                                            */
/* Many exotic languages can interoperate with C code but have a harder time  */
/* with C++ due to name mangling. So in addition to C, this interface enables */
/* tools written in such languages.                                           */
/*                                                                            */
/*===----------------------------------------------------------------------===*/
import "core:c"

_ :: c

when ODIN_OS == .Linux {
	foreign import lib "system:LLVM-18"
} else when ODIN_OS == .Windows {
	foreign import lib "LLVM-C.lib"
} else when ODIN_OS == .Darwin {
	foreign import lib "system:LLVM"
}


// LLVM_C_TARGET_H ::

LLVMTargetDataRef :: distinct rawptr

LLVMTargetLibraryInfoRef :: distinct rawptr

/* Declare all of the target-initialization functions that are available. */
// LLVM_TARGET :: (TargetName)LLVMInitialize##TargetName##TargetInfo();

/* Explicit undef to make SWIG happier */
// LLVM_TARGET :: (TargetName)LLVMInitialize##TargetName##Target();

/* Explicit undef to make SWIG happier */
// LLVM_TARGET :: (TargetName)LLVMInitialize##TargetName##TargetMC();

/* Explicit undef to make SWIG happier */

/* Declare all of the available assembly printer initialization functions. */
// LLVM_ASM_PRINTER :: (TargetName)LLVMInitialize##TargetName##AsmPrinter();

/* Explicit undef to make SWIG happier */

/* Declare all of the available assembly parser initialization functions. */
// LLVM_ASM_PARSER :: (TargetName)LLVMInitialize##TargetName##AsmParser();

/* Explicit undef to make SWIG happier */

/* Declare all of the available disassembler initialization functions. */
// LLVM_DISASSEMBLER :: (TargetName)LLVMInitialize##TargetName##Disassembler();

// LLVM_TARGET :: (TargetName)LLVMInitialize##TargetName##TargetInfo();

// LLVM_TARGET :: (TargetName)LLVMInitialize##TargetName##Target();

// LLVM_TARGET :: (TargetName)LLVMInitialize##TargetName##TargetMC();

// LLVM_ASM_PRINTER :: (TargetName)LLVMInitialize##TargetName##AsmPrinter();

// LLVM_ASM_PARSER :: (TargetName)LLVMInitialize##TargetName##AsmParser();

// LLVM_DISASSEMBLER :: (TargetName)LLVMInitialize##TargetName##Disassembler();

@(default_calling_convention = "c", link_prefix = "")
foreign lib {
	/**
	* Obtain the data layout for a module.
	*
	* @see Module::getDataLayout()
	*/
	LLVMGetModuleDataLayout :: proc(M: c.int) -> LLVMTargetDataRef ---

	/**
	* Set the data layout for a module.
	*
	* @see Module::setDataLayout()
	*/
	LLVMSetModuleDataLayout :: proc(M: c.int, DL: LLVMTargetDataRef) ---

	/** Creates target data from a target layout string.
	See the constructor llvm::DataLayout::DataLayout. */
	LLVMCreateTargetData :: proc(StringRep: cstring) -> LLVMTargetDataRef ---

	/** Deallocates a TargetData.
	See the destructor llvm::DataLayout::~DataLayout. */
	LLVMDisposeTargetData :: proc(TD: LLVMTargetDataRef) ---

	/** Adds target library information to a pass manager. This does not take
	ownership of the target library info.
	See the method llvm::PassManagerBase::add. */
	LLVMAddTargetLibraryInfo :: proc(TLI: LLVMTargetLibraryInfoRef, PM: c.int) ---

	/** Converts target data to a target layout string. The string must be disposed
	with LLVMDisposeMessage.
	See the constructor llvm::DataLayout::DataLayout. */
	LLVMCopyStringRepOfTargetData :: proc(TD: LLVMTargetDataRef) -> cstring ---

	/** Returns the byte order of a target, either LLVMBigEndian or
	LLVMLittleEndian.
	See the method llvm::DataLayout::isLittleEndian. */
	LLVMByteOrder :: proc(TD: LLVMTargetDataRef) -> LLVMByteOrdering ---

	/** Returns the pointer size in bytes for a target.
	See the method llvm::DataLayout::getPointerSize. */
	LLVMPointerSize :: proc(TD: LLVMTargetDataRef) -> c.uint ---

	/** Returns the pointer size in bytes for a target for a specified
	address space.
	See the method llvm::DataLayout::getPointerSize. */
	LLVMPointerSizeForAS :: proc(TD: LLVMTargetDataRef, AS: c.uint) -> c.uint ---

	/** Returns the integer type that is the same size as a pointer on a target.
	See the method llvm::DataLayout::getIntPtrType. */
	LLVMIntPtrType :: proc(TD: LLVMTargetDataRef) -> c.int ---

	/** Returns the integer type that is the same size as a pointer on a target.
	This version allows the address space to be specified.
	See the method llvm::DataLayout::getIntPtrType. */
	LLVMIntPtrTypeForAS :: proc(TD: LLVMTargetDataRef, AS: c.uint) -> c.int ---

	/** Returns the integer type that is the same size as a pointer on a target.
	See the method llvm::DataLayout::getIntPtrType. */
	LLVMIntPtrTypeInContext :: proc(C: c.int, TD: LLVMTargetDataRef) -> c.int ---

	/** Returns the integer type that is the same size as a pointer on a target.
	This version allows the address space to be specified.
	See the method llvm::DataLayout::getIntPtrType. */
	LLVMIntPtrTypeForASInContext :: proc(C: c.int, TD: LLVMTargetDataRef, AS: c.uint) -> c.int ---

	/** Computes the size of a type in bytes for a target.
	See the method llvm::DataLayout::getTypeSizeInBits. */
	LLVMSizeOfTypeInBits :: proc(TD: LLVMTargetDataRef, Ty: c.int) -> c.ulonglong ---

	/** Computes the storage size of a type in bytes for a target.
	See the method llvm::DataLayout::getTypeStoreSize. */
	LLVMStoreSizeOfType :: proc(TD: LLVMTargetDataRef, Ty: c.int) -> c.ulonglong ---

	/** Computes the ABI size of a type in bytes for a target.
	See the method llvm::DataLayout::getTypeAllocSize. */
	LLVMABISizeOfType :: proc(TD: LLVMTargetDataRef, Ty: c.int) -> c.ulonglong ---

	/** Computes the ABI alignment of a type in bytes for a target.
	See the method llvm::DataLayout::getTypeABISize. */
	LLVMABIAlignmentOfType :: proc(TD: LLVMTargetDataRef, Ty: c.int) -> c.uint ---

	/** Computes the call frame alignment of a type in bytes for a target.
	See the method llvm::DataLayout::getTypeABISize. */
	LLVMCallFrameAlignmentOfType :: proc(TD: LLVMTargetDataRef, Ty: c.int) -> c.uint ---

	/** Computes the preferred alignment of a type in bytes for a target.
	See the method llvm::DataLayout::getTypeABISize. */
	LLVMPreferredAlignmentOfType :: proc(TD: LLVMTargetDataRef, Ty: c.int) -> c.uint ---

	/** Computes the preferred alignment of a global variable in bytes for a target.
	See the method llvm::DataLayout::getPreferredAlignment. */
	LLVMPreferredAlignmentOfGlobal :: proc(TD: LLVMTargetDataRef, GlobalVar: c.int) -> c.uint ---

	/** Computes the structure element that contains the byte offset for a target.
	See the method llvm::StructLayout::getElementContainingOffset. */
	LLVMElementAtOffset :: proc(TD: LLVMTargetDataRef, StructTy: c.int, Offset: c.ulonglong) -> c.uint ---

	/** Computes the byte offset of the indexed struct element for a target.
	See the method llvm::StructLayout::getElementContainingOffset. */
	LLVMOffsetOfElement :: proc(TD: LLVMTargetDataRef, StructTy: c.int, Element: c.uint) -> c.ulonglong ---
}
