package llvm

import "core:c"

/*===-- llvm-c/Support.h - C Interface Types declarations ---------*- C -*-===*\
|*                                                                            *|
|* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
|* Exceptions.                                                                *|
|* See https://llvm.org/LICENSE.txt for license information.                  *|
|* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
|*                                                                            *|
|*===----------------------------------------------------------------------===*|
|*                                                                            *|
|* This file defines types used by the C interface to LLVM.                   *|
|*                                                                            *|
\*===----------------------------------------------------------------------===*/
// LLVM_C_TYPES_H ::

LLVMBool :: c.int

// Missing enum that bindgen didn't generate
LLVMByteOrdering :: enum c.int {
	BigEndian    = 0,
	LittleEndian = 1,
}

/**
* Used to pass regions of memory through LLVM interfaces.
*
* @see llvm::MemoryBuffer
*/
LLVMMemoryBufferRef :: distinct rawptr

/**
* The top-level container for all LLVM global data. See the LLVMContext class.
*/
LLVMContextRef :: distinct rawptr

/**
* The top-level container for all other LLVM Intermediate Representation (IR)
* objects.
*
* @see llvm::Module
*/
LLVMModuleRef :: distinct rawptr

/**
* Each value in the LLVM IR has a type, an LLVMTypeRef.
*
* @see llvm::Type
*/
LLVMTypeRef :: distinct rawptr

/**
* Represents an individual value in LLVM IR.
*
* This models llvm::Value.
*/
LLVMValueRef :: distinct rawptr

/**
* Represents a basic block of instructions in LLVM IR.
*
* This models llvm::BasicBlock.
*/
LLVMBasicBlockRef :: distinct rawptr

/**
* Represents an LLVM Metadata.
*
* This models llvm::Metadata.
*/
LLVMMetadataRef :: distinct rawptr

/**
* Represents an LLVM Named Metadata Node.
*
* This models llvm::NamedMDNode.
*/
LLVMNamedMDNodeRef :: distinct rawptr

/**
* Represents an entry in a Global Object's metadata attachments.
*
* This models std::pair<unsigned, MDNode *>
*/
LLVMValueMetadataEntry :: distinct rawptr

/**
* Represents an LLVM basic block builder.
*
* This models llvm::IRBuilder.
*/
LLVMBuilderRef :: distinct rawptr

/**
* Represents an LLVM debug info builder.
*
* This models llvm::DIBuilder.
*/
LLVMDIBuilderRef :: distinct rawptr

/**
* Interface used to provide a module to JIT or interpreter.
* This is now just a synonym for llvm::Module, but we have to keep using the
* different type to keep binary compatibility.
*/
LLVMModuleProviderRef :: distinct rawptr

/** @see llvm::PassManagerBase */
LLVMPassManagerRef :: distinct rawptr

/**
* Used to get the users and usees of a Value.
*
* @see llvm::Use */
LLVMUseRef :: distinct rawptr

/**
* @see llvm::OperandBundleDef
*/
LLVMOperandBundleRef :: distinct rawptr

/**
* Used to represent an attributes.
*
* @see llvm::Attribute
*/
LLVMAttributeRef :: distinct rawptr

/**
* @see llvm::DiagnosticInfo
*/
LLVMDiagnosticInfoRef :: distinct rawptr

/**
* @see llvm::Comdat
*/
LLVMComdatRef :: distinct rawptr

/**
* @see llvm::Module::ModuleFlagEntry
*/
LLVMModuleFlagEntry :: distinct rawptr

/**
* @see llvm::JITEventListener
*/
LLVMJITEventListenerRef :: distinct rawptr

/**
* @see llvm::object::Binary
*/
LLVMBinaryRef :: distinct rawptr
