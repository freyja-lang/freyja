// Generated from LLVM Attributes.inc

package llvm

import "core:c"
// WARNING: LLVM enum indices are not stable across versions; use the string helper below
LLVMAttributeKind :: enum u32 {
	ALLOC_ALIGN                       = 1, // allocalign
	ALLOCATED_POINTER                 = 2, // allocptr
	ALWAYS_INLINE                     = 3, // alwaysinline
	BUILTIN                           = 4, // builtin
	COLD                              = 5, // cold
	CONVERGENT                        = 6, // convergent
	CORO_DESTROY_ONLY_WHEN_COMPLETE   = 7, // coro_only_destroy_when_complete
	CORO_ELIDE_SAFE                   = 8, // coro_elide_safe
	DEAD_ON_RETURN                    = 9, // dead_on_return
	DEAD_ON_UNWIND                    = 10, // dead_on_unwind
	DISABLE_SANITIZER_INSTRUMENTATION = 11, // disable_sanitizer_instrumentation
	FN_RET_THUNK_EXTERN               = 12, // fn_ret_thunk_extern
	HOT                               = 13, // hot
	HYBRID_PATCHABLE                  = 14, // hybrid_patchable
	IMM_ARG                           = 15, // immarg
	IN_REG                            = 16, // inreg
	INLINE_HINT                       = 17, // inlinehint
	JUMP_TABLE                        = 18, // jumptable
	MIN_SIZE                          = 19, // minsize
	MUST_PROGRESS                     = 20, // mustprogress
	NAKED                             = 21, // naked
	NEST                              = 22, // nest
	NO_ALIAS                          = 23, // noalias
	NO_BUILTIN                        = 24, // nobuiltin
	NO_CALLBACK                       = 25, // nocallback
	NO_CF_CHECK                       = 26, // nocf_check
	NO_DIVERGENCE_SOURCE              = 27, // nodivergencesource
	NO_DUPLICATE                      = 28, // noduplicate
	NO_EXT                            = 29, // noext
	NO_FREE                           = 30, // nofree
	NO_IMPLICIT_FLOAT                 = 31, // noimplicitfloat
	NO_INLINE                         = 32, // noinline
	NO_MERGE                          = 33, // nomerge
	NO_PROFILE                        = 34, // noprofile
	NO_RECURSE                        = 35, // norecurse
	NO_RED_ZONE                       = 36, // noredzone
	NO_RETURN                         = 37, // noreturn
	NO_SANITIZE_BOUNDS                = 38, // nosanitize_bounds
	NO_SANITIZE_COVERAGE              = 39, // nosanitize_coverage
	NO_SYNC                           = 40, // nosync
	NO_UNDEF                          = 41, // noundef
	NO_UNWIND                         = 42, // nounwind
	NON_LAZY_BIND                     = 43, // nonlazybind
	NON_NULL                          = 44, // nonnull
	NULL_POINTER_IS_VALID             = 45, // null_pointer_is_valid
	OPT_FOR_FUZZING                   = 46, // optforfuzzing
	OPTIMIZE_FOR_DEBUGGING            = 47, // optdebug
	OPTIMIZE_FOR_SIZE                 = 48, // optsize
	OPTIMIZE_NONE                     = 49, // optnone
	PRESPLIT_COROUTINE                = 50, // presplitcoroutine
	READ_NONE                         = 51, // readnone
	READ_ONLY                         = 52, // readonly
	RETURNED                          = 53, // returned
	RETURNS_TWICE                     = 54, // returns_twice
	SEXT                              = 55, // signext
	SAFE_STACK                        = 56, // safestack
	SANITIZE_ADDRESS                  = 57, // sanitize_address
	SANITIZE_HWADDRESS                = 58, // sanitize_hwaddress
	SANITIZE_MEM_TAG                  = 59, // sanitize_memtag
	SANITIZE_MEMORY                   = 60, // sanitize_memory
	SANITIZE_NUMERICAL_STABILITY      = 61, // sanitize_numerical_stability
	SANITIZE_REALTIME                 = 62, // sanitize_realtime
	SANITIZE_REALTIME_BLOCKING        = 63, // sanitize_realtime_blocking
	SANITIZE_THREAD                   = 64, // sanitize_thread
	SANITIZE_TYPE                     = 65, // sanitize_type
	SHADOW_CALL_STACK                 = 66, // shadowcallstack
	SKIP_PROFILE                      = 67, // skipprofile
	SPECULATABLE                      = 68, // speculatable
	SPECULATIVE_LOAD_HARDENING        = 69, // speculative_load_hardening
	STACK_PROTECT                     = 70, // ssp
	STACK_PROTECT_REQ                 = 71, // sspreq
	STACK_PROTECT_STRONG              = 72, // sspstrong
	STRICT_FP                         = 73, // strictfp
	SWIFT_ASYNC                       = 74, // swiftasync
	SWIFT_ERROR                       = 75, // swifterror
	SWIFT_SELF                        = 76, // swiftself
	WILL_RETURN                       = 77, // willreturn
	WRITABLE                          = 78, // writable
	WRITE_ONLY                        = 79, // writeonly
	ZEXT                              = 80, // zeroext
	BY_REF                            = 81, // byref
	BY_VAL                            = 82, // byval
	ELEMENT_TYPE                      = 83, // elementtype
	IN_ALLOCA                         = 84, // inalloca
	PREALLOCATED                      = 85, // preallocated
	STRUCT_RET                        = 86, // sret
	ALIGNMENT                         = 87, // align
	ALLOC_KIND                        = 88, // allockind
	ALLOC_SIZE                        = 89, // allocsize
	CAPTURES                          = 90, // captures
	DEREFERENCEABLE                   = 91, // dereferenceable
	DEREFERENCEABLE_OR_NULL           = 92, // dereferenceable_or_null
	MEMORY                            = 93, // memory
	NO_FPCLASS                        = 94, // nofpclass
	STACK_ALIGNMENT                   = 95, // alignstack
	UWTABLE                           = 96, // uwtable
	VSCALE_RANGE                      = 97, // vscale_range
}

// Get LLVM IR string name for attribute
llvm_attribute_string :: proc(attr: LLVMAttributeKind) -> cstring {
	switch attr {
	case .ALLOC_ALIGN:
		return "allocalign"
	case .ALLOCATED_POINTER:
		return "allocptr"
	case .ALWAYS_INLINE:
		return "alwaysinline"
	case .BUILTIN:
		return "builtin"
	case .COLD:
		return "cold"
	case .CONVERGENT:
		return "convergent"
	case .CORO_DESTROY_ONLY_WHEN_COMPLETE:
		return "coro_only_destroy_when_complete"
	case .CORO_ELIDE_SAFE:
		return "coro_elide_safe"
	case .DEAD_ON_RETURN:
		return "dead_on_return"
	case .DEAD_ON_UNWIND:
		return "dead_on_unwind"
	case .DISABLE_SANITIZER_INSTRUMENTATION:
		return "disable_sanitizer_instrumentation"
	case .FN_RET_THUNK_EXTERN:
		return "fn_ret_thunk_extern"
	case .HOT:
		return "hot"
	case .HYBRID_PATCHABLE:
		return "hybrid_patchable"
	case .IMM_ARG:
		return "immarg"
	case .IN_REG:
		return "inreg"
	case .INLINE_HINT:
		return "inlinehint"
	case .JUMP_TABLE:
		return "jumptable"
	case .MIN_SIZE:
		return "minsize"
	case .MUST_PROGRESS:
		return "mustprogress"
	case .NAKED:
		return "naked"
	case .NEST:
		return "nest"
	case .NO_ALIAS:
		return "noalias"
	case .NO_BUILTIN:
		return "nobuiltin"
	case .NO_CALLBACK:
		return "nocallback"
	case .NO_CF_CHECK:
		return "nocf_check"
	case .NO_DIVERGENCE_SOURCE:
		return "nodivergencesource"
	case .NO_DUPLICATE:
		return "noduplicate"
	case .NO_EXT:
		return "noext"
	case .NO_FREE:
		return "nofree"
	case .NO_IMPLICIT_FLOAT:
		return "noimplicitfloat"
	case .NO_INLINE:
		return "noinline"
	case .NO_MERGE:
		return "nomerge"
	case .NO_PROFILE:
		return "noprofile"
	case .NO_RECURSE:
		return "norecurse"
	case .NO_RED_ZONE:
		return "noredzone"
	case .NO_RETURN:
		return "noreturn"
	case .NO_SANITIZE_BOUNDS:
		return "nosanitize_bounds"
	case .NO_SANITIZE_COVERAGE:
		return "nosanitize_coverage"
	case .NO_SYNC:
		return "nosync"
	case .NO_UNDEF:
		return "noundef"
	case .NO_UNWIND:
		return "nounwind"
	case .NON_LAZY_BIND:
		return "nonlazybind"
	case .NON_NULL:
		return "nonnull"
	case .NULL_POINTER_IS_VALID:
		return "null_pointer_is_valid"
	case .OPT_FOR_FUZZING:
		return "optforfuzzing"
	case .OPTIMIZE_FOR_DEBUGGING:
		return "optdebug"
	case .OPTIMIZE_FOR_SIZE:
		return "optsize"
	case .OPTIMIZE_NONE:
		return "optnone"
	case .PRESPLIT_COROUTINE:
		return "presplitcoroutine"
	case .READ_NONE:
		return "readnone"
	case .READ_ONLY:
		return "readonly"
	case .RETURNED:
		return "returned"
	case .RETURNS_TWICE:
		return "returns_twice"
	case .SEXT:
		return "signext"
	case .SAFE_STACK:
		return "safestack"
	case .SANITIZE_ADDRESS:
		return "sanitize_address"
	case .SANITIZE_HWADDRESS:
		return "sanitize_hwaddress"
	case .SANITIZE_MEM_TAG:
		return "sanitize_memtag"
	case .SANITIZE_MEMORY:
		return "sanitize_memory"
	case .SANITIZE_NUMERICAL_STABILITY:
		return "sanitize_numerical_stability"
	case .SANITIZE_REALTIME:
		return "sanitize_realtime"
	case .SANITIZE_REALTIME_BLOCKING:
		return "sanitize_realtime_blocking"
	case .SANITIZE_THREAD:
		return "sanitize_thread"
	case .SANITIZE_TYPE:
		return "sanitize_type"
	case .SHADOW_CALL_STACK:
		return "shadowcallstack"
	case .SKIP_PROFILE:
		return "skipprofile"
	case .SPECULATABLE:
		return "speculatable"
	case .SPECULATIVE_LOAD_HARDENING:
		return "speculative_load_hardening"
	case .STACK_PROTECT:
		return "ssp"
	case .STACK_PROTECT_REQ:
		return "sspreq"
	case .STACK_PROTECT_STRONG:
		return "sspstrong"
	case .STRICT_FP:
		return "strictfp"
	case .SWIFT_ASYNC:
		return "swiftasync"
	case .SWIFT_ERROR:
		return "swifterror"
	case .SWIFT_SELF:
		return "swiftself"
	case .WILL_RETURN:
		return "willreturn"
	case .WRITABLE:
		return "writable"
	case .WRITE_ONLY:
		return "writeonly"
	case .ZEXT:
		return "zeroext"
	case .BY_REF:
		return "byref"
	case .BY_VAL:
		return "byval"
	case .ELEMENT_TYPE:
		return "elementtype"
	case .IN_ALLOCA:
		return "inalloca"
	case .PREALLOCATED:
		return "preallocated"
	case .STRUCT_RET:
		return "sret"
	case .ALIGNMENT:
		return "align"
	case .ALLOC_KIND:
		return "allockind"
	case .ALLOC_SIZE:
		return "allocsize"
	case .CAPTURES:
		return "captures"
	case .DEREFERENCEABLE:
		return "dereferenceable"
	case .DEREFERENCEABLE_OR_NULL:
		return "dereferenceable_or_null"
	case .MEMORY:
		return "memory"
	case .NO_FPCLASS:
		return "nofpclass"
	case .STACK_ALIGNMENT:
		return "alignstack"
	case .UWTABLE:
		return "uwtable"
	case .VSCALE_RANGE:
		return "vscale_range"
	}
	return ""
}
