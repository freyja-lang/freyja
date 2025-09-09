package llvm_backend

import "core:fmt"
import llvm "../../llvm"

test_llvm :: proc() {
    // Create context and module
    ctx := llvm.LLVMContextCreate()
    defer llvm.LLVMContextDispose(ctx)
    
    mod := llvm.LLVMModuleCreateWithNameInContext("test_module", ctx)
    defer llvm.LLVMDisposeModule(mod)
    
    // Create main function type: i32 main()
    i32_type := llvm.LLVMInt32TypeInContext(ctx)
    main_type := llvm.LLVMFunctionType(i32_type, nil, 0, 0)
    main_fn := llvm.LLVMAddFunction(mod, "main", main_type)
    
    // Create entry block  
    entry := llvm.LLVMAppendBasicBlockInContext(ctx, main_fn, "entry")
    
    // Create builder and position at entry
    builder := llvm.LLVMCreateBuilderInContext(ctx)
    defer llvm.LLVMDisposeBuilder(builder)
    llvm.LLVMPositionBuilderAtEnd(builder, entry)
    
    // Return 42
    ret_val := llvm.LLVMConstInt(i32_type, 42, 0)
    llvm.LLVMBuildRet(builder, ret_val)
    
    // Print module
    module_str := llvm.LLVMPrintModuleToString(mod)
    fmt.println(module_str)
    llvm.LLVMDisposeMessage(module_str)
    
    // Write to bitcode file
    result := llvm.LLVMWriteBitcodeToFile(mod, "out/test.bc")
    if result != 0 {
        fmt.println("Failed to write bitcode")
    } else {
        fmt.println("Bitcode written to out/test.bc")
    }
    
    fmt.println("LLVM bindings test successful!")
}