Phase 1: Enhanced Scope Management ✅ (Already have basic version)

  - Hierarchical scopes (universe → package → procedure → block)
  - push_scope() / pop_scope() functions
  - Entity lookup through scope chain

  Phase 2: Deferred Procedure Checking (NEW)

  - Add procedure_queue to CheckerInfo for deferred body checking
  - check_procedure_later() - queue procedure bodies instead of checking immediately
  - check_procedure_bodies() - process all queued procedures after declarations

  Phase 3: Procedure Body Statement Processing (NEW)

  - check_procedure_body() - create procedure scope and check statements
  - check_statement() - dispatch different statement types
  - check_assignment_stmt() - handle a := 1, c := a + b
  - check_expression_stmt() - standalone expressions

  Phase 4: Local Variable Discovery (NEW)

  - Create variable entities for assignments (a, b, c)
  - Add variables to procedure scope
  - Type inference from RHS expressions

  Phase 5: Expression Type Checking (Enhanced)

  - check_expression() - comprehensive expression analysis
  - check_binary_expr() - type checking for a + b
  - check_identifier() - scope-based variable lookup
  - Store expression type info in CheckerInfo

  Proposed Implementation Flow:

  // Phase 1: Declaration pass (collect all procedure signatures)
  for decl in file.decls {
      case ^ast.Value_Decl:
          if is_procedure(decl) {
              entity := create_procedure_entity(decl)
              add_entity(info, entity)
              check_procedure_later(info, entity) // Queue for body checking
          }
  }

  // Phase 2: Body checking pass (check all procedure bodies)
  check_procedure_bodies(info)

  check_procedure_bodies :: proc(info: ^CheckerInfo) {
      for proc_entity in info.procedure_queue {
          check_procedure_body(info, proc_entity)
      }
  }

  check_procedure_body :: proc(info: ^CheckerInfo, proc_entity: ^Entity) {
      // Create procedure scope
      proc_scope := push_scope(info, .PROCEDURE)
      defer pop_scope(info)

      // Get procedure AST body
      proc_lit := get_procedure_ast(proc_entity)
      if proc_lit.body != nil {
          // Check all statements in procedure
          block := proc_lit.body.derived.(^ast.Block_Stmt)
          for stmt in block.stmts {
              check_statement(info, stmt)
          }
      }
  }

  This matches Odin's architecture:
  1. Two-phase checking (declarations first, then bodies)
  2. Deferred body analysis via procedure queue
  3. Proper scope management for procedure locals
  4. Statement-by-statement processing with type checking
  5. Local entity creation for variables