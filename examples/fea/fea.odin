package fea

import "core:fmt"
import "core:math"
import "core:math/linalg"
import "core:slice"

// Basic FEA types for rod elements
Node :: struct {
    id: int,
    x, y, z: f64,
    dofs: [3]int,  // DOF indices in global system (-1 if constrained)
}

Rod_Element :: struct {
    id: int,
    node1, node2: int,  // Node indices
    area: f64,          // Cross-sectional area
    E: f64,             // Young's modulus
}

SPC :: struct {  // Single Point Constraint
    node_id: int,
    dof: int,      // 0=x, 1=y, 2=z
    value: f64,    // Prescribed displacement
}

Force :: struct {
    node_id: int,
    dof: int,      // 0=x, 1=y, 2=z  
    value: f64,    // Force magnitude
}

FEA_Model :: struct {
    nodes: [dynamic]Node,
    elements: [dynamic]Rod_Element,
    spcs: [dynamic]SPC,
    forces: [dynamic]Force,
    
    // Global system
    n_dofs: int,
    K: []f64,  // Global stiffness matrix (row-major)
    F: []f64,  // Global force vector
    U: []f64,  // Global displacement vector
}

// Create a new FEA model
create_model :: proc() -> ^FEA_Model {
    model := new(FEA_Model)
    return model
}

// Add a node to the model
add_node :: proc(model: ^FEA_Model, x, y, z: f64) -> int {
    node := Node{
        id = len(model.nodes),
        x = x, y = y, z = z,
        dofs = {-1, -1, -1},  // Will be assigned later
    }
    append(&model.nodes, node)
    return node.id
}

// Add a rod element
add_rod_element :: proc(model: ^FEA_Model, node1, node2: int, area, E: f64) -> int {
    element := Rod_Element{
        id = len(model.elements),
        node1 = node1,
        node2 = node2,
        area = area,
        E = E,
    }
    append(&model.elements, element)
    return element.id
}

// Add a boundary condition (SPC)
add_spc :: proc(model: ^FEA_Model, node_id: int, dof: int, value: f64 = 0.0) {
    spc := SPC{
        node_id = node_id,
        dof = dof,
        value = value,
    }
    append(&model.spcs, spc)
}

// Add a force
add_force :: proc(model: ^FEA_Model, node_id: int, dof: int, value: f64) {
    force := Force{
        node_id = node_id,
        dof = dof,
        value = value,
    }
    append(&model.forces, force)
}

// Compute element length
element_length :: proc(model: ^FEA_Model, elem: Rod_Element) -> f64 {
    n1 := model.nodes[elem.node1]
    n2 := model.nodes[elem.node2]
    
    dx := n2.x - n1.x
    dy := n2.y - n1.y
    dz := n2.z - n1.z
    
    return math.sqrt(dx*dx + dy*dy + dz*dz)
}

// Compute direction cosines for rod element
direction_cosines :: proc(model: ^FEA_Model, elem: Rod_Element) -> (cx, cy, cz: f64) {
    n1 := model.nodes[elem.node1]
    n2 := model.nodes[elem.node2]
    
    dx := n2.x - n1.x
    dy := n2.y - n1.y
    dz := n2.z - n1.z
    
    L := math.sqrt(dx*dx + dy*dy + dz*dz)
    
    return dx/L, dy/L, dz/L
}

// Build element stiffness matrix in global coordinates (6x6 for 3D rod)
element_stiffness :: proc(model: ^FEA_Model, elem: Rod_Element, K_elem: []f64) {
    // K_elem should be pre-allocated as 6x6 = 36 elements
    assert(len(K_elem) == 36)
    
    // Clear matrix
    for i in 0..<36 {
        K_elem[i] = 0.0
    }
    
    L := element_length(model, elem)
    cx, cy, cz := direction_cosines(model, elem)
    
    // Stiffness coefficient
    k := elem.area * elem.E / L
    
    // Build element stiffness matrix in global coords
    // For a rod element in 3D space:
    // K = k * [T^T * T] where T is direction cosine vector
    
    // First build the direction cosine matrix (3x1 for each node)
    dc := [6]f64{cx, cy, cz, cx, cy, cz}
    
    // The element stiffness matrix for a rod is:
    // K_global = k * [ T*T^T  -T*T^T ]
    //                [-T*T^T   T*T^T ]
    
    // Upper left block (3x3): node1-node1
    for i in 0..<3 {
        for j in 0..<3 {
            K_elem[i*6 + j] = k * dc[i] * dc[j]
        }
    }
    
    // Upper right block (3x3): node1-node2
    for i in 0..<3 {
        for j in 0..<3 {
            K_elem[i*6 + (j+3)] = -k * dc[i] * dc[j+3]
        }
    }
    
    // Lower left block (3x3): node2-node1
    for i in 0..<3 {
        for j in 0..<3 {
            K_elem[(i+3)*6 + j] = -k * dc[i+3] * dc[j]
        }
    }
    
    // Lower right block (3x3): node2-node2
    for i in 0..<3 {
        for j in 0..<3 {
            K_elem[(i+3)*6 + (j+3)] = k * dc[i+3] * dc[j+3]
        }
    }
}

// Assign DOF numbers (accounting for SPCs)
assign_dofs :: proc(model: ^FEA_Model) {
    // First mark constrained DOFs
    for &node in model.nodes {
        node.dofs = {-1, -1, -1}  // Reset
    }
    
    // Mark SPCs
    for spc in model.spcs {
        model.nodes[spc.node_id].dofs[spc.dof] = -2  // Mark as constrained
    }
    
    // Assign DOF numbers to unconstrained DOFs
    dof_counter := 0
    for &node in model.nodes {
        for i in 0..<3 {
            if node.dofs[i] == -1 {  // Not constrained
                node.dofs[i] = dof_counter
                dof_counter += 1
            }
        }
    }
    
    model.n_dofs = dof_counter
}

// Assemble global stiffness matrix
assemble_stiffness :: proc(model: ^FEA_Model) {
    n := model.n_dofs
    
    // Allocate global matrices
    model.K = make([]f64, n * n)
    model.F = make([]f64, n)
    model.U = make([]f64, n)
    
    // Clear arrays
    for i in 0..<(n*n) {
        model.K[i] = 0.0
    }
    for i in 0..<n {
        model.F[i] = 0.0
        model.U[i] = 0.0
    }
    
    // Element stiffness matrix (6x6)
    K_elem := make([]f64, 36)
    defer delete(K_elem)
    
    // Assemble element contributions
    for elem in model.elements {
        element_stiffness(model, elem, K_elem)
        
        // Get DOF mapping
        n1 := &model.nodes[elem.node1]
        n2 := &model.nodes[elem.node2]
        
        dof_map := [6]int{
            n1.dofs[0], n1.dofs[1], n1.dofs[2],
            n2.dofs[0], n2.dofs[1], n2.dofs[2],
        }
        
        // Add to global matrix
        for i in 0..<6 {
            gi := dof_map[i]
            if gi < 0 do continue  // Skip constrained DOF
            
            for j in 0..<6 {
                gj := dof_map[j]
                if gj < 0 do continue  // Skip constrained DOF
                
                model.K[gi*n + gj] += K_elem[i*6 + j]
            }
        }
    }
    
    // Apply forces
    for force in model.forces {
        node := &model.nodes[force.node_id]
        dof_idx := node.dofs[force.dof]
        if dof_idx >= 0 {
            model.F[dof_idx] += force.value
        }
    }
}

// Simple Gaussian elimination solver
solve_system :: proc(model: ^FEA_Model) {
    n := model.n_dofs
    
    if n == 0 {
        fmt.println("Warning: No DOFs in system!")
        return
    }
    
    // Create working copies
    A := make([]f64, n * n)
    b := make([]f64, n)
    defer delete(A)
    defer delete(b)
    
    copy(A, model.K)
    copy(b, model.F)
    
    // Forward elimination
    for k in 0..<n {
        // Find pivot
        max_val := math.abs(A[k*n + k])
        max_row := k
        
        for i in (k+1)..<n {
            if math.abs(A[i*n + k]) > max_val {
                max_val = math.abs(A[i*n + k])
                max_row = i
            }
        }
        
        // Check for zero pivot
        if max_val < 1e-10 {
            fmt.printf("Warning: Near-zero pivot at row %d (value: %e)\n", k, max_val)
            continue
        }
        
        // Swap rows
        if max_row != k {
            for j in k..<n {
                A[k*n + j], A[max_row*n + j] = A[max_row*n + j], A[k*n + j]
            }
            b[k], b[max_row] = b[max_row], b[k]
        }
        
        // Eliminate column
        for i in (k+1)..<n {
            if math.abs(A[k*n + k]) < 1e-10 do continue
            factor := A[i*n + k] / A[k*n + k]
            for j in k..<n {
                A[i*n + j] -= factor * A[k*n + j]
            }
            b[i] -= factor * b[k]
        }
    }
    
    // Back substitution
    for i := n-1; i >= 0; i -= 1 {
        model.U[i] = b[i]
        for j in (i+1)..<n {
            model.U[i] -= A[i*n + j] * model.U[j]
        }
        if math.abs(A[i*n + i]) > 1e-10 {
            model.U[i] /= A[i*n + i]
        } else {
            model.U[i] = 0.0
        }
    }
}

// Solve the FEA problem
solve :: proc(model: ^FEA_Model) {
    assign_dofs(model)
    assemble_stiffness(model)
    solve_system(model)
}

// Print results
print_results :: proc(model: ^FEA_Model) {
    fmt.println("\n=== FEA RESULTS ===")
    fmt.println("Node Displacements:")
    
    for &node in model.nodes {
        fmt.printf("Node %d: ", node.id)
        for i in 0..<3 {
            if node.dofs[i] >= 0 {
                disp := model.U[node.dofs[i]]
                axis := i == 0 ? "x" : i == 1 ? "y" : "z"
                fmt.printf("u%s = %.6f  ", axis, disp)
            }
        }
        fmt.println()
    }
    
    // Calculate and print element stresses
    fmt.println("\nElement Stresses:")
    for elem in model.elements {
        n1 := &model.nodes[elem.node1]
        n2 := &model.nodes[elem.node2]
        
        // Get displacements
        u1 := [3]f64{}
        u2 := [3]f64{}
        
        for i in 0..<3 {
            if n1.dofs[i] >= 0 {
                u1[i] = model.U[n1.dofs[i]]
            }
            if n2.dofs[i] >= 0 {
                u2[i] = model.U[n2.dofs[i]]
            }
        }
        
        // Calculate elongation
        cx, cy, cz := direction_cosines(model, elem)
        delta_u := (u2[0] - u1[0])*cx + (u2[1] - u1[1])*cy + (u2[2] - u1[2])*cz
        
        // Stress = E * strain
        L := element_length(model, elem)
        strain := delta_u / L
        stress := elem.E * strain
        
        fmt.printf("Element %d: stress = %.3f, strain = %.6f\n", elem.id, stress, strain)
    }
}

// Cleanup
destroy_model :: proc(model: ^FEA_Model) {
    delete(model.nodes)
    delete(model.elements)
    delete(model.spcs)
    delete(model.forces)
    delete(model.K)
    delete(model.F)
    delete(model.U)
    free(model)
}