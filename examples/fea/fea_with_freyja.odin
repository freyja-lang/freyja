package fea

// Example of FEA solver using Freyja's high-level matrix API
// This shows how clean the code becomes with proper bindings

import freyja "./freyja_bindings"
import "core:fmt"
import "core:math"

FEA_Model_Freyja :: struct {
    nodes: [dynamic]Node,
    elements: [dynamic]Rod_Element,
    spcs: [dynamic]SPC,
    forces: [dynamic]Force,
    
    // Using Freyja matrix types
    K: freyja.Matrix,  // Global stiffness matrix
    F: freyja.Vector,  // Force vector
    U: freyja.Vector,  // Displacement vector
    
    n_dofs: int,
}

// Build element stiffness using Freyja matrices
element_stiffness_freyja :: proc(model: ^FEA_Model_Freyja, elem: Rod_Element) -> freyja.Matrix {
    n1 := model.nodes[elem.node1]
    n2 := model.nodes[elem.node2]
    
    // Direction vector
    dir := freyja.vector(3)
    freyja.vec_set(&dir, 0, n2.x - n1.x)
    freyja.vec_set(&dir, 1, n2.y - n1.y)
    freyja.vec_set(&dir, 2, n2.z - n1.z)
    
    L := freyja.norm(dir)
    
    // Normalize direction vector
    dir = freyja.mul(1.0/L, dir)
    
    // Build 6x6 element stiffness matrix
    K_elem := freyja.matrix(6, 6)
    k := elem.area * elem.E / L
    
    // Build using outer product: K = k * d ⊗ d
    // Where d = [cx, cy, cz, -cx, -cy, -cz]
    d := freyja.vector(6)
    freyja.vec_set(&d, 0, freyja.at(dir, 0))
    freyja.vec_set(&d, 1, freyja.at(dir, 1))
    freyja.vec_set(&d, 2, freyja.at(dir, 2))
    freyja.vec_set(&d, 3, -freyja.at(dir, 0))
    freyja.vec_set(&d, 4, -freyja.at(dir, 1))
    freyja.vec_set(&d, 5, -freyja.at(dir, 2))
    
    // K_elem = k * (d ⊗ d) - outer product
    for i in 0..<6 {
        for j in 0..<6 {
            freyja.mat_set(&K_elem, i, j, k * freyja.at(d, i) * freyja.at(d, j))
        }
    }
    
    return K_elem
}

// Assemble global system using Freyja
assemble_system_freyja :: proc(model: ^FEA_Model_Freyja) {
    n := model.n_dofs
    
    // Create system matrices
    model.K = freyja.matrix(n, n)
    model.F = freyja.vector(n)
    model.U = freyja.vector(n)
    
    // Assemble stiffness from elements
    for elem in model.elements {
        K_elem := element_stiffness_freyja(model, elem)
        
        // Get DOF mapping
        n1 := &model.nodes[elem.node1]
        n2 := &model.nodes[elem.node2]
        
        dof_map := [6]int{
            n1.dofs[0], n1.dofs[1], n1.dofs[2],
            n2.dofs[0], n2.dofs[1], n2.dofs[2],
        }
        
        // Add element contribution to global matrix
        for i in 0..<6 {
            gi := dof_map[i]
            if gi < 0 do continue
            
            for j in 0..<6 {
                gj := dof_map[j]
                if gj < 0 do continue
                
                current := freyja.at(model.K, gi, gj)
                addition := freyja.at(K_elem, i, j)
                freyja.mat_set(&model.K, gi, gj, current + addition)
            }
        }
    }
    
    // Apply forces
    for force in model.forces {
        node := &model.nodes[force.node_id]
        dof_idx := node.dofs[force.dof]
        if dof_idx >= 0 {
            current := freyja.at(model.F, dof_idx)
            freyja.vec_set(&model.F, dof_idx, current + force.value)
        }
    }
}

// Solve using Freyja's high-level API
solve_freyja :: proc(model: ^FEA_Model_Freyja) -> bool {
    // Simply use Freyja's linear solver
    U, ok := freyja.solve(model.K, model.F)
    if !ok {
        fmt.println("Error: System is singular")
        return false
    }
    
    model.U = U
    return true
}

// Alternative: Use decomposition for multiple right-hand sides
solve_freyja_decomposed :: proc(model: ^FEA_Model_Freyja) -> bool {
    // LU decomposition for general matrices
    lu, ok := freyja.decompose(model.K)
    if !ok {
        fmt.println("Error: Matrix decomposition failed")
        return false
    }
    
    // Solve L*U*x = P*b
    // Freyja would provide a solve_lu function
    // model.U = freyja.solve_lu(lu, model.F)
    
    return true
}

// Calculate element stress using Freyja
calculate_stress :: proc(model: ^FEA_Model_Freyja, elem: Rod_Element) -> (stress, strain: f64) {
    n1 := &model.nodes[elem.node1]
    n2 := &model.nodes[elem.node2]
    
    // Get nodal displacements
    u1 := freyja.vector(3)
    u2 := freyja.vector(3)
    
    for i in 0..<3 {
        if n1.dofs[i] >= 0 {
            freyja.vec_set(&u1, i, freyja.at(model.U, n1.dofs[i]))
        }
        if n2.dofs[i] >= 0 {
            freyja.vec_set(&u2, i, freyja.at(model.U, n2.dofs[i]))
        }
    }
    
    // Calculate strain
    delta_u := freyja.add(u2, freyja.mul(-1.0, u1))
    
    // Project onto element axis
    dir := freyja.vector(3)
    freyja.vec_set(&dir, 0, n2.x - n1.x)
    freyja.vec_set(&dir, 1, n2.y - n1.y)
    freyja.vec_set(&dir, 2, n2.z - n1.z)
    
    L := freyja.norm(dir)
    dir = freyja.mul(1.0/L, dir)
    
    elongation := freyja.dot(delta_u, dir)
    strain = elongation / L
    stress = elem.E * strain
    
    return stress, strain
}

// Example: Modal analysis using Freyja
modal_analysis :: proc(model: ^FEA_Model_Freyja) {
    // Mass matrix (simplified - diagonal lumped mass)
    M := freyja.matrix(model.n_dofs, model.n_dofs)
    
    // For each node, add mass contribution
    for node, idx in model.nodes {
        for i in 0..<3 {
            if node.dofs[i] >= 0 {
                // Assuming unit mass per node for simplicity
                freyja.mat_set(&M, node.dofs[i], node.dofs[i], 1.0)
            }
        }
    }
    
    // Solve eigenvalue problem: K*phi = lambda*M*phi
    // This would use a generalized eigenvalue solver
    
    // Freyja would provide:
    // eigenvals, eigenvecs, ok := freyja.eigen_generalized(model.K, M)
    
    // For now, use standard eigenvalue (assumes M = I)
    eigenvals, eigenvecs, ok := freyja.eigen(model.K)
    
    if ok {
        fmt.println("\nNatural Frequencies:")
        for i in 0..<min(5, eigenvals.size) {
            lambda := freyja.at(eigenvals, i)
            if lambda > 0 {
                freq := math.sqrt(lambda) / (2 * math.PI)
                fmt.printf("Mode %d: %.2f Hz\n", i+1, freq)
            }
        }
    }
}

// Example usage showing the clean API
example_clean_api :: proc() {
    // Create stiffness matrix for a simple problem
    K := freyja.matrix(3, 3)
    freyja.mat_set(&K, 0, 0, 1000.0)
    freyja.mat_set(&K, 0, 1, -500.0)
    freyja.mat_set(&K, 1, 0, -500.0)
    freyja.mat_set(&K, 1, 1, 1000.0)
    freyja.mat_set(&K, 1, 2, -500.0)
    freyja.mat_set(&K, 2, 1, -500.0)
    freyja.mat_set(&K, 2, 2, 500.0)
    
    // Force vector
    F := freyja.vector(3)
    freyja.vec_set(&F, 2, 100.0)
    
    // Solve K*u = F
    u, ok := freyja.solve(K, F)
    
    if ok {
        freyja.print(u, "Displacements")
    }
    
    // Matrix operations are clean and intuitive
    K2 := freyja.mul(2.0, K)           // Scale matrix
    K3 := freyja.add(K, K2)            // Add matrices
    Kt := freyja.transpose(K)          // Transpose
    K_inv, _ := freyja.inverse(K)      // Inverse
    
    // Decompositions
    lu, _ := freyja.decompose(K)       // LU decomposition
    qr, _ := freyja.decompose_qr(K)    // QR decomposition
    
    // Matrix multiply
    A := freyja.matrix(3, 2)
    B := freyja.matrix(2, 3)
    C := freyja.mul(A, B)              // C = A * B
    
    // Vector operations
    v1 := freyja.vector(3)
    v2 := freyja.vector(3)
    
    v3 := freyja.add(v1, v2)           // Vector addition
    v4 := freyja.mul(2.0, v1)          // Scale vector
    dot_product := freyja.dot(v1, v2)  // Dot product
    norm_v1 := freyja.norm(v1)         // Euclidean norm
    
    // Matrix-vector multiply
    y := freyja.mul(K, v1)             // y = K * v1
}