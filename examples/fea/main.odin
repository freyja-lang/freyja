package fea

import "core:fmt"

main :: proc() {
    fmt.println("=== Simple FEA Rod Element Solver ===")
    
    // Example 1: Simple tension bar
    example_tension_bar()
    
    // Example 2: 2D truss structure
    example_2d_truss()
}

// Example 1: Simple bar in tension
// Fixed at one end, force at the other
example_tension_bar :: proc() {
    fmt.println("\n--- Example 1: Tension Bar ---")
    fmt.println("1m steel bar, fixed at x=0, 1000N force at x=1")
    
    model := create_model()
    defer destroy_model(model)
    
    // Material properties
    E : f64 = 200e9    // Steel: 200 GPa
    A : f64 = 0.001    // Cross-section: 0.001 m^2
    
    // Create nodes
    n0 := add_node(model, 0.0, 0.0, 0.0)  // Fixed end
    n1 := add_node(model, 1.0, 0.0, 0.0)  // Free end
    
    // Create element
    add_rod_element(model, n0, n1, A, E)
    
    // Apply boundary conditions (fix left end)
    add_spc(model, n0, 0)  // Fix x
    add_spc(model, n0, 1)  // Fix y
    add_spc(model, n0, 2)  // Fix z
    
    // Apply force (1000 N in x-direction)
    add_force(model, n1, 0, 1000.0)
    
    // Solve
    solve(model)
    
    // Print results
    print_results(model)
    
    // Analytical solution
    fmt.println("\nAnalytical solution:")
    stress : f64 = 1000.0 / A
    strain : f64 = stress / E
    displacement : f64 = strain * 1.0  // Length = 1m
    fmt.printf("Displacement at x=1: %.6f m\n", displacement)
    fmt.printf("Stress: %.3f Pa\n", stress)
    fmt.printf("Strain: %.6f\n", strain)
}

// Example 2: Simple 2D truss
example_2d_truss :: proc() {
    fmt.println("\n--- Example 2: 2D Truss ---")
    fmt.println("Simple triangular truss with vertical load")
    
    model := create_model()
    defer destroy_model(model)
    
    // Material properties
    E : f64 = 200e9    // Steel: 200 GPa
    A : f64 = 0.001    // Cross-section: 0.001 m^2
    
    // Create nodes (triangle shape)
    n0 := add_node(model, 0.0, 0.0, 0.0)  // Bottom left (pinned)
    n1 := add_node(model, 4.0, 0.0, 0.0)  // Bottom right (roller)
    n2 := add_node(model, 2.0, 3.0, 0.0)  // Top (free)
    
    // Create elements
    add_rod_element(model, n0, n1, A, E)  // Bottom chord
    add_rod_element(model, n0, n2, A, E)  // Left diagonal
    add_rod_element(model, n1, n2, A, E)  // Right diagonal
    
    // Apply boundary conditions
    // Pin support at node 0
    add_spc(model, n0, 0)  // Fix x
    add_spc(model, n0, 1)  // Fix y
    add_spc(model, n0, 2)  // Fix z
    
    // Roller support at node 1 (can move in x)
    add_spc(model, n1, 1)  // Fix y
    add_spc(model, n1, 2)  // Fix z
    
    // Apply downward force at top node
    add_force(model, n2, 1, -10000.0)  // 10kN downward
    
    // Solve
    solve(model)
    
    // Print results
    print_results(model)
}