package pga3d

// Main PGA3D package file - re-exports and high-level API

// Operator overloading support (when Freyja implements it)
// These would map to the specialized geometric products

/*
// Geometric product
operator * :: proc{
    rotor_rotor,
    motor_motor,
    motor_point,
    // ... other combinations
}

// Wedge product (outer product)
operator ^ :: proc{
    wedge_point_point,
    wedge_point_line,
    wedge_line_line,
    // ... other combinations
}

// Regressive product (vee/join)
operator & :: proc{
    vee_plane_plane,
    // ... other combinations
}

// Inner product (dot)
operator | :: proc{
    dot_plane_point,
    dot_line_line,
    // ... other combinations
}

// Reverse
operator ~ :: proc{
    reverse_rotor,
    reverse_motor,
    // ... other types
}
*/

// High-level API

// Transform a point by a motor (most common operation)
transform :: proc(m: Motor, p: Point) -> Point {
	impl := select_sandwich_impl()
	return impl(m, p)
}

// Batch transform for efficiency
transform_batch :: proc(m: Motor, points: []Point) {
	transform_points_simd(m, points)
}

// Create a line through two points
line_through :: proc(p1, p2: Point) -> Line {
	return wedge_point_point(p1, p2)
}

// Create a plane through three points
plane_through :: proc(p1, p2, p3: Point) -> Plane {
	line := wedge_point_point(p1, p2)
	return wedge_point_line(p3, line)
}

// Find intersection of two planes
intersect :: proc {
	intersect_planes,
	intersect_lines,
	intersect_plane_line,
}

intersect_planes :: proc(p1, p2: Plane) -> Line {
	return vee_plane_plane(p1, p2)
}

intersect_lines :: proc(l1, l2: Line) -> Point {
	return wedge_line_line(l1, l2)
}

intersect_plane_line :: proc(p: Plane, l: Line) -> Point {
	// Plane & Line intersection
	return Point{} // TODO: Implement
}

// Distance calculations
distance :: proc {
	distance_point_plane,
	distance_point_line,
	distance_point_point,
}

distance_point_plane :: proc(p: Point, plane: Plane) -> f32 {
	return abs(dot_plane_point(plane, p))
}

distance_point_line :: proc(p: Point, l: Line) -> f32 {
	// TODO: Implement point-line distance
	return 0
}

distance_point_point :: proc(p1, p2: Point) -> f32 {
	// Normalize to Euclidean space first
	p1n := normalize_point(p1)
	p2n := normalize_point(p2)

	dx := p1n.data[0] - p2n.data[0]
	dy := p1n.data[1] - p2n.data[1]
	dz := p1n.data[2] - p2n.data[2]

	return sqrt(dx * dx + dy * dy + dz * dz)
}

// Reflection across a plane
reflect :: proc(p: Point, plane: Plane) -> Point {
	// Reflection formula: -plane * p * plane
	// TODO: Implement efficient reflection
	return p
}

// Projection onto a plane
project :: proc(p: Point, plane: Plane) -> Point {
	// Project point onto plane
	// TODO: Implement projection
	return p
}

// Interpolation (SLERP for rotors/motors)
slerp :: proc {
	slerp_rotor,
	slerp_motor,
}

slerp_rotor :: proc(r1, r2: Rotor, t: f32) -> Rotor {
	// Spherical linear interpolation
	// TODO: Implement rotor SLERP
	return r1
}

slerp_motor :: proc(m1, m2: Motor, t: f32) -> Motor {
	// Screw linear interpolation
	// TODO: Implement motor SLERP
	return m1
}

// Debugging utilities

print_point :: proc(p: Point) -> string {
	if p.data[3] == 0 {
		return fmt.sprintf("Direction[{:.3f}, {:.3f}, {:.3f}]", p.data[0], p.data[1], p.data[2])
	}

	// Normalize for display
	pn := normalize_point(p)
	return fmt.sprintf("Point[{:.3f}, {:.3f}, {:.3f}]", pn.data[0], pn.data[1], pn.data[2])
}

print_line :: proc(l: Line) -> string {
	return fmt.sprintf(
		"Line[d:({:.3f}, {:.3f}, {:.3f}), m:({:.3f}, {:.3f}, {:.3f})]",
		l.data[0],
		l.data[1],
		l.data[2],
		l.data[3],
		l.data[4],
		l.data[5],
	)
}

print_plane :: proc(p: Plane) -> string {
	return fmt.sprintf(
		"Plane[n:({:.3f}, {:.3f}, {:.3f}), d:{:.3f}]",
		p.data[1],
		p.data[2],
		p.data[3],
		p.data[0],
	)
}

print_rotor :: proc(r: Rotor) -> string {
	return fmt.sprintf(
		"Rotor[{:.3f} + {:.3f}e23 + {:.3f}e31 + {:.3f}e12]",
		r.data[0],
		r.data[1],
		r.data[2],
		r.data[3],
	)
}

print_motor :: proc(m: Motor) -> string {
	return fmt.sprintf(
		"Motor[R:({:.3f}, {:.3f}, {:.3f}, {:.3f}), T:({:.3f}, {:.3f}, {:.3f})]",
		m.data[0],
		m.data[1],
		m.data[2],
		m.data[3],
		m.data[4],
		m.data[5],
		m.data[6],
	)
}

// Import fmt for string formatting (when needed)
import "core:fmt"

// Import math functions we use
import "core:math"

sqrt :: math.sqrt
abs :: math.abs
