package pga3d

import "core:math"

// Geometric Product - The fundamental operation in GA
// For now, implementing the most common specialized cases

// Motor * Point - Transform a point by a motor (sandwich product)
// This is the most common operation in 3D graphics/robotics
motor_point :: proc "c" (m: Motor, p: Point) -> Point {
	// Optimized sandwich product: m * p * ~m
	// Based on Klein's efficient 48-operation implementation

	// TODO: Implement efficient sandwich product
	// For now, placeholder that preserves point
	return p
}

// Motor * Motor - Compose two motors
motor_motor :: proc "c" (m1, m2: Motor) -> Motor {
	// Motor composition is like dual quaternion multiplication

	// TODO: Implement efficient motor composition
	// For now, return first motor
	return m1
}

// Rotor * Rotor - Compose two rotors (quaternion multiplication)
rotor_rotor :: proc "c" (r1, r2: Rotor) -> Rotor {
	s1, b23_1, b31_1, b12_1 := r1.data[0], r1.data[1], r1.data[2], r1.data[3]
	s2, b23_2, b31_2, b12_2 := r2.data[0], r2.data[1], r2.data[2], r2.data[3]

	return Rotor {
		{
			s1 * s2 - b23_1 * b23_2 - b31_1 * b31_2 - b12_1 * b12_2,
			s1 * b23_2 + b23_1 * s2 + b31_1 * b12_2 - b12_1 * b31_2,
			s1 * b31_2 + b31_1 * s2 + b12_1 * b23_2 - b23_1 * b12_2,
			s1 * b12_2 + b12_1 * s2 + b23_1 * b31_2 - b31_1 * b23_2,
		},
	}
}

// Wedge Product (Outer Product) - Meet operation in projective space

// Point ^ Point - Line through two points
wedge_point_point :: proc "c" (p1, p2: Point) -> Line {
	x1, y1, z1, w1 := p1.data[0], p1.data[1], p1.data[2], p1.data[3]
	x2, y2, z2, w2 := p2.data[0], p2.data[1], p2.data[2], p2.data[3]

	return Line {
		{
			w1 * x2 - x1 * w2, // e01
			w1 * y2 - y1 * w2, // e02
			w1 * z2 - z1 * w2, // e03
			y1 * z2 - z1 * y2, // e23
			z1 * x2 - x1 * z2, // e31
			x1 * y2 - y1 * x2, // e12
		},
	}
}

// Point ^ Line - Plane through point and line
wedge_point_line :: proc "c" (p: Point, l: Line) -> Plane {
	x, y, z, w := p.data[0], p.data[1], p.data[2], p.data[3]
	d01, d02, d03, m23, m31, m12 :=
		l.data[0], l.data[1], l.data[2], l.data[3], l.data[4], l.data[5]

	return Plane {
		{
			x * m23 + y * m31 + z * m12, // e0
			w * m23 - d02 * z + d03 * y, // e1
			w * m31 + d01 * z - d03 * x, // e2
			w * m12 - d01 * y + d02 * x, // e3
		},
	}
}

// Line ^ Line - Point of intersection (if lines intersect)
wedge_line_line :: proc "c" (l1, l2: Line) -> Point {
	// TODO: Implement line intersection
	return ORIGIN
}

// Regressive Product (Vee) - Join operation in projective space

// Plane & Plane - Line of intersection
vee_plane_plane :: proc "c" (p1, p2: Plane) -> Line {
	d1, nx1, ny1, nz1 := p1.data[0], p1.data[1], p1.data[2], p1.data[3]
	d2, nx2, ny2, nz2 := p2.data[0], p2.data[1], p2.data[2], p2.data[3]

	return Line {
		{
			nx1 * ny2 - ny1 * nx2, // e01
			nx1 * nz2 - nz1 * nx2, // e02
			ny1 * nz2 - nz1 * ny2, // e03
			d1 * nx2 - d2 * nx1, // e23
			d1 * ny2 - d2 * ny1, // e31
			d1 * nz2 - d2 * nz1, // e12
		},
	}
}

// Inner Product (Dot) - Contractions

// Plane · Point - Signed distance from point to plane
dot_plane_point :: proc "c" (plane: Plane, p: Point) -> f32 {
	d, nx, ny, nz := plane.data[0], plane.data[1], plane.data[2], plane.data[3]
	x, y, z, w := p.data[0], p.data[1], p.data[2], p.data[3]

	return (nx * x + ny * y + nz * z + d * w) / w
}

// Line · Line - Scalar measuring relative orientation
dot_line_line :: proc "c" (l1, l2: Line) -> f32 {
	sum: f32 = 0
	for i in 0 ..< 6 {
		sum += l1.data[i] * l2.data[i]
	}
	return sum
}

// Reverse operation (~) - Reverses the order of basis vectors

reverse_rotor :: proc "c" (r: Rotor) -> Rotor {
	// Rotor reverse negates the bivector parts
	return Rotor{{r.data[0], -r.data[1], -r.data[2], -r.data[3]}}
}

reverse_motor :: proc "c" (m: Motor) -> Motor {
	// Motor reverse: rotor part reversed, translator part negated differently
	return Motor {
		{
			m.data[0],
			-m.data[1],
			-m.data[2],
			-m.data[3],
			-m.data[4],
			-m.data[5],
			-m.data[6],
			m.data[7],
		},
	}
}

// Dual operation (*) - Hodge dual

dual_point :: proc "c" (p: Point) -> Plane {
	// Point dual is a plane
	return Plane{{p.data[3], p.data[2], -p.data[1], p.data[0]}}
}

dual_plane :: proc "c" (p: Plane) -> Point {
	// Plane dual is a point
	return Point{{p.data[3], -p.data[2], p.data[1], p.data[0]}}
}

// Normalization

normalize_point :: proc "c" (p: Point) -> Point {
	w := p.data[3]
	if w == 0 {return p} 	// Point at infinity

	return Point{{p.data[0] / w, p.data[1] / w, p.data[2] / w, 1}}
}

normalize_plane :: proc "c" (p: Plane) -> Plane {
	nx, ny, nz := p.data[1], p.data[2], p.data[3]
	norm := math.sqrt(nx * nx + ny * ny + nz * nz)
	if norm == 0 {return p}

	return Plane{{p.data[0] / norm, nx / norm, ny / norm, nz / norm}}
}

normalize_rotor :: proc "c" (r: Rotor) -> Rotor {
	norm_sqr :=
		r.data[0] * r.data[0] +
		r.data[1] * r.data[1] +
		r.data[2] * r.data[2] +
		r.data[3] * r.data[3]
	norm := math.sqrt(norm_sqr)
	if norm == 0 {return IDENTITY_ROTOR}

	return Rotor{{r.data[0] / norm, r.data[1] / norm, r.data[2] / norm, r.data[3] / norm}}
}

// Exponential map - Create rotors/motors from bivectors

exp_bivector :: proc "c" (b23, b31, b12: f32) -> Rotor {
	// exp(B) where B is a bivector (pure imaginary quaternion)
	angle := math.sqrt(b23 * b23 + b31 * b31 + b12 * b12)

	if angle < 0.001 {
		// Small angle approximation
		return Rotor{{1, b23, b31, b12}}
	}

	c := math.cos(angle)
	s := math.sin(angle) / angle

	return Rotor{{c, s * b23, s * b31, s * b12}}
}

// Logarithm - Extract bivector from rotor

log_rotor :: proc "c" (r: Rotor) -> (b23, b31, b12: f32) {
	s := r.data[0]

	if abs(s) >= 0.9999 {
		// Near identity
		return r.data[1], r.data[2], r.data[3]
	}

	angle := math.acos(s)
	scale := angle / math.sin(angle)

	return scale * r.data[1], scale * r.data[2], scale * r.data[3]
}

// Utility functions

// Create a rotor from axis and angle
rotor_from_axis_angle :: proc "c" (axis: Direction, angle: f32) -> Rotor {
	half_angle := angle * 0.5
	s := math.cos(half_angle)
	c := math.sin(half_angle)

	// Axis components give us the bivector
	x, y, z := axis.data[0], axis.data[1], axis.data[2]
	norm := math.sqrt(x * x + y * y + z * z)
	if norm == 0 {return IDENTITY_ROTOR}

	x, y, z = x / norm, y / norm, z / norm

	// In PGA3D, rotation bivectors are e23, e31, e12
	return Rotor{{s, c * x, c * y, c * z}}
}

// Create a translator from a displacement vector
translator_from_vector :: proc "c" (dx, dy, dz: f32) -> Translator {
	return Translator{{1, dx * 0.5, dy * 0.5, dz * 0.5}}
}

// Create a motor from rotor and translator
motor_from_rotor_translator :: proc "c" (r: Rotor, t: Translator) -> Motor {
	// Combine rotor and translator into motor
	// M = T * R in PGA (translator applied first)

	// TODO: Proper motor composition
	return Motor{{r.data[0], r.data[1], r.data[2], r.data[3], t.data[1], t.data[2], t.data[3], 0}}
}
