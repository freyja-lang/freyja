package pga3d

// PGA3D (3,0,1) - Projective Geometric Algebra for 3D
// Based on Klein's efficient storage approach
// https://github.com/jeremyong/klein

// Scalar type (grade 0)
// Stores: s
Scalar :: distinct f32

// Point type (grade 3)
// Stores: e032, e013, e021, e123 (homogeneous coordinates)
// Layout: [x, y, z, w] where w=e123 is the homogeneous coordinate
Point :: struct {
	data: [4]f32,
}

// Direction type (grade 3) - point at infinity
// Same storage as Point but w=0
Direction :: distinct Point

// Line type (grade 2) - Plücker coordinates
// Stores: e01, e02, e03, e23, e31, e12
// Layout: [d01, d02, d03, m23, m31, m12]
// where d is the direction and m is the moment
Line :: struct {
	data: [6]f32,
}

// Plane type (grade 1)
// Stores: e0, e1, e2, e3
// Layout: [d, nx, ny, nz] where n is normal and d is distance
Plane :: struct {
	data: [4]f32,
}

// Rotor type (even grade: 0,2) - pure rotation
// Stores: scalar, e23, e31, e12 (quaternion-like)
// Layout: [s, b23, b31, b12]
Rotor :: struct {
	data: [4]f32,
}

// Translator type (even grade: 0,2) - pure translation
// Stores: scalar, e01, e02, e03
// Layout: [1, dx, dy, dz]
Translator :: struct {
	data: [4]f32,
}

// Motor type (even grade: 0,2) - rotation + translation (dual quaternion)
// Stores: rotor part + translator part
// Layout: [s, b23, b31, b12, e01, e02, e03, e0123]
Motor :: struct {
	data: [8]f32,
}

// Full multivector type (all grades) - fallback for general operations
// Stores all 16 components of PGA3D
// Layout: [s, e0, e1, e2, e3, e01, e02, e03, e23, e31, e12, e021, e013, e032, e123, e0123]
Multivector :: struct {
	data: [16]f32,
}

// Grade tracking for compile-time optimization
Grade_Info :: struct {
	grade:      u8, // Primary grade
	basis_mask: u16, // Bit mask of which basis elements are present
}

// Type aliases for clarity
Bivector :: Line // Grade 2 elements are bivectors in PGA
Trivector :: Point // Grade 3 elements
Quadvector :: Scalar // Grade 4 element (pseudoscalar e0123)

// Constructors
point :: proc(x, y, z: f32, w: f32 = 1.0) -> Point {
	return Point{{x, y, z, w}}
}

direction :: proc(x, y, z: f32) -> Direction {
	return Direction{{x, y, z, 0}}
}

plane :: proc(nx, ny, nz, d: f32) -> Plane {
	return Plane{{d, nx, ny, nz}}
}

line :: proc(d01, d02, d03, m23, m31, m12: f32) -> Line {
	return Line{{d01, d02, d03, m23, m31, m12}}
}

rotor :: proc(s, b23, b31, b12: f32) -> Rotor {
	return Rotor{{s, b23, b31, b12}}
}

translator :: proc(dx, dy, dz: f32) -> Translator {
	return Translator{{1.0, dx, dy, dz}}
}

motor :: proc(r: Rotor, t: Translator) -> Motor {
	return Motor{{r.data[0], r.data[1], r.data[2], r.data[3], t.data[1], t.data[2], t.data[3], 0}}
}

// Identity elements
IDENTITY_ROTOR :: Rotor{{1, 0, 0, 0}}
IDENTITY_TRANSLATOR :: Translator{{1, 0, 0, 0}}
IDENTITY_MOTOR :: Motor{{1, 0, 0, 0, 0, 0, 0, 0}}

// Origin point
ORIGIN :: Point{{0, 0, 0, 1}}

// Basis vectors (directions)
EX :: Direction{{1, 0, 0, 0}}
EY :: Direction{{0, 1, 0, 0}}
EZ :: Direction{{0, 0, 1, 0}}

// Basis planes
XY_PLANE :: Plane{{0, 0, 0, 1}} // e3 plane (z=0)
XZ_PLANE :: Plane{{0, 0, 1, 0}} // e2 plane (y=0)
YZ_PLANE :: Plane{{0, 1, 0, 0}} // e1 plane (x=0)
