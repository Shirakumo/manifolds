(in-package #:org.shirakumo.fraf.manifolds)

(docs:define-docs
  (type vertex-array
    "Type for a vertex array.

Vertices are always a triplet of X Y Z coordinates. The vertices by
themselves represent a point cloud. A FACE-ARRAY is required to build
a surface using the vertices.

If an element-type argument is given is an alias for:
  (SIMPLE-ARRAY element-type (*))

Otherwise is an alias for:
  (OR (SIMPLE-ARRAY SINGLE-FLOAT (*))
      (SIMPLE-ARRAY DOUBLE-FLOAT (*)))")
  
  (type face-array
    "Type for a face vertex indices array.

Faces are always triangular, meaning a pair of three vertex indices
represents a face. The order of the vertices must always be
counter-clockwise whenever the normal of the face matters.

Each vertex index in the face array describes the index of the vertex,
NOT the starting index in the corresponding vertex-array. Meaning in
order to reach the first element of the described vertex, the index
must first be multiplied by 3.

This is an alias for:
  (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))")
  
  (type vertex
    "Type for a vertex index.

See VERTEX-ARRAY (type)")
  
  (type face
    "Type for a face index.

See FACE-ARRAY (type)")
  
  (type u32
    "Type alias for (UNSIGNED-BYTE 32)

See U32")
  
  (type f32
    "Type alias for SINGLE-FLOAT.

See F32")
  
  (type f64
    "Type alias for DOUBLE-FLOAT.

See F64")
  
  (function u32
    "Coerce a REAL into a U32.

See U32 (type)")
  
  (function f32
    "Coerce a REAL into a F32.

See F32 (type)")
  
  (function f64
    "Coerce a REAL into a F64.

See F64 (type)")
  
  (function ensure-u32
    "Ensure the given vector is an U32-vector.

If it is not, returns a fresh vector that is.

See U32 (type)")
  
  (function ensure-f32
    "Ensure the given vector is an F32-vector.

If it is not, returns a fresh vector that is.

See F32 (type)")
  
  (function ensure-f64
    "Ensure the given vector is an F64-vector.

If it is not, returns a fresh vector that is.

See F64 (type)")
  
  (function u32*
    "Construct a U32-vector from arguments.

The arguments are automatically coerced to U32 elements.

See U32 (type)")
  
  (function f32*
    "Construct a F32-vector from arguments.

The arguments are automatically coerced to F32 elements.

See F32 (type)")
  
  (function f64*
    "Construct a F64-vector from arguments.

The arguments are automatically coerced to F64 elements.

See F64 (type)"))

(docs:define-docs
  (function do-faces
    "Iterate over the faces of a FACE-ARRAY.

A B and C are bound to the vertex indices for each face in the
FACE-ARRAY. RESULT is evaluated and returned at the end of iteration.

FACES must be of type FACE-ARRAY.

See FACE-ARRAY (type)")
  
  (function vertex-adjacency-list
    "Returns a vector of adjacent vertices for each vertex.

To get the list of adjacent vertices for a vertex, access the
element corresponding to the vertex' index.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)")
  
  (function face-adjacency-list
    "Returns a vector of adjacent faces for each face.

To get the list of adjacent faces for a face, access the element
corresponding to the face's index.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)")
  
  (function half-edge-list
    "Returns a vector of half edges for the described faces.

A half-edge is a directed edge from one vertex of a face to
another. Each entry in the array represents a half-edge and contains
an EDGE instance with the START and END slots each being vertex
indices, and the edge pointing from the START to the END.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)
See EDGE-LIST")
  
  (function edge-list
    "Returns a vector of bidirectional edges for the described faces.

Unlike the half-edge-list, this edge-list does not contain any
duplicated edges between any two vertices. Each entry in the resulting
vector is one edge described by an EDGE instance with the START and
END slots being vertex indices. Note that the actual direction of the
edge is not indicated.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)
See HALF-EDGE-LIST")

  (function boundary-list
    "Returns a vector of edges on the boundary of the faces.

Each entry in the resulting vector is one edge described by an
EXTENDED-EDGE instance with the START and END slots being vertex
indices, and the edge pointing from the START to the END. The OPPOSITE
slot contains the index of the third vertex of the face to which the
edge indicated by START and END belongs.

Note that this requires that the FACES describe one coherent shape
with only one outside edge. It does not consider multiple edge loops
on the faces as separate boundaries.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)")
  
  (function v
    "Access a vertex as a vector.

The vector matches the element-type of the vertices array.

VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)")
  
  (function face-normal
    "Returns the normal vector of the FACE.

FACE must be a FACE.
FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See FACE (type)
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function face-normals
    "Returns an array of the normals of all faces.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function face-area
    "Returns the area covered by the face.

FACE must be a FACE.
FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See FACE (type)
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function surface-area
    "Returns the total surface area of the mesh.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See FACE-AREA
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function boundary-length
    "Returns the length of the boundary of the mesh.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See BOUNDARY-LIST
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function centroid
    "Returns the centroid of the mesh.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function convex-volume
    "Returns the volume of the mesh.

The mesh must be convex. If it is concave, the result will not be
accurate and may fail to compute.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function closest-point-on-triangle
    "Returns the closest point to POINT that lies on the FACE.

FACE must be a FACE.
FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See FACE (type)
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function face-in-volume-p
    "Returns true if the FACE is within the AABB described by LOCATION and BSIZE.

LOCATION should be the center of the AABB, and BSIZE the half-size
extent of the AABB.

FACE must be a FACE.
FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See FACES-IN-VOLUME
See FACE (type)
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function faces-in-volume
    "Returns a vector of all faces that are part of the AABB described by LOCATION and BSIZE.

LOCATION should be the center of the AABB, and BSIZE the half-size
extent of the AABB.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See FACE-IN-VOLUME-P
See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function bounding-box
    "Returns the AABB of the point cloud.

The AABB is returned as two VEC3s, the first being the center,, and
the second being the half-size.

VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)")
  
  (function vertex-faces
    "Returns a vector of faces bordering each vertex.

Each element in the returned array is another vector of face indices
for the element's corresponding vector.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)")
  
  (function 2-manifold-p
    "Returns true if the face array describes a 2-manifold.

FACES must be a FACE-ARRAY.

See FACE-ARRAY (type)")
  
  (function separate-meshes
    "Returns a list of disjoint meshes.

Each entry in the list is a CONS of a VERTEX-ARRAY and a FACE-ARRAY
that make up the mesh. Each mesh is also disjoint from every other
mesh, meaning they do not share any vertices.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)
See FACE-ARRAY (type)")
  
  (function normalize
    "Normalizes the mesh by removing faces with no surface area, and merging vertices that are within a threshold from each other.

Returns two values, a fresh VERTICES array and fresh FACES array.
The THRESHOLD is the distance threshold below which two vertices are
considered the same. You may also optionally pass in a CENTER and
SCALE to which the resulting vertices are positioned and scaled prior
to normalisation.

FACES must be a FACE-ARRAY.
VERTICES must be a VERTEX-ARRAY.

See VERTEX-ARRAY (type)
See FACE-ARRAY (type)"))
