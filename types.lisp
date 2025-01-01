(in-package #:org.shirakumo.fraf.manifolds)

(deftype vertex-array (&optional element-type)
  (if element-type
      `(simple-array ,element-type (*))
      `(or (simple-array single-float (*))
           (simple-array double-float (*)))))

(deftype face-array (&optional element-type)
  (if element-type
      `(simple-array ,element-type (*))
      `(or (simple-array (unsigned-byte 32) (*))
           (simple-array (unsigned-byte 16) (*)))))

(deftype u16 ()
  '(unsigned-byte 16))

(deftype u32 ()
  '(unsigned-byte 32))

(deftype f32 ()
  'single-float)

(deftype f64 ()
  'double-float)

(deftype vertex ()
  `(integer 0 ,(truncate (1- (ash 1 32)) 3)))

(deftype face ()
  `(integer 0 ,(truncate (1- (ash 1 32)) 3)))

(declaim (inline edge extended-edge))

(defstruct (edge
            (:constructor edge (start end))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (start (error "required") :type vertex)
  (end (error "required") :type vertex))

(defstruct (extended-edge
            (:include edge)
            (:constructor extended-edge (start end opposite))
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (opposite (error "required") :type vertex))

(declaim (inline u16* u16 ensure-u16 u32* u32 ensure-u32 f32* f32 ensure-f32 f64* f64 ensure-f64))

(declaim (ftype (function (real) u16) u16))
(defun u16 (a)
  (ldb (byte 16 0) (truncate a)))

(declaim (ftype (function (&rest real) face-array) u16*))
(defun u16* (&rest i)
  (map-into (make-array (length i) :element-type '(unsigned-byte 16))
            #'u16 i))

(declaim (ftype (function (vector) face-array) ensure-u16))
(defun ensure-u16 (a)
  (etypecase a
    ((simple-array (unsigned-byte 16) (*))
     a)
    (vector
     (map-into (make-array (length a) :element-type '(unsigned-byte 16))
               #'u16 a))))

(declaim (ftype (function (real) u32) u32))
(defun u32 (a)
  (ldb (byte 32 0) (truncate a)))

(declaim (ftype (function (&rest real) face-array) u32*))
(defun u32* (&rest i)
  (map-into (make-array (length i) :element-type '(unsigned-byte 32))
            #'u32 i))

(declaim (ftype (function (vector) face-array) ensure-u32))
(defun ensure-u32 (a)
  (etypecase a
    ((simple-array (unsigned-byte 32) (*))
     a)
    (vector
     (map-into (make-array (length a) :element-type '(unsigned-byte 32))
               #'u32 a))))

(declaim (ftype (function (real) f32) f32))
(defun f32 (a)
  (float a 0f0))

(declaim (ftype (function (&rest real) (vertex-array single-float)) f32*))
(defun f32* (&rest i)
  (map-into (make-array (length i) :element-type 'single-float)
            #'f32 i))

(declaim (ftype (function (vector) (vertex-array single-float)) ensure-f32))
(defun ensure-f32 (a)
  (etypecase a
    ((simple-array single-float (*))
     a)
    (vector
     (map-into (make-array (length a) :element-type 'single-float)
               #'f32 a))))

(declaim (ftype (function (real) f64) f64))
(defun f64 (a)
  (float a 0d0))

(declaim (ftype (function (&rest real) (vertex-array double-float)) f64*))
(defun f64* (&rest i)
  (map-into (make-array (length i) :element-type 'double-float)
            #'f64 i))

(declaim (ftype (function (vector) (vertex-array double-float)) ensure-f64))
(defun ensure-f64 (a)
  (etypecase a
    ((simple-array double-float (*))
     a)
    (vector
     (map-into (make-array (length a) :element-type 'double-float)
               #'f64 a))))

(defmacro with-specialization ((var typevar &rest expanded-types) &body body)
  `(etypecase ,var
     ,@(loop for type in expanded-types
             collect `(,type
                       ,(eval `(let ((,typevar ',type)) ,@body))))))

(defmacro with-vertex-specialization ((var &optional (component-type-var 'vertex-component-type)) &body body)
  `(with-specialization (,var type (vertex-array single-float) (vertex-array double-float))
     `(let ((,',component-type-var ',(second type)))
        (declare (ignorable ,',component-type-var))
        ,@',body)))

(defmacro with-face-specialization ((var &optional (component-type-var 'face-component-type)) &body body)
  `(with-specialization (,var type (face-array (unsigned-byte 16)) (face-array (unsigned-byte 32)))
     `(let ((,',component-type-var ',(second type)))
        (declare (ignorable ,',component-type-var))
        ,@',body)))

(declaim (inline simplify unsimplify))
(defun simplify (array)
  (declare (type array array))
  (make-array (length array) :element-type (array-element-type array)
                             :initial-contents array))

(defun unsimplify (array)
  (declare (type array array))
  (make-array (length array) :element-type (array-element-type array)
                             :initial-contents array
                             :adjustable T
                             :fill-pointer T))
