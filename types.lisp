(in-package #:org.shirakumo.fraf.manifolds)

(deftype vertex-array (&optional (element-type 'single-float))
  `(simple-array ,element-type (*)))

(deftype face-array ()
  '(simple-array (unsigned-byte 32) (*)))

(deftype u32 ()
  '(unsigned-byte 32))

(deftype f32 ()
  'single-float)

(deftype f64 ()
  'double-float)

(declaim (inline u32* u32 ensure-u32 f32* f32 ensure-f32 f64* f64 ensure-f64))

(declaim (ftype (function (real) u32) u32*))
(defun u32* (a)
  (ldb (byte 32 0) (truncate a)))

(declaim (ftype (function (&rest real) face-array) u32))
(defun u32 (&rest i)
  (make-array (length i) :element-type '(unsigned-byte 32) :initial-contents i))

(declaim (ftype (function (vector) face-array) ensure-u32))
(defun ensure-u32 (a)
  (etypecase a
    ((simple-array (unsigned-byte 32) (*))
     a)
    (vector
     (make-array (length a) :element-type '(unsigned-byte 32) :initial-contents a))))

(declaim (ftype (function (real) f32) f32*))
(defun f32* (a)
  (float a 0f0))

(declaim (ftype (function (&rest real) vertex-array) f32))
(defun f32 (&rest i)
  (map-into (make-array (length i) :element-type 'single-float)
            #'f32* i))

(defun ensure-f32 (a)
  (etypecase a
    ((simple-array single-float (*))
     a)
    (vector
     (make-array (length a) :element-type 'single-float :initial-contents a))))

(declaim (ftype (function (real) f64) f64*))
(defun f64* (a)
  (float a 0d0))

(declaim (ftype (function (&rest real) (vertex-array double-float)) f64))
(defun f64 (&rest i)
  (map-into (make-array (length i) :element-type 'double-float)
            #'f64* i))

(declaim (ftype (function (vector) (vertex-array double-float)) f64))
(defun ensure-f64 (a)
  (etypecase a
    ((simple-array double-float (*))
     a)
    (vector
     (make-array (length a) :element-type 'double-float :initial-contents a))))
