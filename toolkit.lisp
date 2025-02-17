(in-package #:org.shirakumo.fraf.manifolds)

(defparameter *dbg-start-time* (get-internal-real-time))

(defun dbg (&rest stuff)
  (format *debug-io* "~&MANIFOLDS ~6,2f> ~{~a~^ ~}~%"
          (float (/ (- (get-internal-real-time) *dbg-start-time*) internal-time-units-per-second)) stuff))

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
