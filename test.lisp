(defpackage #:org.shirakumo.fraf.manifolds.test
  (:use #:cl #:parachute #:org.shirakumo.flare.vector)
  (:local-nicknames
   (#:manifolds #:org.shirakumo.fraf.manifolds))
  (:export))

(in-package #:org.shirakumo.fraf.manifolds.test)

(define-test manifolds)

(defun u32 (&rest i)
  (make-array (length i) :element-type '(unsigned-byte 32) :initial-contents i))

(defun f32 (&rest i)
  (make-array (length i) :element-type 'single-float :initial-contents (mapcar #'float i)))

(defun cube (&optional (s 0.5))
  (values
   (f32 (+ s) (+ s) (- s)
        (- s) (+ s) (- s)
        (- s) (+ s) (+ s)
        (+ s) (+ s) (+ s)
        (+ s) (- s) (+ s)
        (- s) (- s) (+ s)
        (- s) (- s) (- s)
        (+ s) (- s) (- s))
   (u32 0 1 2   2 3 0
        4 5 6   6 7 4
        3 2 5   5 4 3
        7 6 1   1 0 7
        2 1 6   6 5 2
        0 3 4   4 7 0)))

(define-test decomposition
  :parent manifolds
  (finish (multiple-value-call #'manifolds::decompose (cube) :resolution 100)))
