(asdf:defsystem manifolds-test
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the manifolds system."
  :homepage "https://shirakumo.github.io/manifolds/"
  :bug-tracker "https://github.com/shirakumo/manifolds/issues"
  :source-control (:git "https://github.com/shirakumo/manifolds.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:manifolds :cl-wavefront :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :manifolds-test)))
