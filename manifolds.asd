(asdf:defsystem manifolds
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Various manifold mesh algorithms"
  :homepage "https://shirakumo.github.io/manifolds/"
  :bug-tracker "https://github.com/shirakumo/manifolds/issues"
  :source-control (:git "https://github.com/shirakumo/manifolds.git")
  :serial T
  :components ((:file "package")
               (:file "manifolds")
               (:file "aabb")
               (:file "convert")
               (:file "decomposition"))
  :depends-on (:3d-vectors
               :3d-spaces
               :quickhull
               :priority-queue
               :documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :manifolds-test))))
