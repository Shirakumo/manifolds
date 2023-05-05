#|
 This file is a part of manifolds
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem manifolds
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Various manifold mesh algorithms"
  :homepage "https://shirakumo.github.io/manifolds/"
  :bug-tracker "https://github.com/shirakumo/manifolds/issues"
  :source-control (:git "https://github.com/shirakumo/manifolds.git")
  :serial T
  :components ((:file "manifolds"))
  :depends-on (:documentation-utils))
