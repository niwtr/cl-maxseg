;;;; cl-maxseg.asd

(asdf:defsystem #:cl-maxseg
  :description "cl-maxseg, high-performance implementation of maximum-probability segmentation algorithm"
  :author "Tianrui Niu niwtr@bupt.edu.cn"
  :license "MIT"
  :depends-on (#:excalibur
               #:cl-kn)
  :serial t
  :components ((:file "package")
               (:file "cl-maxseg")))

