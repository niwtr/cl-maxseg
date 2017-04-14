;;;; package.lisp

(defpackage #:cl-maxseg
  (:use #:cl #:cl-user #:common-lisp #:excalibur #:cl-kn)
  ;;(:import-from #:let-over-lambda dlambda)
  (:export
   #:split-by-stop
   #:split-by-stop/unhold
   #:read-raw-file
   #:read-raw-file/gbk
   #:generate-word-dict
   #:forward-maximum-match
   #:backward-maximum-match
   #:maximum-probability-kernel
   #:maximum-probability-generate-matcher
   #:maximum-probability-clear
   #:maximum-probability-establish
   ))


