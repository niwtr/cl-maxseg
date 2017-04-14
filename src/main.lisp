(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:cl-maxseg :lparallel)))

(in-package :cl-maxseg)

(defparameter *training-file*
  #p"./train_utf8.txt")

(defun flat (lst)
  (let ((st))
  (mapcar (lambda (sent)
            (mapcar (lambda (y) (push y st)) sent))
          lst)
    st))

(defparameter *training-data*
  (flat
   (mapcar (lambda (sent)
             (split-by-stop/unhold sent #\space))
           (read-raw-file/line *training-file*))))

(defparameter *test-file*
  #p"./test_utf8.txt")
(defparameter *test-data* nil)
(defparameter *output-file*
  #P"./output_utf8.txt")

(defparameter *dict*
  (generate-word-dict *training-data*))


(defparameter *kernel*
  (maximum-probability-kernel))


(defun test-once ()
  (let
      ((fn (maximum-probability-generate-matcher *kernel*)))
    (loop for i from 1 to 100 do
         (progn
           (format t "Testing... ~A~%" i)
           (time (loop for line in *test-data* collect
                      (loop for sent in line collect (funcall fn sent))))))))





(lparallel:defpun test-mp ()
  (let ((fn (maximum-probability-generate-matcher *kernel*)))
    (loop for i from 1 to 100 do
         (progn
           (time (lparallel:pmap 'list (lambda (l)
                         (lparallel:pmap 'list (lambda (y)
                                 (funcall fn y))
                               l))
                       *test-data*))))))

(defun prompt-for-cpu ()
  (format t "Set number of CPU(s):")(force-output)
  (let ((cpus (read)))
    (if (numberp cpus)
        (progn
          (format t "Initializing parallelization kernel...~%")
          (setf lparallel:*kernel* (lparallel:make-kernel 2))
          (format t "Kernel initialized.~%")
          t)
        (progn
          (format t "Input a number, please.~%") nil))))



(defun prompt-for-test-file ()
  (format t "Now tell me filename of your test data (default test_utf8.txt):")
  (force-output)
  (let ((fname (read-line)))
    (unless (equal fname "")
      (setf *test-file* fname))
    (format t "Reading test file...~%")
    (setf *test-data*
      (mapcar #'split-by-stop
              (split-by-stop/unhold (read-raw-file *test-file*)
                                    #\return)))
      (format t "Success...~%") t))

(defun test ()
  (let*
      ((fn (maximum-probability-generate-matcher *kernel*))
       (rst
        (time (loop for line in *test-data* collect
                   (loop for sent in line collect (funcall fn sent))))))
    (format t "Writing file ...~%")
    (with-open-file (ss *output-file*
                        :direction :output :if-exists :supersede)
      (loop for line in rst do
           (progn
             (loop for sent in line do
                  (loop for wd in sent do (progn (princ wd ss)
                                                 (princ #\Space ss))))
             (fresh-line ss))))))


(defun test/mp ()
  (let*
      ((fn (maximum-probability-generate-matcher *kernel*))
       (rst
        (time (lparallel:pmap 'list (lambda (l)
                                      (lparallel:pmap 'list (lambda (y)
                                                              (funcall fn y))
                                                      l))
                              *test-data*))))
    (format t "Writing file ...~%")
    (with-open-file (ss *output-file*
                        :direction :output :if-exists :supersede)
      (loop for line in rst do
           (progn
             (loop for sent in line do
                  (loop for wd in sent do (progn (princ wd ss)
                                                 (princ #\Space ss))))
             (fresh-line ss))))))


(defun prompt-for-output (fn)
  (format t "Now tell me filename of your output data (default output.txt):")
  (force-output)
  (let ((fname (read-line)))
    (unless (equal fname "")
      (setf *output-file* fname))
    (format t "Performing segmentation...~%")
    (funcall fn)
      ;;(test)
      (format t "Success...~%") t))


(defun prompt-for-usage ()
  (format t
          "Type <s> to perform segmentation (single core)
Type <t> to test algorithm (single core)
Type <m> to perform segmentation (multiple core)
Type <p> to test algorithm (multiple core)~%")
  (force-output)
  (let ((ch (read-line)))
    (cond ((equal ch "t")
           (test-once) t)
          ((equal ch "s")
            (prompt-for-output #'test)
           t)
          ((equal ch "m")
           (and
            (prompt-for-cpu)
            (prompt-for-output #'test/mp)))
          ((equal ch "p")
           (if (prompt-for-cpu)
               (progn (test-mp) t)
               nil))
          (t
           (progn
             (format t "Try again.~%") nil)))))

(maximum-probability-establish *kernel* *training-data*)


(defun main ()
  (format t "cl-maxseg, a Common Lisp implementation of MPM algorithm.~%")
  (format t "Author: Tianrui Niu : niwtr@bupt.edu.cn~%")
  (format t "Trianing corpus: 人民日报 ~%");;(force-output)
  (format t "Specially dedicated for Li.~%")
  (format t "This is a pre-compiled suite which is for demonstation ~%but is also productive-ready.~% ")
  (format t "Source code of this software can be found on github. ~%")
  (format t "See https://github.com/niwtr/cl-maxseg ~%")
  (format t "Compiled and packaged by Clozure CL 1.11.~%")
  (format t "Type <Return> to begin.~%")
  (read-line)
  (prompt-for-test-file)
  (prompt-for-usage)
  (format t "<return> to exit.~%")
  (read-line)
  (format t "bye...")
  )



#+ccl
(ccl:save-application "mp-maxseg" :toplevel-function #'cl-maxseg::main :purify t :prepend-kernel t)











;;(sb-ext:save-lisp-and-die "hello" :toplevel #'test :executable t)
