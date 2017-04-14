;;;; cl-maxseg.lisp

(in-package #:cl-maxseg)

;;; Hacks and glory await!

;;; This software is dedicated to my girlfriend, Lingxuan.
;;; I love her very much.


(defun split-by-stop/unhold (string &optional (delim #\。))
  "split a string by a delimiter.
   the delimiter is then omitted"
  (loop for i = 0 then (1+ j)
     as j = (position delim string :start i)
     collect (subseq string i j)
     while j))

(defun split-by-stop (string &optional (delim #\。))
  "split a string. the delimiter is then
   attached to the original string."
  (loop for i = 0 then (1+ j)
     as j = (position delim string :start i)
     if j collect
       (concatenate 'string (subseq string i j) (string  delim))
     else collect (subseq string i j)
     while j))

(defun read-raw-file/line (filename)
  "read a file and concatenate into a large string."
  (with-open-file (ss filename)
           (loop for line = (read-line ss nil)
              while line collect line)))

(defun read-raw-file (filename)
  ;;(lett (sb-impl::*default-external-format* :gbk)
  (with-open-file (ss filename)
    (apply #'concatenate 'string
           (loop for line = (read-line ss nil)
              while line collect line))))

#+sbcl
(defun read-raw-file/gbk (filename)
  "only for sbcl, read gbk file."
  (lett (sb-impl::*default-external-format* :gbk)
    (with-open-file (ss filename)
      (apply #'concatenate 'string
             (loop for line = (read-line ss nil)
                while line collect line)))))

(defun accumu-str (str)
  (concatenate 'string str))

(defun wordlist->dict (wordlst &key (fn #'identity))
  (lett (hash (make-hash-table :test #'equal :size 1500000))
    (mapc #'(lambda (word)
              (incf (gethash (funcall fn word) hash 0)))
          wordlst)
    hash))


(defun generate-word-dict (wordlst)
  "generate dict for forward/backward match."
  (wordlist->dict wordlst :fn #'identity))


(defun maximum-match (dir-fn seq dict wsize)
  "well this is trivial.
   dir-fn: #'identity->forward or #'reverse->backward.
   seq: sequence (text list)
   dict: word dict.
   wsize: window size."
  (let ((assembler (compose #'accumu-str dir-fn));;symbolize
        (seq (funcall dir-fn seq)))
    (funcall dir-fn
             (loop while seq collect
                  (loop for sz from wsize downto 1
                     do
                       (let* ((chbeg (firstn sz seq))
                              (word? (funcall assembler chbeg)))
                         (when (gethash word? dict)
                           (setf seq (nthcdr sz seq))
                           (return word?)))
                     finally
                       (unless (null seq)
                         (lett (word! (list (first seq)))
                           (setf seq (cdr seq))
                           (return (funcall assembler word!)))))))))

(defun forward-maximum-match (seq dict &optional (wsize 5))
  (maximum-match #'identity seq dict wsize))
(defun backward-maximum-match (seq dict &optional (wsize 5))
  (maximum-match #'reverse seq dict wsize))


(defun seg->size (seglst)
  (mapcar (lambda (seg)
            (length  seg))
          seglst))

(defstruct wordframe
  "wordframe for mpm. stored in stack."
  (:word    nil :type string :read-only t)
  (:prob    1.0              :read-only t)
  (:preword nil :type list   :read-only t))


(defun maximum-probability-generate-matcher (kernel)
  "generate a matcher for fast use."
  (funcall kernel :generate-matcher))
(defun maximum-probability-establish (kernel corpus
                                      &key (start-padding "<s>") (end-padding "<e>"))
  "build the ngram database (word dicts.)"
  (funcall kernel :establish corpus start-padding end-padding))

(defun maximum-probability-clear (kernel)
  "clear out the ngram dict."
  (funcall kernel :clear))

(defun maximum-probability-kernel ()
  "get the mpm kernel. a closure is returned."
  (let ((start-padding) (end-padding)
        (dict)
        (kn-kernel (kn-smooth))
        (kn-prober))
    (dlambda
     (:establish (corpus start-pad end-pad)
                 (declare (type string start-pad end-pad)
                          (type list corpus))
                 (setf start-padding start-pad)
                 (setf end-padding end-pad)
                 (setf dict (generate-word-dict corpus))
                 (kn-smooth-establish kn-kernel corpus)
                 (setf kn-prober (kn-smooth-generate-prober kn-kernel)))
     (:clear nil
             (setf dict nil)
             (kn-smooth-clear kn-kernel))
     (:generate-matcher nil
       (lambda (sent)
         (let* ((sentl (coerce sent 'list))
                (_a (forward-maximum-match sentl dict))
                (al (append _a (list end-padding)))
                (pre-al (cons start-padding  _a))
                (_b (backward-maximum-match sentl dict))
                (bl (append _b (list end-padding)))
                (pre-bl (cons start-padding  _b))
                (stack `(,(make-wordframe :word start-padding :prob 1 :preword nil)))
                (a-acc) (b-acc))
           (if (equal _a _b) _a
               (labels ((kn (w1 w2)
                          (funcall kn-prober `(,start-padding ,w2 ,w2)))
                        (slength (seg)
                          (declare (inline seg->size))
                          (reduce #'+ (seg->size seg)))
                        (find-word-frame (word)
                          (find word stack
                                :test (lambda (x pl)
                                        (equal (wordframe-word pl) word))))
                        (get-prob-from-stack (word) ;;always find the first.
                          (declare (inline find-word-frame))
                          (wordframe-prob (find-word-frame word)))
                        (accumula (w1 w2)
                          (if (equal w1 w2)
                              `(,w1) `(,w1 ,w2)))
                        (accumuprob (w wpre)
                          (* (kn w wpre)
                             (get-prob-from-stack wpre)))
                        (backward-walk (word &optional (acc nil))
                          ;;backward walk the stack, generate final segmentation.
                          (declare (type string word))
                          (if (equal word start-padding) (butlast acc)
                              (let* ((wordframe (find-word-frame word))
                                     (prewds (wordframe-preword wordframe)))
                                ;; we need to pop this wordframe to omit cycles.
                                ;; which would ruin our stack. game over.
                                (loop while (not (equal (pop stack) wordframe)))
                                (let ((bst (car (best (lambda (x y)
                                                        (> (cdr x) (cdr y)))
                                                      (mapcar (lambda (preword)
                                                                (cons preword
                                                                      (get-prob-from-stack preword)))
                                                              (wordframe-preword wordframe))))))
                                  (backward-walk bst (cons word acc)))))))
                 (declare (inline slength accumula accumuprob get-prob-from-stack))
                 (loop while (or al bl) do
                      (let ((l1 (slength a-acc)) (l2 (slength b-acc)))
                        (cond ((= l1 l2) ;;join point, add both prewords.
                               (let ((w1pre (pop pre-al))
                                     (w1 (pop al))
                                     (w2pre (pop pre-bl))
                                     (w2 (pop bl)))
                                 (push w1 a-acc) (push w2 b-acc) ;;加入acc
                                 (cond ((equal w1 w2) ;;w1 equals w2, they share both pres.
                                        (push
                                         (make-wordframe
                                          :word w1
                                          :prob (max ;;calc accumulate prob.
                                                 (accumuprob w1 w1pre)
                                                 (accumuprob w2 w2pre))
                                          :preword
                                          (accumula w1pre w2pre))
                                         stack))
                                       (t ;;w1 unequals w2. they gain their sole pres.
                                        (push
                                         (make-wordframe
                                          :word w1
                                          :prob (accumuprob w1 w1pre)
                                          :preword `(,w1pre))
                                         stack)
                                        (push
                                         (make-wordframe
                                          :word w2
                                          :prob (accumuprob w2 w2pre)
                                          :preword `(,w2pre))
                                         stack)))))
                              ((> l1 l2) ;;move l2 forward.
                               (let ((w2pre (pop pre-bl))
                                     (w2 (pop bl)))
                                 (push w2 b-acc) ;;add acc into the list.
                                 (push
                                  (make-wordframe
                                   :word w2
                                   :prob (accumuprob w2 w2pre)
                                   :preword `(,w2pre))
                                  stack)))
                              (t ;;move l1 forward.
                               (let ((w1pre (pop pre-al))
                                     (w1 (pop al)))
                                 (push w1 a-acc) ;;add acc into the list.
                                 (push
                                  (make-wordframe
                                   :word w1
                                   :prob (accumuprob w1 w1pre)
                                   :preword `(,w1pre))
                                  stack))))))
                 (backward-walk end-padding)))))))))

