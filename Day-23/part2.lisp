(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-line-to-pair (line)
  (split-sequence:split-sequence #\- line))

(defun add-edge (a b)
  (flet ((link (from to)
           (let ((set (or (gethash from *graph*)
                          (setf (gethash from *graph*) (make-hash-table :test #'equal)))))
             (setf (gethash to set) t))))
    (link a b)
    (link b a)))

(defun hash-keys (table)
  (loop for k being the hash-keys of table collect k))

(defun get-connected (s v)
  (remove-if-not (lambda (item) (gethash item (gethash v *graph*))) s))

(defun find-cliques (r p e)
  (if (and (null p) (null e))
      (push r *cliques*)
      (dolist (v p)
        (progn
          (find-cliques (cons v r) (get-connected p v) (get-connected e v))
          (setf p (remove v p))
          (setf e (cons v e))))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *pairs* (mapcar #'split-line-to-pair *lines*))
(defparameter *graph* (make-hash-table :test #'equal))
(defparameter *cliques* nil)

(loop for (a b) in *pairs*
    do (add-edge a b))

(find-cliques nil (hash-keys *graph*) nil)
(format nil "~{~a~^,~}"
  (sort (copy-list (first (sort *cliques* #'> :key #'length))) #'string<))