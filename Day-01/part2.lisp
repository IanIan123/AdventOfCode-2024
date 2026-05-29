(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defparameter *file-contents* (read-file "data.txt"))

(defparameter *lines* (split-sequence #\Newline *file-contents* :remove-empty-subseqs t))

(defun split-line-to-pair (line)
  (let ((parts (split-sequence #\Space line :remove-empty-subseqs t)))
    (cons (parse-integer(first parts)) (parse-integer(second parts)))))

(defparameter *pairs* (mapcar #'split-line-to-pair *lines*))

(defparameter *items0* (mapcar #'first *pairs*))
(defparameter *items1* (mapcar #'rest *pairs*))

(defparameter *counts*
  (let ((counts (make-hash-table)))
    (dolist (item *items1*)
      (when (member item *items0*)
        (incf (gethash item counts 0))))
    counts))

(defparameter *items3*
  (let ((products '()))
    (maphash (lambda (key value) (push (* key value) products)) *counts*)
    products))

(defparameter *sum* (reduce #'+ *items3*))

(format t "~A~%" *sum*)