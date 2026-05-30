(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defparameter *file-contents* (read-file "data.txt"))

(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))

(defun string-to-number-list (str)
    (mapcar #'parse-integer (split-sequence:split-sequence #\Space str)))

(defparameter *items0* (mapcar #'string-to-number-list *lines*))

(defparameter *items1* (mapcar (lambda (line) (list (butlast line) (rest line))) *items0*))

(defparameter *items2* (mapcar (lambda (item)
    (mapcar (lambda (x y) (cons x y))
        (first item)
        (second item)))
    *items1*))

(defun safe-report-p (pairs)
  (let* ((diffs (mapcar (lambda (pair) (- (first pair) (cdr pair))) pairs))
      (abs-ok (every (lambda (d) (and (<= (abs d) 3) (/= d 0))) diffs)))
    (and abs-ok
      (or (every (lambda (d) (< d 0)) diffs)
       (every (lambda (d) (> d 0)) diffs)))))

(defparameter *validDiffs* (remove-if-not #'safe-report-p *items2*))

(format t "~A~%" (length *validDiffs*))