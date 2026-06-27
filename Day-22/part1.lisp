(ql:quickload "split-sequence")
(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun parse-lines (filename)
  (let ((file-contents (read-file filename)))
    (mapcar #'parse-integer
            (split-sequence:split-sequence #\Newline file-contents
                                           :remove-empty-subseqs t))))

(defun mix (a b)
  (logxor a b))

(defun prune (number)
  (mod number 16777216))

(defparameter *step-functions*
  (list
   (lambda (n) (ash n 6))
   (lambda (n) (ash n -5))
   (lambda (n) (ash n 11))))

(defun transform-number (number)
  (reduce (lambda (value fn)
            (prune (mix (funcall fn value) value)))
          *step-functions*
          :initial-value number))

(defun run-transform-n-times (number n)
  (do ((value number (transform-number value))
       (i 0 (1+ i)))
      ((= i n) value)
   ))

(defun sum-numbers (numbers)
  (reduce #'+ (mapcar (lambda (number)
                        (run-transform-n-times number 2000))
                      numbers)
          :initial-value 0))

(defparameter *numbers* (parse-lines "data.txt"))
(defparameter *sum* (sum-numbers *numbers*))

(format t "Part 1: ~a~%" *sum*)