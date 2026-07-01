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
  (let ((queue '()) (digit 0) (diff 0) (prevDigit 0))
    (loop for i from 0 repeat (1+ n)
          for value = number then (transform-number value)
          do
            (setf digit (mod value 10))
            (setf diff (- digit prevDigit))
            (when (> i 0)
              (push diff queue))
            (when (> (length queue) 4)
              (setf queue (butlast queue)))
            (if (= (length queue) 4)
              (progn
                (unless (gethash queue *patterns*)
                  (setf (gethash queue *patterns*) (make-hash-table :test 'equal)))
                (let ((dict (gethash queue *patterns*)))
                  (unless (gethash number dict)
                    (setf (gethash number dict) digit)))))
            (setf prevDigit digit)
         ; (format t "~a ~a ~a ~a ~a~%" number value digit diff queue)
          finally (return value))))

(defun sum-numbers (numbers)
  (reduce #'+ (mapcar (lambda (number)
                        (run-transform-n-times number 2000))
                      numbers)
          :initial-value 0))

(defun highest-pattern-total (patterns)
  (let ((best-pattern nil)
        (best-total 0))
    (maphash (lambda (pattern inner-dict)
               (let ((total 0))
                 (maphash (lambda (key value)
                            (declare (ignore key))
                            (incf total value))
                          inner-dict)
                 (when (> total best-total)
                   (setf best-total total)
                   (setf best-pattern pattern))))
             patterns)
    (list (nreverse best-pattern) best-total)))

(defparameter *patterns* (make-hash-table :test 'equal))
(defparameter *numbers* (parse-lines "data.txt"))
(defparameter *sum* (sum-numbers *numbers*))

(format t "Part 1: ~a~%" *sum*)
(format t "Part 2: ~a~%" (highest-pattern-total *patterns*))