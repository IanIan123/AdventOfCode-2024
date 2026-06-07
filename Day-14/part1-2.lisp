(ql:quickload "split-sequence")
(use-package :split-sequence)

(defstruct b
  x
  y)

(defstruct a
  pos
  vel)

(defun copy-b (b)
  (make-b :x (b-x b) :y (b-y b)))

(defun copy-a (a)
  (make-a :pos (copy-b (a-pos a)) :vel (copy-b (a-vel a))))

(defun deep-copy-robots (robots)
  (mapcar #'copy-a robots))

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *pairs1*
  (mapcar (lambda (x) (split-sequence #\Space x)) *lines*))
(defparameter *pairs2*
  (mapcar (lambda (x) (cons (subseq (car x) 2) (subseq (second x) 2))) *pairs1*))
(defparameter *pairs3*
  (mapcar
    (lambda (x) (list (split-sequence #\, (car x)) (split-sequence #\, (cdr x)))) *pairs2*))
(defparameter *robots*
  (mapcar (lambda (x)
    (let ((p (first x)) (v (second x)))
      (make-a
        :pos (make-b :x (parse-integer (first p)) :y (parse-integer (second p)))
        :vel (make-b :x  (parse-integer (first v)) :y (parse-integer (second v))))))
          *pairs3*))

(defun render (robots width height)
  (let ((grid (make-array (list height width) :initial-element 0)))
    (dolist (r robots)
      (let ((x (b-x (a-pos r)))
            (y (b-y (a-pos r))))
        (incf (aref grid y x))))
    (dotimes (y height)
      (terpri)
      (dotimes (x width)
        (let ((n (aref grid y x)))
          (princ (if (zerop n) #\. (digit-char n))))))))

(defun update (robots width height)
  (dolist (r robots)
    (let ((x (b-x (a-pos r)))
          (y (b-y (a-pos r)))
          (vx (b-x (a-vel r)))
          (vy (b-y (a-vel r))))
      (setf (b-x (a-pos r)) (mod (+ x vx) width))
      (setf (b-y (a-pos r)) (mod (+ y vy) height))
  )))

(defun evaluate (robots width height)
  (let ((midx (floor width 2)) (midy (floor height 2))
        (topLeft 0) (topRight 0) (bottomLeft 0) (bottomRight 0))
    (dolist (r robots)
      (let ((x (b-x (a-pos r)))
            (y (b-y (a-pos r))))
        (cond
          ((and (< x midx) (< y midy)) (incf topLeft))
          ((and (< x midx) (> y midy)) (incf bottomLeft))
          ((and (> x midx) (< y midy)) (incf topRight))
          ((and (> x midx) (> y midy)) (incf bottomRight)))))
          (format t "Top Left: ~A~%" topLeft)
          (format t "Top Right: ~A~%" topRight)
          (format t "Bottom Left: ~A~%" bottomLeft)
          (format t "Bottom Right: ~A~%" bottomRight)
          (* topLeft topRight bottomLeft bottomRight)))

(defparameter *part1* (let ((rs (deep-copy-robots *robots*)) (width 101) (height 103) (seconds 100))
  (dotimes (i seconds)
  (update rs width height))
    (evaluate rs width height)))

;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;

(defun evaluate2 (robots width height &optional seconds)
  (let ((positions (make-hash-table :test #'equal)))
    (dolist (r robots)
      (setf (gethash (cons (b-x (a-pos r)) (b-y (a-pos r))) positions) t))
    (let ((with-neighbour 0))
      (dolist (r robots)
        (let ((x (b-x (a-pos r)))
              (y (b-y (a-pos r))))
          (when (or (gethash (cons (1+ x) y) positions)
                    (gethash (cons (1- x) y) positions)
                    (gethash (cons x (1+ y)) positions)
                    (gethash (cons x (1- y)) positions))
            (incf with-neighbour))))
      (when (>= with-neighbour (floor (length robots) 2))
        (format t "Found at ~A seconds~%" (or seconds 0))
        t))))

(defparameter *part2* (let ((rs (deep-copy-robots *robots*)) (width 101) (height 103))
  (loop for s from 1 do
    (progn
      (update rs width height)
      (when (evaluate2 rs width height s)
        (progn
          (render rs width height)
          (return s)))))))

(format t "Part 1: ~A~%" *part1*)
(format t "Part 2: ~A~%" *part2*)