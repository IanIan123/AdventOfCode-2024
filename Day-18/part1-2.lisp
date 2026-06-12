(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-string-into-integers (input-string)
  (let* ((substrings (split-sequence:split-sequence #\, input-string)))
    (cons (parse-integer (first substrings)) (parse-integer (second substrings)))))

(defun new-position (pos direction)
  (cons (+ (car pos) (first direction)) (+ (cdr pos) (second direction))))

(defun get-new-positions (pos)
  (mapcar (lambda (item) (new-position pos item)) (list (list -1 0) (list 0 1) (list 1 0) (list 0 -1))))

(defstruct node
  pos
  parent)

(defun in-bounds-p (point)
  (and
    (>= (car point) 0)
    (>= (cdr point) 0)
    (< (car point) *cols*)
    (< (cdr point) *rows*)))

(defun first-n-points (hash-table n)
  (let ((res (make-hash-table :test #'equal))
        (count 0))
    (maphash (lambda (k v)
               (when (< count n)
                 (setf (gethash k res) t)
                 (incf count)))
             hash-table)
    res))

(defun key-in-points-at-index (hash-table index)
  "Return the key at zero-based INDEX from HASH-TABLE, or NIL if out of range."
  (let ((count 0))
    (block found
      (maphash (lambda (k v)
                 (when (= count index)
                   (return-from found k))
                 (incf count))
               hash-table)
      nil)))

(defun count-items-in-chain (endPath)
  "Count the number of items in the chain ending at endPath."
  (let ((count 0)
        (current endPath))
    (loop
      (incf count)
      (setf current (node-parent current))
      (unless current
        (return (1- count))))))

(defun move-min (obsticles visited queue)
  (if (= (length queue) 0)
      nil
      (let* ((item (pop queue))
             (point (node-pos item)))
        ;(format t "Checking point: ~a against end: ~a~%" point *end*)
        (if (and (= (car point) (car *end*)) (= (cdr point) (cdr *end*)))
            item
            (let* ((newPositions (get-new-positions point))
                   (filteredNewPositions
                    (remove-if-not (lambda (p)
                                     (and (null (gethash p visited))
                                          (null (gethash p obsticles))
                                          (in-bounds-p p)))
                                   newPositions)))
              ;(format t "New positions: ~a~%" filteredNewPositions)
              (dolist (newPos filteredNewPositions)
                (setf (gethash newPos visited) t)
                (setf queue (append queue (list (make-node :pos newPos :parent item)))))
              (move-min obsticles visited queue))))))

(defun render-grid (points endpath)
  "Display grid with obstacles (#), final path (O), and empty cells (.)."
  (let ((path-table (make-hash-table :test #'equal)))
    (when endpath
      (loop for node = endpath then (node-parent node) while node do
        (setf (gethash (node-pos node) path-table) t)))
    (format t "~%=== Grid ===~%")
    (loop for row from 0 below *rows* do
      (loop for col from 0 below *cols* do
        (let ((pos (cons col row)))
          (cond
            ((gethash pos points) (format t "#"))
            ((gethash pos path-table) (format t "O"))
            (t (format t ".")))))
      (format t "~%"))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (remove-if (lambda (s) (= (length s) 0)) (split-sequence #\Newline *file-contents*)))
(defparameter *pairs* (mapcar #'split-string-into-integers *lines*))
(defparameter *points* (let ((table (make-hash-table :test #'equal)))
  (mapc (lambda (point) (setf (gethash point table) t)) *pairs*) table))
(defparameter *cols* (1+ (apply #'max (mapcar #'car *pairs*))))
(defparameter *rows* (1+ (apply #'max (mapcar #'cdr *pairs*))))
(defparameter *limit* (if (>= (hash-table-count *points*) 1024) 1024 12))

(format t "Grid: ~ax~a~%" *cols* *rows*)
(format t "Obstacles loaded: ~a~%" (hash-table-count *points*))
(format t "First few pairs: ~a~%" (subseq *pairs* 0 (min 5 (length *pairs*))))

(defparameter *start* (cons 0 0))
(defparameter *end* (cons (1- *cols*) (1- *rows*)))
(format t "Start: ~a, End: ~a~%" *start* *end*)

;; Part 1: Find the shortest path from start to end, avoiding obstacles
(let ((points (first-n-points *points* *limit*))
      (visited (make-hash-table :test #'equal))
      (queue (list (make-node :pos *start* :parent nil))))
  (setf (gethash *start* visited) t)
  (let ((end (move-min points visited queue)))
    (render-grid points end)
    (format t "Path length: ~a~%" (count-items-in-chain end))))

;; Part 2: Find the minimum number of obstacles to remove to create a path
(block search
  (let ((total (hash-table-count *points*)))
    (loop for i from (1+ *limit*) below total
          do (let* ((points (first-n-points *points* i))
                    (visited (make-hash-table :test #'equal))
                    (queue (list (make-node :pos *start* :parent nil))))
               (setf (gethash *start* visited) t)
               (let ((path (move-min points visited queue)))
                 (when (not path)
                   (format t "No path found with ~a obstacles~%" i)
                   (let ((point (key-in-points-at-index points (1- i))))
                      (format t "Obsticle: ~a,~a~%" (car point) (cdr point)))
                   (return-from search)))))))