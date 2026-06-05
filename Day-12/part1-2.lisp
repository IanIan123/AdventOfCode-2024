(ql:quickload "split-sequence")
(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun get-direction (index)
  (nth index (list (cons -1 0) (cons 0 1) (cons 1 0) (cons 0 -1))))

(defun new-position (pos direction)
  (cons (+ (first pos) (first direction)) (+ (cdr pos) (cdr direction))))

(defun in-bounds-p (point)
  (and
    (>= (car point) 0)
    (>= (cdr point) 0)
    (< (car point) (length (first *grid*)))
    (< (cdr point) (length *grid*)))))

(defun grid-char (point)
  (nth (car point) (nth (cdr point) *grid*)))

(defun same-char-p (point p)
  (string= (grid-char point) (grid-char p)))

(defun addToSpace (x y)
  (let* (
    (pos (cons x y))
    (matching-spaces
      (remove-duplicates
        (remove nil
          (mapcar (lambda (n)
            (let ((neighbor (new-position pos (get-direction n))))
              (when (and (in-bounds-p neighbor) (same-char-p neighbor pos))
                (gethash neighbor *point-spaces*))))
          '(0 3)))
        :test #'eq))
    (target-space
      (if matching-spaces
        (let ((target-space (first matching-spaces)))
          (dolist (other-space (rest matching-spaces))
            (maphash (lambda (k v)
              (setf (gethash k target-space) v)
              (setf (gethash k *point-spaces*) target-space))
              other-space))
          target-space)
        (make-hash-table :test #'equal))))
  (setf (gethash pos target-space) (grid-char pos))
  (setf (gethash pos *point-spaces*) target-space)))

(defun get-spaces ()
  (let ((spaces (make-hash-table :test #'eq)))
    (maphash (lambda (_ space)
      (setf (gethash space spaces) space))
      *point-spaces*)
    (loop for space being the hash-values of spaces collect space)))

(defun space-perimeter (space)
  (let ((perimeter 0))
    (maphash (lambda (pos _)
      (dolist (n '(0 1 2 3))
        (let ((neighbor (new-position pos (get-direction n))))
          (unless (and (in-bounds-p neighbor) (gethash neighbor space))
            (incf perimeter)))))
      space)
    perimeter))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar #'string-to-string-list *lines*))
(defparameter *point-spaces* (make-hash-table :test #'equal))

(dotimes (y (length *grid*))
  (dotimes (x (length (first *grid*)))
    (addToSpace x y)))

(defparameter *spaces*
  (let ((spaces '()))
    (maphash (lambda (_ space)
      (pushnew space spaces :test #'eq))
      *point-spaces*)
    spaces))

(format t "~a~%" (reduce #'+
  (mapcar (lambda (space)
    (* (space-perimeter space) (hash-table-count space)))
  *spaces*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun point-missing-edges (space pos)
  (let ((edges '()))
    (dolist (dir '((-1 . 0) (0 . 1) (1 . 0) (0 . -1)))
      (unless (gethash (new-position pos dir) space)
        (push (cond
                ((equal dir '(-1 . 0)) :left)
                ((equal dir '(0 . 1)) :bottom)
                ((equal dir '(1 . 0)) :right)
                ((equal dir '(0 . -1)) :top))
              edges)))
    edges))

(defun perimeter-points (space)
  (let ((perimeter-points '()))
    (maphash (lambda (pos _)
               (let ((missing-edges (point-missing-edges space pos)))
                 (when missing-edges
                   (push (cons pos missing-edges) perimeter-points))))
             space)
    (nreverse perimeter-points)))

(defun group-by-x (points)
  (let ((table (make-hash-table)))
    (dolist (pt points)
      (let ((x (first pt))
            (y (cdr pt)))
        (push y (gethash x table ()))))
    (let ((result '()))
      (maphash (lambda (x ys)
                 (push (cons x (list ys)) result))
               table)
      result)))

(defun contiguous-runs (nums9)
  (let ((nums2 (sort (copy-list nums9) #'<)))
    (let ((runs '())
          (current (list (first nums2))))
      (dolist (n (rest nums2))
        (if (= n (1+ (first current)))
         (progn
            (push n current))
            (progn
              (push (nreverse current) runs)
              (setf current (list n)))))
      (push (nreverse current) runs)
      runs)))

(defun group-contiguous-blocks (points)
  (mapcar (lambda (x-group)
            (let ((x (car x-group))
                  (ys (cadr x-group)))
              (cons x (list (contiguous-runs ys)))))
          (group-by-x points)))

(defun sides-from-grouped (grouped)
  (reduce #'+ grouped
          :key (lambda (x-group)
                 (length (second x-group)))
          :initial-value 0))

(defun swap-dims (points)
  (mapcar (lambda (pt) (cons (cdr pt) (car pt))) points))

(defun points-with-missing-edge (points edge)
  (remove-if-not (lambda (point-edge)
                   (member edge (cdr point-edge) :test #'eq))
                 points))

(defun get-points (pointsWithEdges)
  (mapcar #'car pointsWithEdges))

(defun points-for-edge (points edge)
  (get-points (points-with-missing-edge points edge)))

(defun side-count (points edge &optional (swap-p nil))
  (let ((pts (points-for-edge points edge)))
    (when swap-p
      (setf pts (swap-dims pts)))
    (sides-from-grouped (group-contiguous-blocks pts))))

(defun count-all-sides (points)
  (+ (side-count points :left)
     (side-count points :right)
     (side-count points :top t)
     (side-count points :bottom t)))

(format t "~a~%" (reduce #'+
  (mapcar (lambda (space)
    (progn
    (format t "~a x ~a~%" (hash-table-count space) (count-all-sides (perimeter-points space)))
      (* (count-all-sides (perimeter-points space)) (hash-table-count space))))
  *spaces*)))