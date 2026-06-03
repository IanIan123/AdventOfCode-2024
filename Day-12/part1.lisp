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
    (< (car point) (length *grid*))
    (< (cdr point) (length (car *grid*)))))

(defun grid-char (point)
  (nth (cdr point) (nth (car point) *grid*)))

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