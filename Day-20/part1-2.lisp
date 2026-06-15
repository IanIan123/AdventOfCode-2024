(ql:quickload "split-sequence")
(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun count-frequencies (items)
  "Return a hash table with counts for each item in ITEMS."
  (let ((freq (make-hash-table :test 'eql)))
    (dolist (item items)
      (incf (gethash item freq 0)))
    freq))

(defun print-savings-summary (savings-counts)
  "Print counts for each saving value in ascending order."
  (let ((freq (count-frequencies savings-counts)))
    (dolist (saving (sort (loop for k being the hash-keys of freq collect k) #'<))
      (let ((count (gethash saving freq))
            (label (if (= (gethash saving freq) 1) "cheat" "cheats")))
        (format t "There are ~d ~a that save ~d picoseconds.~%" count label saving)))))

(defun render-grid-with-start-end-and-direction (points start-point end-point more-points width height more-points-char)
  "Render a grid with the given points, start point, end point, direction points, width, and height."
  (let ((grid (make-array (list height width) :initial-element #\.)))
    ;; Populate the grid with points
    (maphash (lambda (key value)
               (setf (aref grid (first key) (second key)) #\#))
             points)
    ;; Populate the grid with direction points based on keys
    (maphash (lambda (key value)
               (setf (aref grid (first key) (second key)) more-points-char))
             more-points)
    ;; Set the start point and end point
    (setf (aref grid (first start-point) (second start-point)) #\S)
    (setf (aref grid (first end-point) (second end-point)) #\E)
    ;; Print the grid
    (dotimes (i height)
      (dotimes (j width)
        (princ (aref grid i j)))
      (terpri))))

(defun lookup-char (pos grid)
  (nth (second pos) (nth (first pos) grid)))

(defun get-items (str)
  (let* ((items (make-hash-table :test 'equal))
         (lines (split-sequence:split-sequence #\Newline str))
         (grid (mapcar #'string-to-string-list lines)))
    (defparameter *width* (length (first grid)))
    (defparameter *height* (length grid))
    (dotimes (i *height*)
      (dotimes (j *width*)
        (let* ((c (lookup-char (list i j) grid))
               (ch (and c (char c 0))))
          (case ch
            (#\S (defparameter *start* (list i j)))
            (#\E (defparameter *end* (list i j)))
            (#\# (setf (gethash (list i j) items) c))
            (t nil)))))
    items))

(defun get-new-positions (pos)
  (mapcar (lambda (item) (list (new-position pos item) item)) (get-directions)))

(defun new-position (pos direction)
  (list (+ (first pos) (first direction)) (+ (second pos) (second direction))))

(defun get-directions ()
  (list (list -1 0) (list 0 1) (list 1 0) (list 0 -1)))

(defun move-min (queue visited)
  (if (= (length queue) 0)
    nil
    (let* ((item (pop queue)) (point (first item)))
      (let* ((newPositions (get-new-positions (first item)))
        (filteredNewPositions (remove-if-not (lambda (p)
          (and (null (gethash (first p) visited))
            (null (gethash (first p) *walls*)))) newPositions)))
        (dolist (newPos filteredNewPositions)
          (let ((point (first newPos)) (direction (second newPos)) (dist (1+ (second item))))
            (setf (gethash point visited) (cons dist item))
            (setf queue (append queue (list (list point dist)))))))
      (move-min queue visited))))

; parse data, look up start, end and walls
(defparameter *file-contents* (read-file "data.txt"))
(defparameter *walls* (get-items *file-contents*))

; create distances from start
(defparameter *distances-from-start* (make-hash-table :test 'equal))
(setf (gethash *start* *distances-from-start*) (cons 0 nil))
(let ((queue (list (list *start* 0))))
  (move-min queue *distances-from-start*))

; create distances from end
(defparameter *distances-from-end* (make-hash-table :test 'equal))
(setf (gethash *end* *distances-from-end*) (cons 0 nil))
(let ((queue (list (list *end* 0))))
  (move-min queue *distances-from-end*))

; shortest path with no cheats activated
(defparameter *noCheatCount* (car (gethash *end* *distances-from-start*)))
(format t "No-cheat count: ~a~%" *noCheatCount*)

; iterate over all point pairs and calculate the path length
(defun find-cheats-with-max-distance (max-manhattenDistance)
  "Find all cheat distances up to max-manhattenDistance Manhattan distance."
  (let ((times '()))
    (let ((all-points (loop for k being the hash-keys of *distances-from-start* collect k)))
      (loop for (a . rest) on all-points do
        (loop for b in rest do
          (let ((manhattenDistance (+ (abs (- (first a) (first b))) (abs (- (second a) (second b))))))
            (when (<= manhattenDistance max-manhattenDistance)
              (let* ((dist-from-start (car (gethash a *distances-from-start*)))
                  (dist-from-end (car (gethash b *distances-from-end*)))
                  (total-distance (+ dist-from-start manhattenDistance dist-from-end)))
                (push total-distance times)))))))
    times))

; solve-part: wrapper function for each part
(defun solve-part (max-distance part-label)
  "Find cheats up to max-distance and count those saving >= 100 picoseconds."
  (let* ((times (find-cheats-with-max-distance max-distance))
         (savings (mapcar (lambda (item) (- *noCheatCount* item)) times))
         (savings-100+ (remove-if-not (lambda (saving) (>= saving 100)) savings)))
    (format t "~a Savings: ~a~%" part-label (length savings-100+))))

; Part 1: max distance 2
(solve-part 2 "Part 1")

; Part 2: max distance 20
(solve-part 20 "Part 2")
;(print-savings-summary *savings*)

;(render-grid-with-start-end-and-direction *walls* *start* *end* *distances-from-start* *width* *height* ">")