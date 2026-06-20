(ql:quickload "split-sequence")
(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun convert-if-number (char)
  (if (every #'digit-char-p char)
      (parse-integer char)
      char))

(defun split-string-characters (str)
  (mapcar (lambda (char) (convert-if-number(string char))) (coerce str 'list)))

(defun lookup-char (pos grid)
  (nth (second pos) (nth (first pos) grid)))

(defun get-items (str)
  (let* ((items (make-hash-table :test 'equal))
    (lines (split-sequence:split-sequence #\Newline str)) (grid (mapcar #'string-to-string-list lines)))
      (defparameter *width* (length (first grid)))
      (defparameter *height* (length grid))
      (dotimes (i *height*)
        (dotimes (j *width*)
          (let ((c (lookup-char (list i j) grid)))
            (if (string-equal c "#")
              (setf (gethash (list i j) items) c)))))
  items))

(defun new-position (pos direction)
  (list (+ (first pos) (first direction)) (+ (second pos) (second direction))))

(defun get-new-positions (pos)
  (mapcar (lambda (item) (list (new-position pos item) item)) (get-directions)))

(defun get-directions ()
  (list (list 1 0) (list 0 -1) (list -1 0) (list 0 1) ))

(defun print-hash-table (hash-table)
  (maphash (lambda (key value)
             (if (hash-table-p value)
                 (format t "Key: ~a, Values: ~{~a~^, ~}~%"
                         key
                         (loop for k being the hash-keys of value
                               collect k))
                 (format t "Key: ~a, Value: ~a~%" key value)))
           hash-table))

(defun filter-hash-table-by-value (hash-table predicate)
  (let ((result (make-hash-table :test (hash-table-test hash-table))))
    (maphash (lambda (key value)
               (when (funcall predicate value)
                 (setf (gethash key result) value)))
             hash-table)
    result))

(defun flatten-linked-list (item)
  "Flatten a linked list into a single list. Each node's third item points to the next node."
  (if (null item)
      nil
      (append (list (second item)) (flatten-linked-list (third item)))))

(defun flatten (lst)
  "Flatten a nested list."
  (cond
    ((null lst) nil)  ;; Base case: empty list
    ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))  ;; If the first element is an atom, add it to the result
    (t (append (flatten (car lst)) (flatten (cdr lst))))))  ;; If the first element is a list, flatten it and append it to the result

(defun build-all-paths (point start preds)
  "Walk PREDS backwards from POINT to START, returning every
   shortest path as a list of directions (start -> point)."
  (if (equal point start)
      (list nil)
      (let ((result nil))
        (dolist (pred (gethash point preds))
          (destructuring-bind (ppoint direction) pred
            (dolist (subpath (build-all-paths ppoint start preds))
              (push (append subpath (list direction)) result))))
        result)))

(defun move-all-min (end start walls)
  "BFS from START to END. Returns a list of all shortest paths;
   each path is a list of directions, in order, start -> end."
  (let ((dist (make-hash-table :test 'equal))
        (preds (make-hash-table :test 'equal))
        (queue (list start)))
    (setf (gethash start dist) 0)
    (loop while queue do
      (let* ((point (pop queue))
             (d (gethash point dist))
             (new-positions (get-new-positions point)))
        (dolist (np new-positions)
          (let ((npoint (first np))
                (direction (second np)))
            (unless (gethash npoint walls)
              (let ((existing (gethash npoint dist)))
                (cond
                  ;; first time we've reached this cell -> it's at d+1
                  ((null existing)
                   (setf (gethash npoint dist) (1+ d))
                   (setf (gethash npoint preds) (list (list point direction)))
                   (setf queue (append queue (list npoint))))
                  ;; reached again, but via an equally-short route -> extra predecessor
                  ((= existing (1+ d))
                   (push (list point direction) (gethash npoint preds)))
                  ;; otherwise it's a longer route, ignore
                  (t nil))))))))
    (build-all-paths end start preds)))

(defun build-paths-table (grid items)
  "Build a hash table mapping (from-char to-char) -> list of shortest
   direction-paths, for every reachable pair of non-wall cells in GRID."
  (let* ((paths (make-hash-table :test 'equal))
         (width (length (first grid)))
         (height (length grid)))
    (dotimes (i width)
      (dotimes (j height)
        (dotimes (i0 width)
          (dotimes (j0 height)
            (when (and (null (gethash (list j i) items))
                       (null (gethash (list j0 i0) items)))
              (let ((key (list (lookup-char (list j i) grid)
                                (lookup-char (list j0 i0) grid))))
                (if (and (= i i0) (= j j0))
                    ;; same cell: no movement, just press A
                    (setf (gethash key paths) (list nil))
                    (let* ((all-routes (move-all-min (list j0 i0) (list j i) items))
                           (all-directionsC
                             (mapcar (lambda (directions)
                                       (mapcar (lambda (step)
                                                 (cond
                                                   ((equal step '(1 0))  "v")
                                                   ((equal step '(0 1))  ">")
                                                   ((equal step '(-1 0)) "^")
                                                   (t                    "<")))
                                               directions))
                                     all-routes)))
                      (setf (gethash key paths) all-directionsC)))))))))
    paths))

(defun iterate (sequence pointer depth)
  (if (= depth 0)
    (cons (length sequence) pointer)
    (let ((output 0) (innerPointer pointer))
      (dolist (c sequence)
        (let* ((key (list (convert-if-number pointer) (convert-if-number c)))
               (cacheKey (list key depth))
               (cachedValue (gethash cacheKey *cache* nil)))
          (if cachedValue
            (progn
              (setf *cache-count* (1+ *cache-count*))
              (setf output (+ output (car cachedValue)))
              (setf innerPointer (cdr cachedValue))
              (setf pointer c))
         (let* ((seqs (or (gethash key *paths-controller*) (list nil)))
       (best nil))
              ;; try every shortest path for this transition, keep the cheapest
              (dolist (seq0 seqs)
                (let* ((output0 (list (append seq0 (list "A"))))
                       (output1 (iterate (flatten output0) innerPointer (1- depth))))
                  (when (or (null best) (< (car output1) (car best)))
                    (setf best output1))))
              (setf output (+ output (car best)))
              (setf innerPointer (cdr best))
              (setf pointer c)
              (setf (gethash cacheKey *cache*) best)
              (setf *no-cache-count* (1+ *no-cache-count*))))))
      (cons output innerPointer))))

(defun calc (code depth)
  (let ((pointer "A") (total 0))
    (dolist (c (string-to-string-list code))
      (let* ((key (list (convert-if-number pointer) (convert-if-number c)))
             (seqs (or (gethash key *paths-keypad*) (list nil)))
             (best nil))
        (dolist (seq0 seqs)
          (let* ((subseq (append seq0 (list "A")))
                 (result (iterate subseq "A" depth)))
            (when (or (null best) (< (car result) best))
              (setf best (car result)))))
        (setf total (+ total best))
        (setf pointer c)))
    total))

(defun code-initial-value (code depth)
  (calc code depth))

(defun code-multiplier (code)
  (parse-integer code :junk-allowed t))

(defun code-weighted-value (code depth)
  (let* ((initial (code-initial-value code depth))
         (multiplier (code-multiplier code))
         (product (* initial multiplier)))
    (format t "Code: ~a, initial: ~d, multiplier: ~d, product: ~d~%"
            code initial multiplier product)
    product))

(defun total-weighted-score (depth)
  (reduce #'+ (mapcar (lambda (code) (code-weighted-value code depth)) *codes*)
          :initial-value 0))

(defun run-part (part-number depth)
  (format t "~%--- Part ~d (depth ~d) ---~%" part-number depth)
  (format t "Total weighted score: ~d~%" (total-weighted-score depth))
  (format t "Cache count: ~d, No cache count: ~d ~%" *cache-count* *no-cache-count*))

(defparameter *file-contents-keypad* (read-file "data-keypad.txt"))
(defparameter *lines-keypad* (split-sequence:split-sequence #\Newline *file-contents-keypad*))
(defparameter *grid-keypad* (mapcar #'split-string-characters *lines-keypad*))
(defparameter *items-keypad* (get-items *file-contents-keypad*))
(defparameter *paths-keypad* (build-paths-table *grid-keypad* *items-keypad*))

(defparameter *file-contents-controller* (read-file "data-controller.txt"))
(defparameter *lines-controller* (split-sequence:split-sequence #\Newline *file-contents-controller*))
(defparameter *grid-controller* (mapcar #'split-string-characters *lines-controller*))
(defparameter *items-controller* (get-items *file-contents-controller*))
(defparameter *paths-controller* (build-paths-table *grid-controller* *items-controller*))

(defparameter *cache* (make-hash-table :test 'equal))
(defparameter *cache-count* 0)
(defparameter *no-cache-count* 0)

(defparameter *codes* '("029A" "980A" "179A" "456A" "379A")) ;test data
(defparameter *codes* '("286A" "480A" "140A" "413A" "964A"))

(run-part 1 2)
(run-part 2 25)