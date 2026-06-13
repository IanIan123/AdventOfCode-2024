(ql:quickload "split-sequence")
(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defstruct trie-node
  (children (make-hash-table) :type hash-table)
  (is-end nil :type boolean))

(defun make-trie ()
  (make-trie-node))

(defun trie-insert (trie word)
  (let ((node trie))
    (loop for ch across word do
      (let ((next (gethash ch (trie-node-children node))))
        (unless next
          (setf next (make-trie-node))
          (setf (gethash ch (trie-node-children node)) next))
        (setf node next)))
    (setf (trie-node-is-end node) t)))

(defun build-trie (patterns)
  (let ((trie (make-trie)))
    (dolist (p patterns) (trie-insert trie p))
    trie))

(defun trie-matches (trie string start)
  "Return list of end positions (exclusive) of patterns matching string starting at start."
  (let ((node trie)
        (matches '()))
    (loop for i from start below (length string) do
      (let ((next (gethash (char string i) (trie-node-children node))))
        (unless next (return))
        (setf node next)
        (when (trie-node-is-end node)
          (push (1+ i) matches))))
    (nreverse matches)))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (remove-if (lambda (s) (= (length s) 0)) (split-sequence #\Newline *file-contents*)))
(defparameter *patterns* (mapcar (lambda (p) (string-trim " " p)) (split-sequence #\, (first *lines*))))
(defparameter *trie* (build-trie *patterns*))
(defparameter *designs* (rest *lines*))

(defparameter *ways* (list))
(dolist (design *designs*)
  (let ((totals (make-array
                  (1+ (length design))
                  :element-type 'integer
                  :initial-element 0)))
    (setf (aref totals 0) 1)
    (loop for newPos from 0 below (length design) do
      (let ((val (aref totals newPos)))
        (when (> val 0)
          (let ((matches (trie-matches *trie* design newPos)))
            (dolist (matchIndex matches)
              (incf (aref totals matchIndex) val))))))
    (push (aref totals (length design)) *ways*)))

; Part 1
(format t "Ways: ~a~%"  (count-if (lambda (x) (/= x 0)) *ways*))

; Part 2
(format t "Ways: ~a~%"  (reduce #'+ *ways*))