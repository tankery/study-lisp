;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Square array rotation
;;; > (quarter-turn #2A((a b) (c d)))
;;; #2A((C A) (D B))
(defun quarter-turn (sqr-arr)
  (and (arrayp sqr-arr)
       (let* ((dims (array-dimensions sqr-arr))
              (x-dim (second dims))
              (y-dim (first dims)))
         (and (eql (length dims) 2)
              (and (eql x-dim y-dim)
                   (setf qrt-arr (make-array dims))
                   (do ((y 0 (+ y 1)))
                       ((eql y y-dim) qrt-arr)
                     (do ((x 0 (+ x 1)))
                         ((eql x x-dim) qrt-arr)
                       (setf (aref qrt-arr x (- y-dim 1 y))
                             (aref sqr-arr y x)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. copy/reverse list using #'reduce
;;;
(defun copy-lst (lst)
  (reduce 'cons lst :from-end t :initial-value nil))

(defun reverse-lst (lst)
  (reduce #'(lambda (a b)
              (cons b a))
          lst
          :initial-value nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. copy/find three branches tree
;;;
(defstruct (node-3)
  elt
  (l nil)
  (m nil)
  (r nil))

;;; Insert element to tree.
;;; > (setf nums nil)
;;; > (dolist (x '(5 8 4 2 1 9 6 7 3)) (setf nums (tree-3-insert x nums)))
(defun tree-3-insert (obj tree)
  (if (null tree)
      (make-node-3 :elt obj)
      (let ((sel (random 3))
            (elt (node-3-elt tree)))
        (cond ((eql obj elt)
               tree)
              ((eql sel 0)
               (make-node-3
                 :elt elt
                 :l (tree-3-insert obj (node-3-l tree))
                 :m (node-3-m tree)
                 :r (node-3-r tree)))
              ((eql sel 1)
               (make-node-3
                 :elt elt
                 :l (node-3-l tree)
                 :m (tree-3-insert obj (node-3-m tree))
                 :r (node-3-r tree)))
              ((eql sel 2)
               (make-node-3
                 :elt elt
                 :l (node-3-l tree)
                 :m (node-3-m tree)
                 :r (tree-3-insert obj (node-3-r tree))))))))

;;; Copy tree
(defun tree-3-copy (tree)
  (if (null tree)
      nil
      (make-node-3
        :elt (node-3-elt tree)
        :l (tree-3-copy (node-3-l tree))
        :m (tree-3-copy (node-3-m tree))
        :r (tree-3-copy (node-3-r tree)))))

;;; Find object in tree
(defun tree-3-find (obj tree)
  (if (null tree)
      nil
      (let ((elt (node-3-elt tree)))
        (if (eql elt obj)
            tree
            (or (tree-3-find obj (node-3-l tree))
                (tree-3-find obj (node-3-m tree))
                (tree-3-find obj (node-3-r tree)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 4. expand BST
;;; > (setf nums nil)
;;; > (dolist (x '(5 8 4 2 1 9 6 7 3)) (setf nums (bst-insert x nums #'<)))
;;; > (expand-bst nums #'<)
;;; (1 2 3 4 5 6 7 8 9)
;;;
(load "../example/4.7.binary-search-tree.lisp")
(defun expand-bst (bst <)
  (if (null bst)
      nil
      (let ((ll (expand-bst (node-l bst) <))
            (obj (node-elt bst))
            (rl (expand-bst (node-r bst) <)))
        (if (null ll)
            (if (null rl)
                (list obj)
                (if (funcall < obj (first rl))
                    (append (list obj) rl)
                    (append rl (list obj))))
            (if (funcall < (first ll) obj)
                (append ll (list obj) rl)
                (append rl (list obj) ll))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 5. adjoin BST
;;; > (setf nums nil)
;;; > (dolist (x '(5 8 4 5 2 1 9 4 6 7 5 2 3)) (setf nums (bst-adjoin x nums #'<)))
;;; > (expand-bst nums #'<)
;;; (1 2 3 4 5 6 7 8 9)
;;;
(load "../example/4.7.binary-search-tree.lisp")
(defun bst-adjoin (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                  :elt elt
                  :l   (bst-adjoin obj (node-l bst) <)
                  :r   (node-r bst))
                (make-node
                  :elt elt
                  :l   (node-l bst)
                  :r   (bst-adjoin obj (node-r bst) <)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 6. assoc-list and hash table conversion.
;;;

;;; assoc-list to hash table
;;; > (assoc-hash '((a . "hello") (b . "world")))
;;; #S(HASH-TABLE :TEST FASTHASH-EQL (A . "hello") (B . "world"))
;;;
(defun assoc-hash (lst)
  (if (null lst)
      (make-hash-table)
      (let ((head (car lst))
            (ht (assoc-hash (cdr lst))))
        (setf (gethash (car head) ht) (cdr head))
        ht)))

;;; hash table to assoc-list
;;; > (hash-assoc #s(hash-table :test fasthash-eql (a . "hello") (b . "world")))
;;; ((A . "hello") (B . "world"))
(defun hash-assoc (ht)
  (let ((lst nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) lst))
             ht)
    lst))
