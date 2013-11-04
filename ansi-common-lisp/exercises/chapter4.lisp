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
(defstruct (node)
  elt
  (l nil)
  (m nil)
  (r nil))

;;; Insert element to tree.
;;; > (setf nums nil)
;;; > (dolist (x '(5 8 4 2 1 9 6 7 3)) (setf nums (tree-3-insert x nums)))
(defun tree-3-insert (obj tree)
  (if (null tree)
      (make-node :elt obj)
      (let ((sel (random 3))
            (elt (node-elt tree)))
        (cond ((eql obj elt)
               tree)
              ((eql sel 0)
               (make-node
                 :elt elt
                 :l (tree-3-insert obj (node-l tree))
                 :m (node-m tree)
                 :r (node-r tree)))
              ((eql sel 1)
               (make-node
                 :elt elt
                 :l (node-l tree)
                 :m (tree-3-insert obj (node-m tree))
                 :r (node-r tree)))
              ((eql sel 2)
               (make-node
                 :elt elt
                 :l (node-l tree)
                 :m (node-m tree)
                 :r (tree-3-insert obj (node-r tree))))))))

;;; Copy tree
(defun tree-3-copy (tree)
  (if (null tree)
      nil
      (make-node
        :elt (node-elt tree)
        :l (node-l tree)
        :m (node-m tree)
        :r (node-r tree))))

;;; Find object in tree
(defun tree-3-find (obj tree)
  (if (null tree)
      nil
      (let ((elt (node-elt tree)))
        (if (eql elt obj)
            tree
            (or (tree-3-find obj (node-l tree))
                (tree-3-find obj (node-m tree))
                (tree-3-find obj (node-r tree)))))))
