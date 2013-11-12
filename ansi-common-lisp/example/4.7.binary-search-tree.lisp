;;;; Binary Search Tree (BST): Lookup, insertion and remove
;;;; In addition, I write a function to print the BST.

;;; Struct of tree node
(defstruct (node (:print-function
                   (lambda (n s d)
                     (bst-print s n))))
  elt (l nil) (r nil))

;;; instertion
;;; > (setf nums nil)
;;; > (dolist (x '(5 8 4 2 1 9 6 7 3)) (setf nums (bst-insert x nums #'<)))
;;; > (bst-print t nums)
(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let* ((elt (node-elt bst))
             (sel-left
               (or (funcall < obj elt)
                   (and (eql obj elt)
                        (eql (random 2) 0)))))
        (if sel-left
            (make-node
              :elt elt
              :l   (bst-insert obj (node-l bst) <)
              :r   (node-r bst))
            (make-node
              :elt elt
              :l   (node-l bst)
              :r   (bst-insert obj (node-r bst) <))))))

;;; Copy bst
(defun bst-copy (bst)
  (if (null bst)
      nil
      (make-node
        :elt (node-elt bst)
        :l (bst-copy (node-l bst))
        :r (bst-copy (node-r bst)))))

;;; find object in BST
(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

;;; find the min value in BST
(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

;;; find the max value in BST
(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))


;;; print BST
(defun bst-print (s bst)
  (bst-print-prefix s bst nil))

;;; print BST with prefix
;;; the prefix can be parsed by prefix-string and print in front of the tree.
(defun bst-print-prefix (s bst prefix)
  (if (null bst)
      nil
      (progn
        ;; 1. print prefix
        ;; only if the prefix queue not empty, we print the prefix
        (and prefix
             (format s "~A"
               ;; every prefix is concat with intent prefix and node prefix
               (concatenate 'string
                 ;; intent prefix
                 (prefix-string (cdr prefix))
                 ;; node prefix
                 (format nil "~A---"
                   (let ((c (car prefix)))
                     (if (eql c #\ )
                         #\`
                         c))))))
        ;; 2. print node element
        (format s "~A~%" (node-elt bst))
        ;; 3. print sub tree.
        (let ((left (node-l bst))
              (right (node-r bst)))
          ;; if contains sibling node, append | prefix, or append space.
          (bst-print-prefix s left (cons (if right #\| #\ ) prefix))
          ;; right node always contains no sibling node.
          (bst-print-prefix s right (cons #\  prefix))))))

;;; print the intent prefix string by prefix queue.
(defun prefix-string (prefix)
  (if (null prefix)
      nil
      (concatenate 'string
        (prefix-string (cdr prefix))
        (format nil "~4,A" (car prefix)))))
