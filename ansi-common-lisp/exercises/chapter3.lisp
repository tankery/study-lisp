;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.
; > (order-union '(a b c) '(b a d))
; (A B C D)
(defun our-union (origin lst)
  (if (null lst)
      (if (null origin)
          nil
          (adjoin (car origin) (our-union (cdr origin) lst)))
      (adjoin (car lst) (our-union origin (cdr lst)))))

(defun order-union (origin lst)
  (if (null lst)
      (if (null origin)
          nil
          (adjoin (car origin) (order-union (cdr origin) lst)))
      (append-join (order-union origin (cdr lst)) (car lst))))

(defun append-join (lst elm)
  (if (member elm lst)
      lst
      (append lst (cons elm nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3.
; > (occurrences '(a b a d a c d c a))
; ((A . 4) (C . 2) (D . 2) (B . 1))
(defun occurrences (lst)
  (if (null lst)
      nil
      (occurrences-acc (cons (cons (car lst) 1) nil) (cdr lst))))

(defun occurrences-acc (result lst)
  ; (format t "occurrences-acc: result ~A, lst ~A~%" result lst)
  (if (null result)
      nil
      (if (null lst)
          (sort result #'(lambda (unit1 unit2)
                          (> (cdr unit1) (cdr unit2))))
          (let* ((next (car lst))
                 (found (member next result :key #'car)))
            (if found
                (let* ((unit (car found))
                       (new-unit (cons (car unit)
                                       (+ (cdr unit) 1))))
                  (setf result (cons new-unit (remove unit result))))
                (let ((new-unit (cons next 1)))
                  (setf result (cons new-unit result))))
            (occurrences-acc result (cdr lst))))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 5.
; > (pos+ '(7 5 1 4))
; (7 6 3 7)
(defun pos+ (lst)
  (if (null lst)
    nil
    (cons (car lst)
          (mapcar #'(lambda (x) (+ x 1))
                  (pos+ (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 7. fewer cons cell compression.
; > (compress '(a a a b b c d e e))
(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x))

(defun compr (elm n lst)
  (if (null lst)
    (list (n-elm n elm))
    (let ((next (car lst)))
      (if (eql elm next)
        (compr elm (+ n 1) (cdr lst))
        (cons (n-elm n elm) (compr next 1 (cdr lst)))))))

(defun n-elm (n elm)
  (if (> n 1)
    ; this is the only difference......
    (cons elm n)
    elm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 8.
; > (showdots '(a b c))
; (A . (B . (C . NIL)))
; NIL
(defun showdots (lst)
  (if (null lst)
      (format t "NIL")
      (progn
        (format t "(~A . " (car lst))
        (showdots (cdr lst))
        (format t ")"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 9. find the longest finite path through a network
; > (longest-path 'a 'd '((a b c) (b c) (c d)))
; (A B C D)
(defun longest-path (start end net)
  (bfs end (list (list start)) nil net))

;; breadth-first search
(defun bfs (end path-queue avail-path net)
  (if (null path-queue)
      ; all path has reached, return the last available path.
      ; because we append path after the deeper node, we can easyly get the node from queue,
      ; but this will make the path reversed.
      (reverse avail-path)
      ; get the first node and the path we get there
      (let* ((path (car path-queue))
             (node (car path)))
        ; print the search process.
        (format t " - ~A~%" path-queue)
        (if (eql node end)
            ; if found the destination, always use the path as result, the latest path will be the
            ; longest path.
            (bfs end (cdr path-queue) path net)
            ; not found, search remaining queue.
            (bfs end
                 ; append new-paths to the end of queue, so the deeper node will be visit latter.
                 (if (member node (cdr path))
                     ; if node already in path, a loop occue, throw away this path.
                     (cdr path-queue)
                     ; node not in path, continue find next level node.
                     (append (cdr path-queue)
                             (new-paths path node net)))
                 avail-path
                 net)))))

;; a new-path is a deeper level path.
;; this function will find all node we can reach and cons with the path we get to there.
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))
