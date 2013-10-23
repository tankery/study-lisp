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
          
; 5.
; > (pos+ '(7 5 1 4))
; (7 6 3 7)
(defun pos+ (lst)
  (if (null lst)
    nil
    (cons (car lst)
          (mapcar #'(lambda (x) (+ x 1))
                  (pos+ (cdr lst))))))

; 7. fewer cons cell compression.

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
