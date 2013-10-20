; 7.
(defun sublistp (x)
  (if (null x)
      nil
      (or (listp (car x))
          (sublistp (cdr x)))))

; 8. (a).
(defun print-dots-iter (num)
  (do ((i 0 (+ i 1)))
      ((>= i num) (format t "~%"))
    (format t ".")))
(defun print-dots-recur (num)
  (if (eql num 0)
    (format t "~%")
    (progn
      (format t ".")
      (print-dots-rec (- num 1)))))

; 8. (b).
(defun count-sym-iter (a lst)
  (let ((l lst)
        (cnt 0))
    (do ((i (car l) (car l)))
        ((null i) cnt)
      (if (eql a i)
        (setf cnt (+ cnt 1)))
      (setf l (cdr l)))))
(defun count-sym-recur (a lst)
  (if (null lst)
    0
    (if (eql a (car lst))
      (+ 1 (count-sym-recur a (cdr lst)))
      (count-sym-recur a (cdr lst)))))

; 9.
(defun summit-fun (lst)
  (apply #'+ (remove nil lst)))
(defun summit-recur (lst)
  (if (null lst)
    0
    (let ((x (car lst)))
      (if (null x)
        (summit-recur (cdr lst))
        (+ x (summit-recur (cdr lst)))))))
