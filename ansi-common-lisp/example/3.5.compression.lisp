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
    (list n elm)
    elm))


(defun uncompress (x)
  (if (null x)
    nil
    (let ((elm (car x))
          (rest (uncompress (cdr x))))
      (if (consp elm)
        (append
          (n-list (car elm) (cadr elm))
          rest)
        (cons elm rest)))))

(defun n-list (n elm)
  (if (zerop n)
    nil
    (cons elm (n-list (- n 1) elm))))

