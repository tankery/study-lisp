;;;; Parsing date string like "16 Aug 1980" and return a list of integers
;;;; representing the day, month, and year.

;;; Split string to several parts.
;;; > (split "hello world" #'constituent 0)
;;; ("hello" "world")
(defun split (str test start)
  (let ((p-start (position-if test str :start start)))
    (if p-start
        (let ((p-end (position-if #'(lambda (c)
                                      (not (funcall test c)))
                                  str :start (+ p-start 1))))
          ;; p-end may equal to nil, this will let subseq pick the rest vector
          ;; from p-start
          (cons (subseq str p-start p-end)
                (if p-end
                    (split str test (+ p-end 1)))))
        nil)))

;;; helper function to test if the character can be displayed.
(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

;;; > (parse-date "16 Aug 1980")
;;; (16 8 1980)
(defun parse-date (str)
  (let ((tokens (split str #'constituent 0)))
    (list (parse-integer (first tokens))
          (parse-month (second tokens))
          (parse-integer (third tokens)))))

(defconstant month-names
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun parse-month (token)
  (let ((p (position token month-names :test #'string-equal)))
    (if p
        (+ p 1)
        nil)))
