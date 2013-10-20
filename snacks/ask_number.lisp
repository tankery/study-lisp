(defun ask-number (rest-time)
  (if (<= rest-time 0)
    (format t "No chance to enter, exit.")
    (progn
      (format t "Please enter a number (~A time~A left): "
        rest-time
        ; function for return string "s" if the number is larger than one
        ; else, will return empty string.
        ((lambda (n)
            (if (> n 1) "s" ""))
          rest-time))
      (let ((val (read)))
        (if (numberp val)
          (format t "Got ~A" val)
          (ask-number (- rest-time 1)))))))

(ask-number 3)
(quit)
