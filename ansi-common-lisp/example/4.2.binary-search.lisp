;;;; Search for an object in a sorted vector.

;;; > (bin-search 3 #(0 2 3 5 8 9))
;;; 2
(defun bin-search (obj vec)
  (let ((len (length vec)))
    (if (zerop len)
        -1
        (finder obj vec 0 (- len 1)))))

;;; find obj between start end end position in vector
(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
        ;; contains only one element, check if we found it.
        (if (eql obj (aref vec start))
            start
            -1)
        ;; contains many element, deside search witch direction.
        (let* ((mid (+ start (truncate (/ range 2))))
               (mid-obj (aref vec mid)))
          (if (> obj mid-obj)
              (finder obj vec (+ mid 1) end)
              (finder obj vec start mid))))))
