;; a net is like this:
;; '((a b c) (b c) (c d))
;; means
;; a -> b -> c -> d
;;  \_______,^

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;; breadth-first search
(defun bfs (end path-queue net)
  (if (null path-queue)
      nil
      ; get the first node and the path we get there
      (let* ((path (car path-queue))
             (node (car path)))
        ; print the search process.
        (format t " - ~A~%" path-queue)
        (if (eql node end)
            ; if found the destination, return the path.
            ; because we append path after the deeper node, we can easyly get the node from queue,
            ; but this will make the path reversed.
            (reverse path)
            ; not found, search remaining queue.
            (bfs end
                 ; append new-paths to the end of queue, so the deeper node will be visit latter.
                 (append (cdr path-queue)
                         (new-paths path node net))
                 net)))))

;; a new-path is a deeper level path.
;; this function will find all node we can reach and cons with the path we get to there.
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))
