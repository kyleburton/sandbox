(defpackage :com.github.kyleburton.land-of-lisp.congestion-city
  (:use :common-lisp
	:com.github.kyleburton.mylib))
(in-package :com.github.kyleburton.land-of-lisp.congestion-city)

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)
(defparameter *player-pos* nil)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b)
	  (cons b a))))

(defun make-edge-list ()
  (apply #'append
	 (loop repeat *edge-num*
	    collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not
   (lambda (x)
     (eql (car x)
	  node))
   edge-list))


(defun get-connected (node edge-list) ;; pg 138
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands))
	    (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))


(defun edges-to-alist (edge-list)
  (mapcar
   (lambda (node1)
     (cons node1
	   (mapcar
	    (lambda (edge)
	      (list (cdr edge)))
	    (remove-duplicates
	     (direct-edges node1 edge-list)
	     :test #'equal))))
   (remove-duplicates (mapcar #'car edge-list))))

;; (edges-to-alist *congestion-city-edges*)
;; (edges-to-alist (make-city-edges))
;; (edges-to-alist *edges*)

(defun add-cops (edge-alist edges-with-cops)
  (mapcar
   (lambda (x)
     (let ((node1 (car x))
	   (node1-edges (cdr x)))
       (cons node1
	     (mapcar
	      (lambda (edge)
		(let ((node2 (car edge)))
		  (if (intersection (edge-pair node1 node2)
				    edges-with-cops
				    :test #'equal)
		      (list node2 'cops)
		      edge)))
	      node1-edges))))
   edge-alist))

(defun make-city-edges () ;; pg 139
  (let* ((nodes     (loop for i from 1 to *node-num*
		       collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops      (remove-if-not
		     (lambda (x)
		       (declare (ignore x))
		       (zerop (random *cop-odds*)))
		     edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))


(comment

  (let* ((*node-num* 10)
	 (*edge-num* (* 2 *node-num*)))
    (sort (make-city-edges) #'< :key #'car))

  )


(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some
       (lambda (x)
	 (within-one x b edge-alist))
       (neighbors a edge-alist))))


(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
		       collect (random-node))))
    (loop for n from 1 to *node-num*
       collect (append (list n)
		       (cond ((eql n wumpus) '(wumpus))
			     ((within-two n wumpus edge-alist) '(blood)))
		       (cond ((member n glow-worms) '(glow-worm))
			     ((some (lambda (worm) (within-one n worm edge-alist))
				    glow-worms)
			      '(lights!)))
		       (when (some #'cdr (cdr (assoc n edge-alist)))
			 '(sirens!))))))

(comment
  (let* ((*node-num* 10)
	 (*edge-num* (* 2 *node-num*))
	 (city-edges (make-city-edges))
	 (city-nodes (make-city-nodes city-edges)))
    `(:nodes ,city-nodes :edges ,city-edges))

  (sort *congestion-city-nodes* #'< :key #'car)
  )

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node)
	x)))

(defun draw-city ()
  (ugraph->png "city.dot" *congestion-city-nodes* *congestion-city-edges*))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))

(comment
  (progn
    (new-game)
    ;; (ext:shell (concatenate 'string "eog city.png"))
    (ext:run-shell-command (concatenate 'string "eog city.png") :wait nil)
    )

  *player-pos*

)


(defun known-city-nodes () ;; pg 146
  (mapcar
   (lambda (node)
     (if (member node *visited-nodes*)
	 (let ((n (assoc node *congestion-city-nodes*)))
	   (if (eql node *player-pos*)
	       (append n '(*))
	       n))
	 (list node '?)))
   (remove-duplicates
    (append *visited-nodes*
	    (mapcan (lambda (node)
		      (mapcar #'car
			      (cdr (assoc node *congestion-city-edges*))))
		    *visited-nodes*)))))

'(comment

  (new-game)
  (known-city-nodes)


  ;; ah, is mapcan equivalent to clojure's mapcat?
  (mapcan
   (lambda (elt)
     elt)
   '((1 2 3) nil (4 5) (6 7 8 9) (10)))
  ;; => (1 2 3 4 5 6 7 8 9 10)
  ;; yep, it is
)
