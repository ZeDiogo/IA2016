;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
	"generate all possible next states"
	;(list st)

	(let ((res '()))
	  	(dolist (next (possible-actions) res)
	  		(push (nextState st next) res)
		)
	)
)

;;; limdepthfirstsearch 
;;;(defun limdepthfirstsearch (problem lim &key cutoff?)
(defun limdepthfirstsearch (problem lim &key cutoff?)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
;(list (make-node :state (problem-initial-state problem)))

	(let ((path '())
		  (isCutp nil))
	    (labels (
	    	(aux (st lim)
		    	(let ((next-States nil)
		    		  (result nil))

		    		  
		   			
		   				(if (funcall (problem-fn-isgoal problem) st)
		   					;return final path from init to objective 
		   					(return-from limdepthfirstsearch (reverse (push st path))))
		   				
		   				(if (zerop lim) 
		   					(return-from aux 'corte))
		   				
		   				(setf next-States (funcall (problem-fn-nextStates problem) st))
		   				(push st path)
		   				(loop for i in next-States do
		   					
		   					(setf result (aux i (1- lim)))
		   					(if (equal result 'corte) (setf isCutp t))
						)
						(pop path)
				)
	    	)
	    )
	    	(if (equal (aux (problem-initial-state problem) lim) 'corte) (setf isCutp t))
	    	;return 'corte or NIL
	    	(if isCutp (return-from limdepthfirstsearch 'corte) (return-from limdepthfirstsearch NIL)) 

		)
	)
)		      

;;;iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
		;(list (make-node :state (problem-initial-state problem)))


	(let ((path 'corte)
		  (limit 0))

		(loop while (equal path 'corte) do
			(setf path (limdepthfirstsearch problem limit))
			(incf limit)
		)
		(return-from iterlimdepthfirstsearch path)
	)
)

