
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (null (nth (second pos) (nth (first pos) (track-env track))))			;;for lists
  ;(not (null (aref (track-env track) (first pos) (second pos))))		;;for arrays
)

(defun isGoalp (st) 
  "check if st is a goal state"
  (dolist (n (track-endpositions (state-track st)) NIL)
  	(if (equalp n (state-pos st)) 
  		(return T)
  	)
  )
)

(defun nextState (st act)
  "generate the nextState after state st and action act"
  ;;;test action input
  (if (and (listp act) (and (and (> (first act) -2) (< (first act) 2)) (and (> (second act) -2) (< (second act) 2))))
  	(if (state-p st)
	    (let* ((nextPos (list (+ (first (state-pos st)) (first (state-vel st)) (first act)) (+ (second (state-pos st)) (second (state-vel st)) (second act))))
	           (stAux (copy-state st)))
	    	(setf (state-pos stAux) nextPos)
	    	(if (isObstaclep nextPos (state-track st))
		    	(return-from nextState 
		      		(make-STATE
		            	:POS (state-pos st)
		    	      	:VEL (list 0 0)
		    	      	:ACTION (state-action st)
		    	      	:COST 20
		    	      	:TRACK (state-track st)
		    	      	:OTHER (state-other st)
		        	)
		        )
		        (return-from nextState 
		      		(make-STATE 
		            	:POS nextPos
		    	      	:VEL (list (+ (first (state-vel st)) (first act)) (+ (second (state-vel st)) (second act)))
		    	      	:ACTION act
		    	      	:COST (cond ((isGoalp stAux) -100)
		                      		(t 1))
		        		:TRACK (state-track st)
		        		:OTHER (state-other st)
		        	)
		        )
	      	)
	    )
    NIL)
  NIL)  
)


