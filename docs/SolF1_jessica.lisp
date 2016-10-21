
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
(load "datastructures.lisp")
(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
;(load "datastructures.fas")
;(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (cond ((equal (aref (car pos) (cdr pos) (track-env track)) NIL))))

(defun isGoalp (st) 
  "check if st is a goal state"
  (dolist (endpos (track-endpositions (state-track st)) NIL)
	(if (equal endpos (state-pos st))
		(return T)))
  

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (let ((nextVel (list (+ (car (state-vel st)) (car act)) (+ (cdr (state-vel st)) (cdr act))))
        (nextPos (list (+ (car nextVel) (car (state-pos st))) (+ (cdr nextVel) (cdr (state-pos st)))))
        (nextCost (state-cost st))
        (newSt (make-STATE :POS nextPos 
                           :VEL nextVel 
                           :ACTION act 
                           :COST nextCost
			   :TRACK (state-track st)
                           :OTHER (state-track st))))
    (cond ((isObstaclep (nextPos (state-track st)))
           (setf (state-pos newSt) (state-pos st)
             (state-vel newSt) '(0 0)
             (state-cost newSt) (+ nextCost 20))) 
          ((isGoalp (newSt)) 
           (setf (state-cost newSt) (- nextCost 100)))
          (T (setf (state-cost newSt) (1+ nextCost)) return-from nextState newSt))))        
 
           



