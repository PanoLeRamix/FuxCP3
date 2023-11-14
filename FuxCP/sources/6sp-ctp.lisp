(in-package :fuxcp)

; Author: Anton Lamotte
; Date: October 2023
; This file contains the function that adds all the necessary constraints to the first species for three voices.

;;================================#
;; First species for three voices #
;;================================#
;; Note: fux-cp-6th executes the first species algorithm with some modified constraints.
(defun fux-cp-6th (species-list counterpoints &optional (species 6))
    (print "########## SIXTH SPECIES ##########")
    (setf counterpoint-1 (first counterpoints))
    (setf counterpoint-2 (second counterpoints))
    (print (list "species list = " species-list))

    ; Creating the first counterpoint
    (fux-cp-1st counterpoint-1 6)
   
    ; Creating the second counterpoint
    (setf *is-first-run 0) ; this is set to 0 so that the algorithm doesn't reset the costs
    (fux-cp-1st counterpoint-2 6)
    
    (setf solution-array (append (first (cp counterpoint-1)) (first (cp counterpoint-2)))) ; the final array with both counterpoints

    ; Constraints on the two counterpoints
    (print "no unisson between cp1 and cp2")
    (add-no-unisson-cst (first (cp counterpoint-1)) (first (cp counterpoint-2)) species)

    (print "all voices can't go in the same direction")
    (add-no-together-move-cst (first (motions counterpoint-1)) (first (motions counterpoint-2)))

    (print "no successive perfect consonances (cp1 to cp2)")
    (setf h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint-1)) (first (cp counterpoint-2)) h-intervals-1-2)
    (setf are-cp1-cp2-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr h-intervals-1-2 are-cp1-cp2-cons-arr)
    (add-no-successive-p-cons-cst are-cp1-cp2-cons-arr)

    ; Cost #15
    (print "prefer perfect chords") ; todo check dependency with 1st and 2nd cost
    (setq *p-chords-cost (gil::add-int-var-array-dom *sp* *cf-len (list 0 1)))
    (compute-prefer-p-chords-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) *p-chords-cost)
    (add-cost-to-factors *p-chords-cost)

    (print "Last chord cannot be minor")
    (add-no-minor-third-in-last-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2)))) 
    (print "Last chord cannot include a tenth")
    (add-no-tenth-in-last-chord-cst (last (h-intervals-brut counterpoint-1)) (last (h-intervals-brut counterpoint-2)))
    (print "Last chord must be a ... chord") 
    (add-chord-cst (last (first (h-intervals counterpoint-1))) (last (first (h-intervals counterpoint-2))))

    ; RETURN
    (if (eq species 6)
        ; then create the search engine
        (append (fux-search-engine solution-array 6) (list species))
        ; else
        nil
    )
    
)