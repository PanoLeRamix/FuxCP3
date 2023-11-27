(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the first species.

;;==========================#
;; FIRST SPECIES            #
;;==========================#
(defun fux-cp-1st (counterpoint &optional (species 1))
    (print "########## FIRST SPECIES ##########")
    "Create the CSP for the first species of Fux's counterpoint."

    ;============================================ CREATING GIL ARRAYS =============================
    ;; initialize the variables
    (print "Initializing variables...")
    
    ; add the counterpoint array to the space with the domain *cp-domain
    ;(setf (first (cp counterpoint)) (gil::add-int-var-array-dom *sp* *cf-len (extended-cp-domain counterpoint)))
    
    ; @completely new or reworked
    ; ======= 2 counterpoints specific
    (if (eq *N-VOICES 2) (let ( ; if re-mi-la-si is the last cf note then you can use a major third even if it's not in the harmony
        (tonal (mod (car (last *cf)) 12))
        )
        (case tonal ((2 4 9 10) 
            ; using the chromatic domain as it is going to be constrained to the harmonic triad by a later constraint
            (setf (nth *cf-last-index (first (cp counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint))) 
        )))
    )
    ; =======

    (if (is-borrow-allowed) (case species ((1 6)
        ; then add to the penultimate note more possibilities
        (setf (nth *cf-penult-index (first (cp counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint))) 
    )))

    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")

    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
    (setf (first (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (cp counterpoint)) *cf (first (h-intervals counterpoint)))

    ; @completely new or reworked
    ; ======= 2 counterpoints specific -> this is used further on for 3 voices costs
    (if (eq *N-VOICES 2) (progn
        (if (eq species 9) (progn 
                (setf (h-intervals-abs counterpoint) (gil::add-int-var-array *sp* *cf-last-index -127 127))
                (setf (h-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-last-index -127 127))
                (create-intervals (rest *cf) (third (cp counterpoint)) (h-intervals-abs counterpoint) (h-intervals-brut counterpoint))
            ) (progn
                (setf (h-intervals-abs counterpoint) (gil::add-int-var-array *sp* *cf-len -127 127))
                (setf (h-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-len -127 127))
                (create-intervals *cf (first (cp counterpoint)) (h-intervals-abs counterpoint) (h-intervals-brut counterpoint))
            ) 
        )
    ))
    ; =======

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the absolute intervals between two notes in a row of the counterpoint
    (setf (first (m-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (first (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12))
    (create-m-intervals-self (first (cp counterpoint)) (first (m-intervals counterpoint)) (first (m-intervals-brut counterpoint)))

    
    (case species ((1 6) ; only for the first species
        ; then
        (progn
            ; creating melodic intervals array between the note n and n+2
            (setf (m2-intervals counterpoint) (gil::add-int-var-array *sp* *cf-penult-index 0 12))
            (setf (m2-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-penult-index -12 12))
            (create-m2-intervals (first (cp counterpoint)) (m2-intervals counterpoint) (m2-intervals-brut counterpoint))
            
            ; creating boolean is counterpoint off key array
            (print "Creating is counterpoint off key array...")
            (setf (is-cp-off-key-arr counterpoint) (gil::add-bool-var-array *sp* *cf-len 0 1))
            (create-is-member-arr (first (cp counterpoint)) (is-cp-off-key-arr counterpoint) (off-domain counterpoint))
        )
    ))

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setf (is-p-cons-arr counterpoint) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first (h-intervals counterpoint)) (is-p-cons-arr counterpoint))
    

    ; creating order/role of pitch array (if cantus firmus is higher or lower than counterpoint)
    ; 0 for being the bass, 1 for being above
    (print "Creating order of pitch array...")
    (setf (first (is-cf-bass-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-cf-bass-arr (first (cp counterpoint)) *cf (first (is-cf-bass-arr counterpoint)))


    ; creating motion array
    (print "Creating motion array...")
    (setf (first (motions counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 2)) ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (setf (first (motions-cost counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (first (m-intervals-brut counterpoint)) *cf-brut-m-intervals (first (motions counterpoint)) (first (motions-cost counterpoint)))

    
    ;============================================ HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all intervals between the cantus firmus and the counterpoint, the interval must be a consonance
    (print "Harmonic consonances...")
    (case species
        ((1 ) (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals counterpoint))))
        ((2 7) (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals counterpoint)) PENULT_THESIS_VAR))
        ((3 8) (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals counterpoint)) PENULT_1Q_VAR))
        ;(otherwise (error "Species not supported"))
    )

    ; no unisson between the cantus firmus and the counterpoint unless it is the first note or the last note
    (print "No unisson...")
    (add-no-unisson-cst (first (cp counterpoint)) *cf)

    (case species ((1 2) 
        ; then
        (progn
        ; must start with a perfect consonance
        (print "Perfect consonance at the beginning...")
        (add-p-cons-start-cst (first (h-intervals counterpoint)))

        ; must end with a perfect consonance
        (print "Perfect consonance at the end...")
        (add-p-cons-end-cst (first (h-intervals counterpoint)))
        )
    ))

    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (case species
        ((1 #|6|#) (add-penult-cons-cst (penult (first (is-cf-bass-arr counterpoint))) (penult (first (h-intervals counterpoint)))))
    )

    ;============================================ MELODIC CONSTRAINTS =============================
    ; NOTE: with the degree iii in penultimate *cf measure -> no solution bc there is a *tritone between I#(minor third) and V.
    (print "Melodic constraints...")
    (case species ((1 6)
        ; then
        (progn
            ; no more than minor sixth melodic interval
            (print "No more than minor sixth...")
            (add-no-m-jump-cst (first (m-intervals counterpoint)))

            ; no chromatic motion between three consecutive notes
            (print "No chromatic motion...")
            (add-no-chromatic-m-cst (first (m-intervals-brut counterpoint)) (m2-intervals-brut counterpoint))

            ;==================================== MOTION CONSTRAINTS ============================
            (print "Motion constraints...")

            (if (eq species 1) ; for the 6th species, it isn't a constraint but a cost
            ; no direct motion to reach a perfect consonance
                (progn
                    (print "No direct motion to reach a perfect consonance...")
                    (add-no-direct-move-to-p-cons-cst (first (motions counterpoint)) (is-p-cons-arr counterpoint))
                )
            )
            ; no battuta kind of motion
            ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
            (print "No battuta kind of motion...")
            (add-no-battuta-cst (first (motions counterpoint)) (first (h-intervals counterpoint)) (first (m-intervals-brut counterpoint)) (first (is-cf-bass-arr counterpoint)))
        )
    ))

    ; @completely new or reworked
    ; ========= 2 counterpoints specific
    (if (eq *N-VOICES 2)
        (progn
            (print "No successive perfect consonances (counterpoint to cantus firmus)")
            (add-no-successive-p-cons-cst (is-p-cons-arr counterpoint))

            (print "Ascending sixths sound harsh")
            (add-no-ascending-sixths-cst (first (h-intervals counterpoint)) (first (cp counterpoint)))
        )
    )
    ; =========
    
    ;============================================ COST FACTORS ====================================
    (print "Cost function...")

    (case species ((1 6)
        ; then
        (progn
            (setf (m-all-intervals counterpoint) (first (m-intervals counterpoint)))
            ; 1, 2) imperfect consonances are preferred to perfect consonances
            (print "Imperfect consonances are preferred to perfect consonances...")
            (add-p-cons-cost-cst (h-intervals counterpoint))

            ; 3, 4) add off-key cost, m-degrees cost and tritons cost
            (print "add off-key cost, m-degrees cost and tritons cost")
            (set-general-costs-cst counterpoint *cf-len)

            ; 5) motion costs
            (print "add motion costs")
            (add-cost-to-factors (first (motions-cost counterpoint)) 'motions-cost)    
        )
    ))

    (setf (solution-array counterpoint) (first (cp counterpoint)))
    (setf (solution-len counterpoint) *cf-len)

    ; RETURN
    (case species 
        (1 (append (fux-search-engine (solution-array counterpoint)) (list (list 1))))
    )
)