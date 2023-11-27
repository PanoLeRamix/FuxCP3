(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the functions that:
;   - dispatch to the right species functions
;   - set the global variables of the CSP
;   - manage the search for solutions

(print "Loading fux-cp...")

; get the value at key @k in the hash table @h as a list
(defun geth-dom (h k)
    (list (gethash k h))
)

; get the value at key @k in the parameters table as a list
(defun getparam-val (k)
    (geth-dom *params* k)
)

; get the value at key @k in the parameters table as a domain
(defun getparam-dom (k)
    (list 0 (getparam k))
)

; get the value at key @k in the parameters table
(defun getparam (k)
    (gethash k *params*)
)

; get if borrow-mode param is allowed
(defun is-borrow-allowed ()
    (not (equal (getparam 'borrow-mode) "None"))
)


; define all the constants that are going to be used
(defun define-global-constants ()
    ;; CONSTANTS
    ; Number of costs added
    (defparameter *n-cost-added 0)
    ; Motion types
    (defparameter DIRECT 2)
    (defparameter OBLIQUE 1)
    (defparameter CONTRARY 0)

    ; Integer constants (to represent costs or intervals)
    ; 0 in IntVar
    (defparameter ZERO (gil::add-int-var-dom *sp* (list 0)))
    ; 1 in IntVar
    (defparameter ONE (gil::add-int-var-dom *sp* (list 1)))
    ; 3 in IntVar (minor third)
    (defparameter THREE (gil::add-int-var-dom *sp* (list 3)))
    ; 9 in IntVar (major sixth)
    (defparameter NINE (gil::add-int-var-dom *sp* (list 9)))

    ; Boolean constants
    ; 0 in BoolVar
    (defparameter FALSE (gil::add-bool-var *sp* 0 0))
    ; 1 in BoolVar
    (defparameter TRUE (gil::add-bool-var *sp* 1 1))

    ; Intervals constants
    ; perfect consonances intervals
    (defparameter P_CONS (list 0 7))
    ; imperfect consonances intervals
    (defparameter IMP_CONS (list 3 4 8 9))
    ; all consonances intervals
    (defparameter ALL_CONS (union P_CONS IMP_CONS))
    ; dissonances intervals
    (defparameter DIS (list 1 2 5 6 10 11))
    ; penultimate intervals, i.e. minor third and major sixth
    (defparameter PENULT_CONS (list 3 9))
    ; penultimate thesis intervals, i.e. perfect fifth and sixth
    (defparameter PENULT_THESIS (list 7 8 9))
    ; penultimate 1st quarter note intervals, i.e. minor third, major sixth and octave/unisson
    (defparameter PENULT_1Q (list 0 3 8))
    ; penultimate syncope intervals, i.e. seconds and sevenths
    (defparameter PENULT_SYNCOPE (list 1 2 10 11))

    ; P_CONS in IntVar
    (defparameter P_CONS_VAR (gil::add-int-var-const-array *sp* P_CONS))
    ; IMP_CONS in IntVar
    (defparameter IMP_CONS_VAR (gil::add-int-var-const-array *sp* IMP_CONS))
    ; ALL_CONS in IntVar
    (defparameter ALL_CONS_VAR (gil::add-int-var-const-array *sp* ALL_CONS))
    ; PENULT_CONS in IntVar
    (defparameter PENULT_CONS_VAR (gil::add-int-var-const-array *sp* PENULT_CONS))
    ; PENULT_THESIS in IntVar
    (defparameter PENULT_THESIS_VAR (gil::add-int-var-const-array *sp* PENULT_THESIS))
    ; PENULT_1Q in IntVar
    (defparameter PENULT_1Q_VAR (gil::add-int-var-const-array *sp* PENULT_1Q))
    ; PENULT_SYNCOPE in IntVar
    (defparameter PENULT_SYNCOPE_VAR (gil::add-int-var-const-array *sp* PENULT_SYNCOPE))

    ; *cf-brut-intervals is the list of brut melodic intervals in the cantus firmus
    (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))
    ; array representing the brut melodic intervals of the cantus firmus
    (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)

    ;; COSTS
    ;; Melodic costs
    (defparameter *m-step-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-step-cost)))
    (defparameter *m-third-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-third-cost)))
    (defparameter *m-fourth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-fourth-cost)))
    (defparameter *m-tritone-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-tritone-cost)))
    (defparameter *m-fifth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-fifth-cost)))
    (defparameter *m-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-sixth-cost)))
    (defparameter *m-seventh-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-seventh-cost)))
    (defparameter *m-octave-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-octave-cost)))
    ;; General costs
    (defparameter *borrow-cost* (gil::add-int-var-dom *sp* (getparam-val 'borrow-cost)))
    (defparameter *h-fifth-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-fifth-cost)))
    (defparameter *h-octave-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-octave-cost)))
    (defparameter *con-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'con-motion-cost)))
    (defparameter *obl-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'obl-motion-cost)))
    (defparameter *dir-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'dir-motion-cost)))
    ;; Species specific costs
    (defparameter *penult-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'penult-sixth-cost)))
    (defparameter *non-cambiata-cost* (gil::add-int-var-dom *sp* (getparam-val 'non-cambiata-cost)))
    (defparameter *two-beats-apart-cost* (gil::add-int-var-dom *sp* (getparam-val 'two-beats-apart-cost)))
    (defparameter *two-bars-apart-cost* (gil::add-int-var-dom *sp* (getparam-val 'two-bars-apart-cost)))
    (defparameter *no-syncopation-cost* (gil::add-int-var-dom *sp* (getparam-val 'no-syncopation-cost)))

    ;; Params domains
    (defparameter *motions-domain*
        (remove-duplicates (mapcar (lambda (x) (getparam x))
            (list 'con-motion-cost 'obl-motion-cost 'dir-motion-cost)
        ))
    )
)

; @completely new or reworked
(defclass counterpoint-class () (
    ; species
    (species :accessor species :initarg :species :initform nil)

    ; solution-array
    (solution-array :accessor solution-array :initarg :solution-array :initform nil)
    (solution-len :accessor solution-len :initarg :solution-len :initform nil)

    ; voice variables
    (cp-range :accessor cp-range :initarg :cp-range :initform nil)
    (cp-domain :accessor cp-domain :initarg :cp-domain :initform nil)
    (chromatic-cp-domain :accessor chromatic-cp-domain :initarg :chromatic-cp-domain :initform nil)
    (extended-cp-domain :accessor extended-cp-domain :initarg :extended-cp-domain :initform nil)
    (off-domain :accessor off-domain :initarg :off-domain :initform nil)
    (voice-type :accessor voice-type :initarg :voice-type :initform nil)

    ; 1st species variables
    (cp :accessor cp :initarg :cp :initform (list nil nil nil nil)) ; represents the notes of the counterpoint
    (h-intervals :accessor h-intervals :initarg :h-intervals :initform (list nil nil nil nil))
    (m-intervals-brut :accessor m-intervals-brut :initarg :m-intervals-brut :initform (list nil nil nil nil))
    (m-intervals :accessor m-intervals :initarg :m-intervals :initform (list nil nil nil nil))
    (motions :accessor motions :initarg :motions :initform (list nil nil nil nil))
    (motions-cost :accessor motions-cost :initarg :motions-cost :initform (list nil nil nil nil))
    (is-cf-bass-arr :accessor is-cf-bass-arr :initarg :is-cf-bass-arr :initform (list nil nil nil nil))
    (m2-intervals-brut :accessor m2-intervals-brut :initarg :m2-intervals-brut :initform nil)
    (m2-intervals :accessor m2-intervals :initarg :m2-intervals :initform nil)
    (cf-brut-m-intervals :accessor cf-brut-m-intervals :initarg :cf-brut-m-intervals :initform nil)
    (is-p-cons-arr :accessor is-p-cons-arr :initarg :is-p-cons-arr :initform nil)
    (is-cp-off-key-arr :accessor is-cp-off-key-arr :initarg :is-cp-off-key-arr :initform nil)
    (p-cons-cost :accessor p-cons-cost :initarg :p-cons-cost :initform nil)
    (fifth-cost :accessor fifth-cost :initarg :fifth-cost :initform nil)
    (octave-cost :accessor octave-cost :initarg :octave-cost :initform nil)
    (m-degrees-cost :accessor m-degrees-cost :initarg :m-degrees-cost :initform nil)
    (m-degrees-type :accessor m-degrees-type :initarg :m-degrees-type :initform nil)
    (off-key-cost :accessor off-key-cost :initarg :off-key-cost :initform nil)
    (m-all-intervals :accessor m-all-intervals :initarg :m-all-intervals :initform nil)

    ; 2nd species variables
    (h-intervals-abs :accessor h-intervals-abs :initarg :h-intervals-abs :initform (list nil nil nil nil))
    (h-intervals-brut :accessor h-intervals-brut :initarg :h-intervals-brut :initform (list nil nil nil nil))
    (m-succ-intervals :accessor m-succ-intervals :initarg :m-succ-intervals :initform (list nil nil nil nil)) 
    (m-succ-intervals-brut :accessor m-succ-intervals-brut :initarg :m-succ-intervals-brut :initform (list nil nil nil nil)) 
    (m2-len :accessor m2-len :initarg :m2-len :initform nil)
    (total-m-len :accessor total-m-len :initarg :total-m-len :initform nil)
    (m-all-intervals-brut :accessor m-all-intervals-brut :initarg :m-all-intervals-brut :initform nil)
    (real-motions :accessor real-motions :initarg :real-motions :initform nil)
    (real-motions-cost :accessor real-motions-cost :initarg :real-motions-cost :initform nil)
    (is-ta-dim-arr :accessor is-ta-dim-arr :initarg :is-ta-dim-arr :initform nil)
    (is-nbour-arr :accessor is-nbour-arr :initarg :is-nbour-arr :initform nil)
    (penult-thesis-cost :accessor penult-thesis-cost :initarg :penult-thesis-cost :initform nil) 

    ; 3rd species variables
    (is-5qn-linked-arr :accessor is-5qn-linked-arr :initarg :is-5qn-linked-arr :initform nil)
    (is-not-cambiata-arr :accessor is-not-cambiata-arr :initarg :is-not-cambiata-arr :initform nil)
    (not-cambiata-cost :accessor not-cambiata-cost :initarg :not-cambiata-cost :initform nil)
    (m2-eq-zero-cost :accessor m2-eq-zero-cost :initarg :m2-eq-zero-cost :initform nil)
    (is-cons-arr :accessor is-cons-arr :initarg :is-cons-arr :initform (list nil nil nil nil))
    (cons-cost :accessor cons-cost :initarg :cons-cost :initform (list nil nil nil nil))

    ; 4th species variables
    (is-no-syncope-arr :accessor is-no-syncope-arr :initarg :is-no-syncope-arr :initform nil)
    (no-syncope-cost :accessor no-syncope-cost :initarg :no-syncope-cost :initform nil)

    ; 5th species variables
    (species-arr :accessor species-arr :initarg :species-arr :initform nil) ; 0: no constraint, 1: first species, 2: second species, 3: third species, 4: fourth species
    (sp-arr :accessor sp-arr :initarg :sp-arr :initform nil) ; represents *species-arr by position in the measure
    (is-nth-species-arr :accessor is-nth-species-arr :initarg :is-nth-species-arr :initform (list nil nil nil nil nil)) ; if *species-arr is n, then *is-nth-species-arr is true
    (is-3rd-species-arr :accessor is-3rd-species-arr :initarg :is-3rd-species-arr :initform (list nil nil nil nil)) ; if *species-arr is 3, then *is-3rd-species-arr is true
    (is-4th-species-arr :accessor is-4th-species-arr :initarg :is-4th-species-arr :initform (list nil nil nil nil)) ; if *species-arr is 4, then *is-4th-species-arr is true
    (is-2nd-or-3rd-species-arr :accessor is-2nd-or-3rd-species-arr :initarg :is-2nd-or-3rd-species-arr :initform nil) ; if *species-arr is 2 or 3, then *is-2nd-or-3rd-species-arr is true
    (m-ta-intervals :accessor m-ta-intervals :initarg :m-ta-intervals :initform nil) ; represents the m-intervals between the thesis note and the arsis note of the same measure
    (m-ta-intervals-brut :accessor m-ta-intervals-brut :initarg :m-ta-intervals-brut :initform nil) ; same but without the absolute reduction
    (is-mostly-3rd-arr :accessor is-mostly-3rd-arr :initarg :is-mostly-3rd-arr :initform nil) ; true if second, third and fourth notes are from the 3rd species
    (is-constrained-arr :accessor is-constrained-arr :initarg :is-constrained-arr :initform nil) ; represents !(*is-0th-species-arr) i.e. there are species constraints
    (is-cst-arr :accessor is-cst-arr :initarg :is-cst-arr :initform (list nil nil nil nil)) ; represents *is-constrained-arr for all beats of the measure

    ; 6st species variables
    (variety-cost :accessor variety-cost :initarg :variety-cost :initform nil)
    ;(is-voice-bass :accessor is-voice-bass :initarg :is-voice-bass :initform 0)
    (is-bass-arr :accessor is-bass-arr :initarg :is-bass-arr :initform (list nil nil nil nil))
))

; @completely new or reworked
(defun init-counterpoint (voice-type species)
    ; Lower bound and upper bound related to the cantus firmus pitch
    (let (
        (range-upper-bound (+ 12 (* 6 voice-type)))
        (range-lower-bound (+ -6 (* 6 voice-type)))
        )
        (let (
            ; set the pitch range of the counterpoint
            (cp-range (range (+ *tone-pitch-cf range-upper-bound) :min (+ *tone-pitch-cf range-lower-bound))) ; arbitrary range
            )
            (let (
            ; set counterpoint pitch domain
            (cp-domain (intersection cp-range *scale))
            ; penultimate (first *cp) note domain
            (chromatic-cp-domain (intersection cp-range *chromatic-scale))
            ; set counterpoint extended pitch domain
            (extended-cp-domain (intersection cp-range (union *scale *borrowed-scale)))
            ; set the domain of the only barrowed notes
            (off-domain (intersection cp-range *off-scale))
            )
            (make-instance 'counterpoint-class :cp-range cp-range
                                               :cp-domain cp-domain
                                               :chromatic-cp-domain chromatic-cp-domain
                                               :extended-cp-domain extended-cp-domain
                                               :off-domain off-domain
                                               :voice-type voice-type
                                               :species species
                                               :cp (list (gil::add-int-var-array-dom *sp* *cf-len extended-cp-domain) nil nil nil)
            ))
        )
    )
)

;; DISPATCHER FUNCTION
; @completely new or reworked
(defun fux-cp (species-list)
    "Dispatches the counterpoint generation to the appropriate function according to the species."
    ; THE CSP SPACE 
    (defparameter *sp* (gil::new-space))

    ; re/set global variables
    (define-global-constants)
    (setq *species-list species-list)
    (setq *cost-indexes (make-hash-table))                      
    (setq *cost-factors (set-cost-factors))

    (print (list "Choosing species: " species-list))
    (setq counterpoints (make-list *N-VOICES :initial-element nil))
    (dotimes (i *N-VOICES) (setf (nth i counterpoints) (init-counterpoint (nth i *voices-types) (nth i species-list))))
    #| TO BE CORRECTED 
    (if (and (< (first *voices-types) 0) (< (first *voices-types) (second *voices-types)))
        (setf *nth-voice-is-bass 0)
        (if (< (second *voices-types) 0)
            (setf *nth-voice-is-bass 1)
            (setf *nth-voice-is-bass -1) ; counterpoint is the bass
        )
    )
    (if (>= *nth-voice-is-bass 0) (setf (is-voice-bass (nth *nth-voice-is-bass counterpoints)) 1))
    |#
    ;(setq *cost-indexes (make-instance 'cost-indexes-class))
    (setq *bass-array (list nil nil nil nil))
    (create-is-voice-bass-arr *cf counterpoints)
    
    (case (length species-list)
        (1 (case (first species-list) ; if only two voices
            (1 (progn
                (fux-cp-1st (first counterpoints))
            ))
            (2 (progn
                (fux-cp-2nd (first counterpoints))
            ))
            (3 (progn
                (fux-cp-3rd (first counterpoints))
            ))
            (4 (progn
                (fux-cp-4th (first counterpoints))
            ))
            (5 (progn
                (fux-cp-5th (first counterpoints))
            ))
            (otherwise (error "Species ~A not implemented" species))
            )
        )
        (2 (progn
            (fux-cp-3v species-list counterpoints)
        ))
        (otherwise (error "The species list is longer than what is currently implemented. Length = ~A" species))
    )
)

; @completely new or reworked
(defun reorder-costs (species-list)
    (print "########## REORDERING ##########")
    (setf species-tag (reduce #'(lambda (x y) (+ (* x 10) y)) species-list))
    (let (
        (i 0)
        (reordered-costs (make-list *N-COST-FACTORS :initial-element nil))
        (costs-names-by-order (list ; from least to most important
                                'direct-move-to-p-cons-cost
                                'not-cambiata-cost
                                'm2-eq-zero-cost
                                'penult-thesis-cost
                                'no-syncope-cost
                                
                                'fifth-cost
                                'octave-cost               
                                'h-triad-3rd-species-cost
                                'h-triad-cost                 
                                'm-degrees-cost
                                'motions-cost
                                'variety-cost
                                'off-key-cost
        ))
        )
        (assert costs-names-by-order () "costs-names-by-order is nil, shouldn't be.")
        (dolist (cost costs-names-by-order)
            (let ((index (gethash cost *cost-indexes)))
                (if index (progn
                    (assert (gethash cost *cost-indexes) () "The hash is nil, but should not be. Current cost name = ~A" (nth i costs-names-by-order))
                    (loop for index in (gethash cost *cost-indexes) do (progn
                        (assert index () "index should not be nil")
                        (setf (nth i reordered-costs) (nth index *cost-factors))
                        (incf i)
                    ))
                    )
                    ; if index is nil (i.e. if this cost doesn't exist in this species)
                    (print (list "Cost " cost " was not found."))
                )
            )
        )
        (assert (eq i *N-COST-FACTORS) () "Some costs are missing in the reordering.")
        (dolist (cost reordered-costs) (assert cost () "A cost is nil. Ordered costs = ~A" reordered-costs))
        (setf *cost-factors reordered-costs)
    )
)

; @completely new or reworked
(defun fux-search-engine (the-cp &optional (species '(1)) (voice-type 0))
    (let (se tstop sopts)
        (print (list "Starting fux-search-engine with species = " species))
        ;; Reorder the costs
        (reorder-costs species)

        (setf linear-combination nil)
        ;; COST
        (if linear-combination 
            ; do a linear combination
            (progn
                (setq cost (gil::add-int-var-array *sp* 1 0 300))
                (gil::g-sum *sp* (first cost) *cost-factors)
                (gil::g-cost *sp* cost) ; set the cost function
            )
            ; else lexicographical order
            (gil::g-cost *sp* *cost-factors) ; set the cost function
        )

        ;; SPECIFY SOLUTION VARIABLES
        (print "Specifying solution variables...")
        (gil::g-specify-sol-variables *sp* the-cp)
        (gil::g-specify-percent-diff *sp* 0)
        
        ;; BRANCHING
        (print "Branching...")
        (setq var-branch-type gil::INT_VAR_DEGREE_MAX)
        (setq val-branch-type gil::INT_VAL_SPLIT_MIN)
        ;(setq var-branch-type gil::INT_VAR_SIZE_MIN)


        (dotimes (i *N-VOICES) (progn
            ; 5th species specific
            (if (eq (nth i species) 5) ; otherwise there is no species array
                (gil::g-branch *sp* (species-arr (nth i counterpoints)) var-branch-type gil::INT_VAL_RND)
            )

            ; 3rd and 5th species specific
            (if (or (eq (nth i species) 3) (eq (nth i species) 5)) (progn
               ; (gil::g-branch *sp* (m-degrees-cost (nth i counterpoints)) var-branch-type val-branch-type)
               ; (gil::g-branch *sp* (off-key-cost (nth i counterpoints)) var-branch-type val-branch-type)
            ))

            ; 5th species specific
            (if (and (eq (nth i species) 5) (>= voice-type 0)) (progn ; otherwise there is no species array
                    (gil::g-branch *sp* (no-syncope-cost (nth i counterpoints)) var-branch-type val-branch-type)
                    (gil::g-branch *sp* (not-cambiata-cost (nth i counterpoints)) var-branch-type val-branch-type)
            ))

            ; branching *total-cost
            ;(if (eq (nth i species) 2)
                ;(gil::g-branch *sp* *cost-factors var-branch-type val-branch-type) ;; TODO why would we do this?? -> asked by pano
            ;)
        ))
         
    
        ;; Solution variables branching
        (gil::g-branch *sp* the-cp var-branch-type val-branch-type)

        ; time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (setq timeout 5)
        (gil::time-stop-init tstop (* timeout 1000)); initialize it (time is expressed in ms)

        ; search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        ; (gil::set-n-threads sopts 1)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ;; SEARCH ENGINE
        (print "Search engine...")
        (setq se (gil::search-engine *sp* (gil::opts sopts) gil::BAB));
        (print se)

        (print "CSP constructed")
        (list se the-cp tstop sopts)
    )
)



; SEARCH-NEXT-SOLUTION
; <l> is a list containing in that order the search engine for the problem, the variables
; this function finds the next solution of the CSP using the search engine given as an argument
(defun search-next-fux-cp (l)
    (print "Searching next solution...")
    (let (
        (se (first l))
        (the-cp (second l))
        (tstop (third l))
        (sopts (fourth l))
        (species-list (fifth l))
        (check t)
        sol sol-pitches sol-species
        )

        (time (om::while check :do
            ; reset the tstop timer before launching the search
            (gil::time-stop-reset tstop)
            ; try to find a solution
            (time (setq sol (try-find-solution se)))
            (if (null sol)
                ; then check if there are solutions left and if the user wishes to continue searching
                (stopped-or-ended (gil::stopped se) (getparam 'is-stopped))
                ; else we have found a solution so break fthe loop
                (setf check nil)
            )
        ))

        ; print the solution from GiL
        (print "Solution: ")
        (print (list "h-intervals1 = " (gil::g-values sol (first (h-intervals (first counterpoints))))))
        (print (list "h-intervals2 = " (gil::g-values sol (first (h-intervals (second counterpoints))))))
        (print (list "h-intervals1-2 = " (gil::g-values sol (first *h-intervals-1-2))))
        (print (list "ALL_CONS_VAR = " (gil::g-values sol ALL_CONS_VAR)))
        (handler-case (print (list "is-cp1-bass = " (gil::g-values sol *is-cp1-bass-print))) (error (c)  (print "error with is-cp1-bass")))
        (handler-case (print (list "is-cp2-bass = " (gil::g-values sol *is-cp2-bass-print))) (error (c)  (print "error with is-cp2-bass")))
        (handler-case (print (list "is-cf-bass  = " (gil::g-values sol *is-cf-bass-print))) (error (c) (print "error with is-cf-bass")))
        
        
        ;(print (list "temp-1 = " (gil::g-values sol *temp-1)))
        ;(print (list "temp-2 = " (gil::g-values sol *temp-2)))
        ;(print (list "temp-1-2 = " (gil::g-values sol *temp-1-2)))
        (print (list "cp1 = " (gil::g-values sol (first (cp (first counterpoints))))))
        
        (handler-case
            (progn 
                (print (list "*cost-factors" (gil::g-values sol *cost-factors)))
                (print (list "current-cost = " (reduce #'+ (gil::g-values sol *cost-factors) :initial-value 0)))
            ) 
            (error (c)
                (error "All costs are not set correctly. Correct this problem before trying to find a solution.")
            )
        )
        (print (list "species = " species-list))
        
        (setq sol-pitches (gil::g-values sol the-cp)) ; store the values of the solution
        (print (list "sol-pitches  =" sol-pitches))

        (let (
            (basic-rythmics (get-basic-rythmics species-list *cf-len sol-pitches counterpoints sol))
            (sol-voices (make-list *N-VOICES :initial-element nil))
            )

            (loop for i from 0 below *N-VOICES do (progn
                (setq rythmic+pitches (nth i basic-rythmics)) ; get the rythmic correpsonding to the species
                (setq rythmic-om (first rythmic+pitches))
                (setq pitches-om (second rythmic+pitches))
            )

                (setf (nth i sol-voices) (make-instance 'voice :chords (to-midicent pitches-om) :tree (om::mktree rythmic-om '(4 4)) :tempo *cf-tempo))
            )
            (make-instance 'poly :voices sol-voices)
        )
    )
)

; try to find a solution, catch errors from GiL and Gecode and restart the search
(defun try-find-solution (se)
    (handler-case
        (gil::search-next se) ; search the next solution, sol is the space of the solution
        (error (c)
            (print "gil::Unexpected error. Please investigate.")
            ;(try-find-solution se)
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user)
    (print (list "stopped-se" stopped-se "stop-user" stop-user))
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
    )
)