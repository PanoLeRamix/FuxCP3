(in-package :mldz)

; converts a list of MIDI values to MIDIcent
(defun to-midicent (l)
    (if (null l) 
        nil
        (cons (* 100 (first l)) (to-midicent (rest l)))
    )
)

; convert from MIDIcent to MIDI
(defun to-midi (l)
    (if (null l) 
        nil
        (cons (/ (first l) 100) (to-midi (rest l)))
    )
)

;function to get the starting times (in seconds) of the notes
; from karim haddad (OM)
(defmethod voice-onsets ((self voice))
  "on passe de voice a chord-seq juste pour avoir les onsets"
    (let ((obj (om::objfromobjs self (make-instance 'om::chord-seq))))
        (butlast (om::lonset obj))
    )
)

(defun notes-from-tonality (key mode)
    (let (admissible-notes note scale)
        (if (string-equal mode "major") ; maybe add a security so if the user types something wrong it doesn't set the mode to minor by default
            (setq scale (list 2 2 1 2 2 2 1)); major
            (setq scale (list 2 1 2 2 1 2 2)); minor
        )
        ; then, create a list and add the notes in it
        (setq note key)
        (setq i 0)
        (setq admissible-notes (list))
        ; add all notes over the key, then add all notes under the key
        (om::while (<= note 127) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list --(push note admissible-notes)?
            (if (>= i 7)
                (setq i 0)            
            )
            (incf note (nth i scale)); note = note + scale[i mod 6]
            (incf i 1); i++
        )
        (setq note key)
        (decf note (nth (- 6 0) scale)); note = note - scale[6-i mod 6]
        (setq i 1)

        (om::while (>= note 0) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list
            (if (>= i 7)
                (setq i 0)
            )
            (decf note (nth (- 6 i) scale)); note = note - scale[6-i mod 6]
            (incf i 1); i++
        )
        admissible-notes
    )
)

; function to get all of a given note (e.g. C)
(defun get-all-notes (note)
    (let ((acc '()) (backup note))
        (om::while (<= note 127) :do
            (setq acc (cons note acc)); add it to the list
            (incf note 12)
        )
        (setf note (- backup 12))
        (om::while (>= note 0) :do
            (setq acc (cons note acc)); add it to the list
            (decf note 12)
        )
        acc
    )
)

; function to get all notes playable on top of a given chord CHECK WHAT NOTES CAN BE PLAYED FOR OTHER CASES THAN M/m
(defun get-admissible-notes (chords mode)
    (let ((return-list '()))
        (cond
            ((string-equal mode "major"); on top of a major chord, you can play either of the notes from the chord though the preferred order is 1-5-3
                (setf return-list (reduce #'cons
                    (get-all-notes (first chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (second chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (third chords))
                    :initial-value return-list
                    :from-end t
                ))
            )
            ((string-equal mode "minor"); on top of a minor chord, you can play either of the notes from the chord though the preferred order is 1-5-3
                (setf return-list (reduce #'cons
                    (get-all-notes (first chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (second chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (third chords))
                    :initial-value return-list
                    :from-end t
                ))
            )
            ((string-equal mode "diminished")

            )
        )
    )
    
)

; function to get the mode of the chord (major, minor, diminished,...) and the inversion (0 = classical form, 1 = first inversion, 2 = second inversion)
(defun get-mode-and-inversion (intervals)
    (let ((major-intervals (list (list 4 3) (list 3 5) (list 5 4))); possible intervals in midicent for major chords
        (minor-intervals (list (list 3 4) (list 4 5) (list 5 3))) ; possible intervals in midicent for minor chords
        (diminished-intervals (list (list 3 3) (list 3 6) (list 6 3))))
        (cond 
            ((position intervals major-intervals :test #'equal); if the chord is major
                (list "major" (position intervals major-intervals :test #'equal))
            )
            ((position intervals minor-intervals :test #'equal); if the chord is minor
                (list "minor" (position intervals minor-intervals :test #'equal))
            )
            ((position intervals diminished-intervals :test #'equal); if the chord is diminished
                (list "diminished" (position intervals diminished-intervals :test #'equal))
            )
        )
    )
)

;converts the Value of a note to its name
(defmethod note-value-to-name (note)
    (cond 
        ((eq note 60) "C")
        ((eq note 61) "C#")
        ((eq note 62) "D")
        ((eq note 63) "Eb")
        ((eq note 64) "E")
        ((eq note 65) "F")
        ((eq note 66) "F#")
        ((eq note 67) "G")
        ((eq note 68) "Ab")
        ((eq note 69) "A")
        ((eq note 70) "Bb")
        ((eq note 71) "B")
    )
)

;converts the name of a note to its value
(defmethod name-to-note-value (name)
    (cond 
        ((string-equal name "C") 60)
        ((string-equal name "C#") 61)
        ((string-equal name "D") 62)
        ((string-equal name "Eb") 63)
        ((string-equal name "E") 64)
        ((string-equal name "F") 65)
        ((string-equal name "F#") 66)
        ((string-equal name "G") 67)
        ((string-equal name "Ab") 68)
        ((string-equal name "A") 69)
        ((string-equal name "Bb") 70)
        ((string-equal name "B") 71)
    )
)

;makes a list (name voice-instance) from a list of voices:
(defun make-data-sol (liste)
  (loop for l in liste
        for i from 1 to (length liste)
        collect (list (format nil "melody ~D: ~A"  i l) l)))
    

; taken from rhythm box (add link)
; https://github.com/blapiere/Rhythm-Box

(defun rel-to-gil (rel)
"Convert a relation operator symbol to a GiL relation value."
    (cond
        ((eq rel '=) gil::IRT_EQ)
        ((eq rel '=/=) gil::IRT_NQ)
        ((eq rel '<) gil::IRT_LE)
        ((eq rel '=<) gil::IRT_LQ)
        ((eq rel '>) gil::IRT_GR)
        ((eq rel '>=) gil::IRT_GQ)
    )
)


; this is not used but kept in case it is needed
; shuffles a list
; from https://gist.github.com/shortsightedsid/62d0ee21bfca53d9b69e
(defun list-shuffler (input-list &optional accumulator)
  "Shuffle a list using tail call recursion."
  (if (eq input-list nil)
      accumulator
      (progn
	(rotatef (car input-list) 
		 (nth (random (length input-list)) input-list))
	(list-shuffler (cdr input-list) 
				 (append accumulator (list (car input-list)))))))


; takes a rhythm tree as argument and returns the number of events in it (doesn't work with dotted notes for now)
;; (defmethod get-events-from-rtree (rtree)
;;     (let ((l (first (rest rtree))) ; get the first element of the list
;;         (nb 0)); the number of events
;;         (dolist (bar l); for each bar
;;             (dolist (elem (second bar)); count each event in the bar
;;                 (if (typep elem 'list); if the element of the bar is a list
;;                     (dolist (event (second elem))
;;                         ;(print event)
;;                         (setq nb (+ nb 1))
;;                     )
;;                     (setq nb (+ nb 1)); if it is just a number
;;                 )
;;             )
;;         )
;;         ;(print nb)
;;         nb
;;     )
;; )

;; ; function to get the mode of a chord from the intervals between the notes
;; (defun get-mode (intervals)
;;     (let ((first (first intervals))
;;         (second (second intervals)))
;;         (cond
;;             ((or (and (eq first 400) (eq second 300)) (and (eq first 300) (eq second 500)) (and (eq first 500) (eq second 400)))
;;                 "major"
;;             ) ; if intervals = (4,3) or (3,5) or (5,4) this is a major chord
;;             ((or (and (eq first 300) (eq second 400)) (and (eq first 400) (eq second 500)) (and (eq first 500) (eq second 300)))
;;                 "minor"
;;             ) ; if intervals = (3,4) or (4,5) or (5,3) this is a minor chord
;;             ; add other types of chords (diminished, augmented, ...)
;;         )
;;     )
;; )