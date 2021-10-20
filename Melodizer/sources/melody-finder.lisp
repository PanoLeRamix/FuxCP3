(in-package :om)

;;;====================
;;;= MELODIZER OBJECT =
;;;====================

;change the input-rhythm to be a voice object where we don't care about the pitch instead of a rhythm tree
(om::defclass! melodizer () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "The input chords on top of which the melody will be played in the form of a voice object.")
    ; maybe allow the rhythm to be a voice object as well?
    (input-rhythm :accessor input-rhythm :initarg :input-rhythm :initform nil :documentation "The rhythm of the melody in the form of a rhythm tree. To make the rhythm tree, express the notes with respect to a whole note (1/2 for half note, 1/4 for quarter note,... and the time signature.")
    (key :accessor key :initarg :key :initform 60 :documentation "The key of the melody is in (default : C).")
    (mode :accessor mode :initarg :mode :initform "major" :documentation "The mode the melody is in (default : major).")
    (tool-mode :accessor tool-mode :initarg :tool-mode :initform "Melody-Finder" :documentation "The mode of the tool, e.g given Melody-Finder if we want to find a melody, Accompagnement-Finder if we want to find an accompagnement, Ornement if we want to complexify the melody,...")
    (result :accessor result :initarg :result :initform (list) :documentation "A temporary list holder to store the result of the call to melody-finder, shouldn't be touched.")
    (solution :accessor solution :initarg :solution :initform nil :documentation "The solution of the CSP in the form of a voice object.")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melodizer.
        UPDATE THIS to a complete description of the tool")
)


;;; OBJECT EDITOR 
(defclass my-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melodizer)) t)
(defmethod om::get-editor-class ((self melodizer)) 'my-editor)

(defmethod om::om-draw-contents ((view my-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self my-editor) &rest args)

  ; To access the melodizer object, (object self)
  
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?, calculate the list of fundamentals, seconds,...
  
  (om::om-add-subviews 
    self

;;; pop-up menus

    ;pop-up list to select the mode of the tool (melodizer, accompagnement finder, ...)
    (om-make-dialog-item 
      'om::pop-up-menu 
      (om-make-point 30 50) 
      (om-make-point 200 20) 
      "Tool Mode selection"
      :range '("Melody-Finder" "Accompagnement-Finder" "Ornement")
      :di-action #'(lambda (m)
        ;(print (nth (om-get-selected-item-index m) (om-get-item-list m))); display the selected option
        (setf (tool-mode (object self)) (nth (om-get-selected-item-index m) (om-get-item-list m))) ; set the tool-mode according to the choice of the user
      )
    )

    ;pop-up list to select the key of the melody
    (om-make-dialog-item 
      'om::pop-up-menu 
      (om-make-point 30 80) 
      (om-make-point 80 20) 
      "Key selection"
      :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (note-value-to-name (key (object self)))
      :di-action #'(lambda (m)
        (setf (key (object self)) (name-to-note-value (nth (om-get-selected-item-index m) (om-get-item-list m)))) ; set the key according to the choice of the user
      )
    )

    ;pop-up list to select the mode of the melody
    (om-make-dialog-item 
      'om::pop-up-menu 
      (om-make-point 30 110) 
      (om-make-point 80 20) 
      "Mode selection"
      :range '("major" "minor"); add pentatonic, chromatic, ... ?
      :value (mode (object self))
      :di-action #'(lambda (m)
        (setf (mode (object self)) (nth (om-get-selected-item-index m) (om-get-item-list m))) ; set the mode according to the choice of the user
      )
    )

;;; text boxes (change that because for now they can be edited!)

    ;temporary replacement to the representation of the solution, to have an idea of the interface
    (om::om-make-dialog-item
      'om::text-box
      (om-make-point 250 100) 
      (om-make-point 400 150) 
      "Display of the solution" 
      :font *om-default-font1* 
    )

    ;text for the slider
    (om::om-make-dialog-item
      'om::text-box
      (om-make-point 660 100) 
      (om-make-point 200 20) 
      "Variety of the solutions" 
      :font *om-default-font1* 
    )

;;; buttons

    ; button to start or restart the search, not sure if I will keep it here
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 250 250) ; position (horizontal, vertical)
      (om::om-make-point 80 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b) 
                    ;(dolist (e (chords (input-chords (object self))))
                    ;  (print (lmidic e))
                    ;)
                    (cond
                      ((string-equal (tool-mode (object self)) "Melody-Finder"); melody finder mode, where the user gives as input a voice with chords
                        (let init; list to take the result of the call to melody-finder
                          (setq init (melody-finder (input-chords (object self)) (input-rhythm (object self)) (key (object self)) (mode (object self)))); get the search engine and the first solution of the CSP
                          ; update the fields of the object to their new value
                          ; modify so we only store one thing, and then we can pass that as an argument to search-next
                          (setf (result (object self)) init); store the result of the call to melody finder
                        )
                      )
                      ((string-equal (tool-mode (object self)) "Accompagnement-Finder"); not supported yet
                        (print "This mode is not supported yet")
                      )
                      ((string-equal (tool-mode (object self)) "Ornement"); not supported yet
                        (print "This mode is not supported yet")
                      )
                    )
                  )
    )

    ; button to find the next solution
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 330 250) ; position
      (om::om-make-point 80 20) ; size
      "Next"
      :di-action #'(lambda (b)
                    (print "Searching for the next solution")
                    (let sol 
                      ; modify this according to what is stated above
                      (setf (solution (object self)) (search-next-melody-finder (result (object self)) (input-rhythm (object self))))
                    )
                  )
    )

    ;button to add the solution to the list of solutions (if we find it interesting and want to keep it)
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 410 250) ; position
      (om::om-make-point 120 20) ; size
      "Save Solution"
      :di-action #'(lambda (b)
                    (print "TODO")
                  )
    )

;;; check-boxes

    ;example of a checkbox
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 200 350) ; position
      (om::om-make-point 20 20) ; size
      "Test"
      :di-action #'(lambda (c)
                    (print "checked")
                  )
    )

;;; sliders

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 660 130) ; position
      (om::om-make-point 200 20) ; size
      "Slider"
      :range '(1 100)
      :increment 1
      :value 1
      :di-action #'(lambda (s)
                    (print (om-slider-value s))
                  )
    )
  ); end of add subviews
  ; return the editor:
  self
)



