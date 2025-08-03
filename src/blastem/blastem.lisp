(in-package :clg-blastem)

(defconstant +SHIP-W+ 12 "Width of ship bitmap in pixels.")
(defconstant +SHIP-H+ 13 "Height of ship bitmap in pixels.")

(defconstant +SHIP-SHOT-W+ 2 "Width of ship shot in pixels.")
(defconstant +SHIP-SHOT-H+ 9 "Height of ship shot in pixels.")

(defconstant +LIFE-W+ 6 "Width of life icon in pixelss.")
(defconstant +LIFE-H+ 6 "Height of life icon in pixels.")

(defconstant +ALIEN-BUG-W+ 14 "Width of alien bug type in pixels.")
(defconstant +ALIEN-BUG-H+ 9 "Height of alien bug type in pixels.")

(defconstant +ALIEN-ARROW-W+ 13 "Width of alien arrow type in pixels.")
(defconstant +ALIEN-ARROW-H+ 10 "Height of alien arrow type in pixels.")

(defconstant +ALIEN-LARGE-W+ 45 "Width of alien large type in pixels.")
(defconstant +ALIEN-LARGE-H+ 27 "Height of alien large type in pixels.")

(defconstant +ALIEN-TYPE-BUG+ 0)
(defconstant +ALIEN-TYPE-ARROW+ 1)
(defconstant +ALIEN-TYPE-LARGE+ 2)
(defconstant +ALIEN-TYPE-N+ 3)

;;;; class config -------------------------------------------------------------

(defclass config ()
    ((buffer-w :initform 320 :type integer :accessor config-buffer-w)
     (buffer-h :initform 240 :type integer :accessor config-buffer-h)
     (display-scale :initform 5 :type integer :accessor config-display-scale)

     ;; Auto-calculated
     (display-width :initform 0 :type integer :reader config-display-width)
     (display-height :initform 0 :type integer :reader config-display-height)
     (ship-speed :initform 3 :type integer :reader config-ship-speed)
     (ship-max-x :initform 0 :type integer :reader config-ship-max-x)
     (ship-max-y :initform 0 :type integer :reader config-ship-max-y)

     ;; Runtime/performance configuration
     (fx-count :initform 128 :type integer :reader config-fx-count)
     (shots-count :initform 128 :type integer :reader config-shots-count)
     (aliens-count :initform 16 :type integer :reader config-aliens-count)
     (stars-count :initform 0 :type integer :reader config-stars-count)
     ))

(defmethod initialize-instance :after ((obj config) &key)
  (setf (slot-value obj 'display-width) (* (config-buffer-w obj) (config-display-scale  obj)))
  (setf (slot-value obj 'display-height) (* (slot-value obj 'buffer-h) (slot-value obj 'display-scale)))
  (setf (slot-value obj 'ship-max-x) (- (slot-value obj 'buffer-w) +SHIP-W+))
  (setf (slot-value obj 'ship-max-y) (- (slot-value obj 'buffer-h) +SHIP-H+))
  (setf (slot-value obj 'stars-count) (- (/ (slot-value obj 'buffer-w) 2) 1)))

(defmethod (setf config-buffer-w) :after (newval object)
  (setf (slot-value object 'display-width) (* (config-buffer-w object) (config-display-scale object)))
  (setf (slot-value object 'ship-max-x) (- newval +SHIP-W+))
  (setf (slot-value object 'stars-count) (- (/ newval 2) 1)))

(defmethod (setf config-buffer-h) :after (newval object)
  (setf (slot-value object 'display-height) (* (config-buffer-h object) (config-display-scale object)))
  (setf (slot-value object 'ship-max-y) (- newval +SHIP-H+)))

(defmethod (setf config-display-scale) :after (newval object)
  (setf (slot-value object 'display-width) (* (config-buffer-w object) newval))
  (setf (slot-value object 'display-height) (* (config-buffer-h object) newval)))

(defvar *CONFIG* :type "config")

;;;; class data ---------------------------------------------------------------

(defclass data ()
  ((display :initform (null-pointer) :type foreign-pointer :accessor data-display)
   (buffer :initform (null-pointer) :type foreign-pointer :accessor data-buffer)
   (frames :initform 0 :type integer :accessor data-frames)
   (score :initform 0 :type integer :accessor data-score)
   (key :initform (make-array +KEY-MAX+ :adjustable nil) :accessor data-key)
   (sprites :initform (make-sprites) :accessor data-sprites)
   (sample-shot :initform (null-pointer) :accessor data-sample-shot)
   (sample-explode :initform (make-array 2 :initial-element (null-pointer) :adjustable nil) :accessor data-sample-explode)
   (fx :initform (make-array (config-fx-count *CONFIG*) :element-type 'FX :adjustable nil) :accessor data-fx)
   (shots :initform (make-array (config-shots-count *CONFIG*) :element-type 'SHOT :adjustable nil) :accessor data-shots)
   (ship :initform (make-ship) :type SHIP :accessor data-ship)
   (aliens :initform (make-array (config-aliens-count *CONFIG*) :element-type 'ALIEN) :accessor data-aliens)
   (stars :initform (make-array (config-stars-count *CONFIG*) :element-type 'STAR) :accessor data-stars)
   (font :initform (null-pointer) :type foreign-pointer :accessor data-font)
   (score-display :initform 0 :type integer :accessor data-score-display)
   ))

(defvar *DATA* :type "data")

;;;; Common -------------------------------------------------------------------

(defun asset (path)
  (namestring (asdf:system-relative-pathname "blastok" (concatenate 'string "assets/blastem/" path))))

;;;; Display ------------------------------------------------------------------

(defun disp-init ()
  "Initialize display."
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 8 :suggest)

  ;; Allocate screen
  (setf (data-display *DATA*) (al:create-display (config-display-width *CONFIG*)
                                                 (config-display-height *CONFIG*)))
  (must-init (data-display *DATA*) "display")

  ;; Allocate draw buffer
  (setf (data-buffer *DATA*) (al:create-bitmap (config-buffer-w *CONFIG*)
                                               (config-buffer-h *CONFIG*)))
  (must-init (data-buffer *DATA*) "buffer"))

(defun disp-deinit ()
  "Clean up display."
  (al:destroy-bitmap (data-buffer *DATA*))
  (setf (data-buffer *DATA*) (null-pointer))
  (al:destroy-display (data-display *DATA*))
  (setf (data-display *DATA*) (null-pointer)))

(defun disp-pre-draw ()
  "Prepare to draw a game frame."
  ;; Draw to offscreen buffer
  (al:set-target-bitmap (data-buffer *DATA*)))

(defun disp-post-draw ()
  "Finish drawing game frame."
  ;; Draw to screen
  (al:set-target-backbuffer (data-display *DATA*))

  ;; Copy off screen buffer to screen
  (al:draw-scaled-bitmap (data-buffer *DATA*)
                         0 0 (config-buffer-w *CONFIG*) (config-buffer-h *CONFIG*)
                         0 0 (config-display-width *CONFIG*) (config-display-height *CONFIG*)
                         0)

  ;; Show frame to user
  (al:flip-display))

;;;; Keyboard -----------------------------------------------------------------

(defvar *KEY-STATE-SEEN* (byte 1 0)
  "Within our *key* array, the false (0) and true (1) bit for any specified key
   having been seen before (regardless of whether its currently down or up).")
(defvar *KEY-STATE-DOWN* (byte 1 1)
  "Within out *key* array, the false (0) and true (1) bit for any specified key
   currently being down.")

(defun keyboard-init ()
  "Initialize keyboard."
  (dotimes (i +KEY-MAX+)
	(setf (aref (data-key *DATA*) i) 0)))

(defun keyboard-deinit ()
  "Claen up keyboard"
  nil)

(defun keyboard-update (event)
  "Handle keyboard event related logic."
  (case (foreign-slot-value event '(:union al:event) 'al::type)
	(:timer (dotimes (index +KEY-MAX+)
			  (let ((old (aref (data-key *DATA*) index)))
				(setf (aref (data-key *DATA*) index) (dpb 0 *KEY-STATE-SEEN* old)))))
    
	(:key-down (let* ((key (foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode))
					  (keyval (foreign-enum-value 'al::keycodes key))
                      (old (aref (data-key *DATA*) keyval)))
 				 (setf old (dpb 1 *KEY-STATE-DOWN* old))
 				 (setf old (dpb 1 *KEY-STATE-SEEN* old))
                 (setf (aref (data-key *DATA*) keyval) old)))
    
	(:key-up (let* ((key (foreign-slot-value event '(:struct al:keyboard-event) 'al::keycode))
					(keyval (foreign-enum-value 'al::keycodes key))
 					(old (aref (data-key *DATA*) keyval)))
 			   (setf (aref (data-key *DATA*) keyval) (dpb 0 *KEY-STATE-DOWN* old))))))

;;;; Sprites ------------------------------------------------------------------

(defparameter *ALIEN-W* (make-array 3 :element-type 'integer :adjustable nil)
  "Width of alien bitmaps in pixels.")

(defparameter *ALIEN-H* (make-array 3 :element-type 'integer :adjustable nil)
  "Height of alien bitmaps in pixels.")

(defconstant +ALIEN-SHOT-W+ 4 "Width of alien ships shot in pixels.")
(defconstant +ALIEN-SHOT-H+ 4 "Height of alien ships shot in pixels.")

(defconstant +EXPLOSION-FRAMES+ 4 "Number of frames for explisions.")
(defconstant +SPARKS-FRAMES+ 3 "Numberof frames for sparks.")

(defstruct SPRITES
  (sheet (null-pointer))
  (ship (null-pointer))
  (ship-shot (make-array 2 :initial-element (null-pointer) :adjustable nil))
  (life (null-pointer))
  (alien (make-array 3 :initial-element (null-pointer) :adjustable nil))
  (alien-shot (null-pointer))
  (explosion (make-array +EXPLOSION-FRAMES+ :initial-element (null-pointer) :adjustable nil))
  (sparks (make-array +SPARKS-FRAMES+ :initial-element (null-pointer) :adjustable nil))
  (powerup (make-array 4 :initial-element (null-pointer) :adjustable nil)))

(defun sprite-grab(x y w h)
  (let ((sprite (al:create-sub-bitmap (sprites-sheet (data-sprites *DATA*)) x y w h)))
	(must-init sprite "sprite grab")
	sprite))

(defmacro sprites-ship-shots-i (o i)
  "Syntactic sugar."
    `(aref (sprites-ship-shot ,o) ,i))

(defmacro sprites-aliens-i (o i)
  "Syntactic sugar."
  `(aref (sprites-alien ,o) ,i))

(defmacro sprites-explosions-i (o i)
  "Syntactic sugar."
  `(aref (sprites-explosion ,o) ,i))

(defmacro sprites-sparkses-i (o i)
  "Syntactic sugar."
  `(aref (sprites-sparks ,o) ,i))

(defmacro sprites-powerups-i (o i)
  "Syntactic sugar."
  `(aref (sprites-powerup ,o) ,i))

(defun sprites-init ()
  ;; Load image with sprites
  (setf (sprites-sheet (data-sprites *DATA*)) (al:load-bitmap (asset "spritesheet.png")))
  (must-init (sprites-sheet (data-sprites *DATA*)) "spritesheet")

  ;; Grab all the sprites
  (setf (sprites-ship (data-sprites *DATA*)) (sprite-grab 0 0 +SHIP-W+ +SHIP-H+))
  (setf (sprites-ship-shots-i (data-sprites *DATA*) 0) (sprite-grab 13 0 +SHIP-SHOT-W+ +SHIP-SHOT-H+))
  (setf (sprites-ship-shots-i (data-sprites *DATA*) 1) (sprite-grab 16 0 +SHIP-SHOT-W+ +SHIP-SHOT-H+))
  (setf (sprites-life (data-sprites *DATA*)) (sprite-grab 0 14 +LIFE-W+ +LIFE-H+))
  (setf (sprites-aliens-i (data-sprites *DATA*) 0) (sprite-grab 19 0 +ALIEN-BUG-W+ +ALIEN-BUG-H+))
  (setf (sprites-aliens-i (data-sprites *DATA*) 1) (sprite-grab 19 10 +ALIEN-ARROW-W+ +ALIEN-ARROW-H+))
  (setf (sprites-aliens-i (data-sprites *DATA*) 2) (sprite-grab 0 21 +ALIEN-LARGE-W+ +ALIEN-LARGE-H+))
  (setf (sprites-alien-shot (data-sprites *DATA*)) (sprite-grab 13 10 +ALIEN-SHOT-W+ +ALIEN-SHOT-H+))
  (setf (sprites-explosions-i (data-sprites *DATA*) 0) (sprite-grab 33 10 9 9))
  (setf (sprites-explosions-i (data-sprites *DATA*) 1) (sprite-grab 43 9 11 11))
  (setf (sprites-explosions-i (data-sprites *DATA*) 2) (sprite-grab 46 21 17 18))
  (setf (sprites-explosions-i (data-sprites *DATA*) 3) (sprite-grab 46 40 17 17))
  (setf (sprites-sparkses-i (data-sprites *DATA*) 0) (sprite-grab 34 0 10 8))
  (setf (sprites-sparkses-i (data-sprites *DATA*) 1) (sprite-grab 45 0 7 8))
  (setf (sprites-sparkses-i (data-sprites *DATA*) 2) (sprite-grab 54 0 9 8))
  (setf (sprites-powerups-i (data-sprites *DATA*) 0) (sprite-grab 0 49 9 12))
  (setf (sprites-powerups-i (data-sprites *DATA*) 1) (sprite-grab 10 49 9 12))
  (setf (sprites-powerups-i (data-sprites *DATA*) 2) (sprite-grab 20 49 9 12))
  (setf (sprites-powerups-i (data-sprites *DATA*) 3) (sprite-grab 30 49 9 12)))

(defun sprites-deinit ()
  (macrolet ((release (obj)
               `(al:destroy-bitmap ,obj)
               `(setf ,obj (null-pointer))))
    ;; Let go of all the sprites
    (release (sprites-ship (data-sprites *DATA*)))
    (release (sprites-ship-shots-i (data-sprites *DATA*) 0))
    (release (sprites-ship-shots-i (data-sprites *DATA*) 1))
    (release (sprites-life (data-sprites *DATA*)))
    (release (sprites-aliens-i (data-sprites *DATA*) 0))
    (release (sprites-aliens-i (data-sprites *DATA*) 1))
    (release (sprites-aliens-i (data-sprites *DATA*) 2))
    (release (sprites-alien-shot (data-sprites *DATA*)))
    (release (sprites-explosions-i (data-sprites *DATA*) 0))
    (release (sprites-explosions-i (data-sprites *DATA*) 1))
    (release (sprites-explosions-i (data-sprites *DATA*) 2))
    (release (sprites-explosions-i (data-sprites *DATA*) 3))
    (release (sprites-sparkses-i (data-sprites *DATA*) 0))
    (release (sprites-sparkses-i (data-sprites *DATA*) 1))
    (release (sprites-sparkses-i (data-sprites *DATA*) 2))
    (release (sprites-powerups-i (data-sprites *DATA*) 0))
    (release (sprites-powerups-i (data-sprites *DATA*) 1))
    (release (sprites-powerups-i (data-sprites *DATA*) 2))
    (release (sprites-powerups-i (data-sprites *DATA*) 3))
    
    ;; Let go of sprites image
    (release (sprites-sheet (data-sprites *DATA*)))))

;;;; Audio --------------------------------------------------------------------

(defun audio-init ()
  (al:install-audio)
  (al:init-acodec-addon)

  ;; Create voice, mixer, and reserve somewhere to play samples
  (al:reserve-samples 128)

  ;; Load ship and alien shooting sound
  (setf (data-sample-shot *DATA*) (al:load-sample (asset "shot.flac")))
  (must-init (data-sample-shot *DATA*) "shot sample")

  ;; Load explosion sounds
  (setf (aref (data-sample-explode *DATA*) 0) (al:load-sample (asset "explode1.flac")))
  (must-init (aref (data-sample-explode *DATA*) 0) "explode[0] sample")
  (setf (aref (data-sample-explode *DATA*) 1) (al:load-sample (asset "explode2.flac")))
  (must-init (aref (data-sample-explode *DATA*) 1) "explode[1] sample"))

(defun audio-deinit ()
  (macrolet ((release (obj)
               `(al:destroy-sample ,obj)
               `(setf ,obj (null-pointer))))
    (release (data-sample-shot *DATA*))
    (release (aref (data-sample-explode *DATA*) 0))
    (release (aref (data-sample-explode *DATA*) 1))))

;;;; FX -----------------------------------------------------------------------

(defstruct FX
  (x 0 :type integer)
  (y 0 :type integer)
  (frame 0 :type integer)
  (spark nil :type boolean)
  (used nil :type boolean))

(defmacro fx-i (i)
  `(aref (data-fx *DATA*) ,i))

(defun fx-init ()
  (dotimes (i (config-fx-count *CONFIG*))
    (setf (fx-i i) (make-fx :x 0 :y 0 :frame 0 :spark nil :used nil))))

(defun fx-add (spark x y)
  (unless spark
	(let* ((once (foreign-enum-value 'al::playmode :once))
           (rand-sample (between 0 2))
           (sample (aref (data-sample-explode *DATA*) rand-sample)))
     (al:play-sample sample 0.75 0 1 once (null-pointer))))
  (symbol-macrolet ((fx (fx-i i)))
   (dotimes (i (config-fx-count *CONFIG*))
	 (unless (fx-used fx)
	   (setf (fx-x fx) x)
	   (setf (fx-y fx) y)
	   (setf (fx-frame fx) 0)
	   (setf (fx-spark fx) spark)
	   (setf (fx-used fx) t)
	   (return)))))

(defun fx-update ()
  ;; For all fx's
  (dotimes (i (config-fx-count *CONFIG*))
    
	;; If its in use
	(when (fx-used (fx-i i))
      
	  ;; Incremenet next frame to show
	  (incf (fx-frame (fx-i i)) 1)
      
	  ;; If previous frame was last
	  (when (or (and (not (fx-spark (fx-i i)))
					 (eql (fx-frame (fx-i i)) (* +EXPLOSION-FRAMES+ 2)))
				(and (fx-spark (fx-i i))
					 (eql (fx-frame (fx-i i)) (* +SPARKS-FRAMES+ 2))))
		;; Disable the fx
		(setf (fx-used (fx-i i)) nil)))))

(defun fx-draw ()
  ;; For all fx's
  (dotimes (i (config-fx-count *CONFIG*))
	;; If its in use
	(when (fx-used (aref (data-fx *DATA*) i))
	  ;; Draw the next frame
	  (let* ((frame-display (truncate (/ (fx-frame (fx-i i)) 2)))
			 (bmp (if (fx-spark (fx-i i))
					  (aref (sprites-sparks (data-sprites *DATA*)) frame-display)
					  (aref (sprites-explosion (data-sprites *DATA*)) frame-display)))
			 (x (- (fx-x (fx-i i)) (/ (al:get-bitmap-width bmp) 2)))
			 (y (- (fx-y (fx-i i)) (/ (al:get-bitmap-height bmp) 2))))
		(al:draw-bitmap bmp x y 0)))))

;;;; Shots --------------------------------------------------------------------

(defstruct SHOT
  (x 0 :type integer)
  (y 0 :type integer)
  (dx 0 :type integer)
  (dy 0 :type integer)
  (frame 0 :type integer)
  (ship nil :type boolean)
  (used nil :type boolean))

(defmacro shot-i (i)
  `(aref (data-shots *DATA*) ,i))

(defun shots-init ()
  (dotimes (i (config-shots-count *CONFIG*))
    (setf (shot-i i) (make-shot :x 0 :y 0 :dx 0 :dy 0 :frame 0 :ship nil :used nil))))

(defun shots-add (ship straight x y)
  ;; User needs feedback regardless of whether we have a free shot
  (al:play-sample (data-sample-shot *DATA*) 0.3 0
    			  (if ship 1.0 (between-f 1.5 1.6))
                  :once (null-pointer))

  (symbol-macrolet ((shot (shot-i i)))
    (dotimes (i (config-shots-count *CONFIG*))
      (unless (shot-used shot)
        (setf (shot-ship shot) ship)
        (if ship
            ;; Player ship
            (progn
              (setf (shot-x shot) (round (- x (/ +SHIP-SHOT-W+ 2))))
              (setf (shot-y shot) y))
            ;; Alien ship
            (progn
              (setf (shot-x shot) (round (- x (/ +ALIEN-SHOT-W+ 2)))
                    (shot-y shot) (round (- y (/ +ALIEN-SHOT-H+ 2))))
              (if straight
                  (setf (shot-dx shot) 0
                        (shot-dy shot) 2)
                  (setf (shot-dx shot) (between -2 2)
                        (shot-dy shot) (between -2 2)))
              (if (and (= (shot-dx shot) 0)
                       (= (shot-dy shot) 0))
                  (return-from shots-add t))
              (setf (shot-frame shot) 0)))
        (setf (shot-frame shot) 0)
        (setf (shot-used shot) t)
        (return-from shots-add t))))
  nil)

(defun shots-update ()
  (symbol-macrolet ((shot (shot-i i)))
    (dotimes (i (config-shots-count *CONFIG*))
      (block shots-loop
        (if (not (shot-used shot))
            (return-from shots-loop))
        (if (shot-ship shot)
            ;; Player ship shot
            (progn
              (decf (shot-y shot) 1)
              (if (< (shot-y shot) (- +SHIP-SHOT-H+))
                  (progn
                    (setf (shot-used shot) nil)
                    (return-from shots-loop))))
            ;; Alien ship shot
            (progn
              (incf (shot-x shot) (shot-dx shot))
              (incf (shot-y shot) (shot-dy shot))
              (let ((sx (shot-x shot))
                    (sy (shot-y shot)))
               (if (or (< sx (- +ALIEN-SHOT-W+))
                       (> sx (config-buffer-w *CONFIG*))
                       (< sy (- +ALIEN-SHOT-H+))
                       (> sy (config-buffer-h *CONFIG*)))
                   (progn
                     (setf (shot-used shot) nil)
                     (return-from shots-loop))))))
        (decf (shot-frame shot) 1)))))

(defun shots-collide (ship x y w h)
  (symbol-macrolet ((shot (shot-i i)))
    (dotimes (i (config-shots-count *CONFIG*))
      (block shots-loop
        (if (not (shot-used shot))
            (return-from shots-loop))
        (if (or (and ship (shot-ship shot))
                (and (not ship) (not (shot-ship shot))))
            (return-from shots-loop))
        (let ((sw)
              (sh))
          (if ship
            (setf sw +ALIEN-SHOT-W+
                  sh +ALIEN-SHOT-H+)
            (setf sw +SHIP-SHOT-W+
                  sh +SHIP-SHOT-H+))
          (if (rect-collide x y (+ x w) (+ y h) (shot-x shot) (shot-y shot)
                       (+ (shot-x shot) sw) (+ (shot-y shot) sh))
              (progn
                (fx-add t (+ (shot-x shot) (round (/ sw 2))) (+ (shot-y shot) (round (/ sh 2))))
                (setf (shot-used shot) nil)
                (return-from shots-collide t)))))))
  nil)

(defun shots-draw ()
  (symbol-macrolet ((shot (shot-i i)))
   (dotimes (i (config-shots-count *CONFIG*))
	 (when (shot-used shot)
	   (let ((frame-display (truncate (mod (/ (shot-frame shot) 2) 2))))
		 (if (shot-ship shot)
			 (al:draw-bitmap (aref (sprites-ship-shot (data-sprites *DATA*)) frame-display)
                             (shot-x shot) (shot-y shot) 0)
			 (let ((tint (if (> frame-display 0)
                             (al:map-rgb-f 1 1 1)
                             (al:map-rgb-f 0.5 0.5 0.5))))
			   (al:draw-tinted-bitmap (sprites-alien-shot (data-sprites *DATA*)) tint
									  (shot-x shot) (shot-y shot) 0))))))))

;;;; Ships --------------------------------------------------------------------

(defstruct SHIP
  (x 0 :type integer)
  (y 0 :type integer)
  (shot-timer 0 :type integer)
  (lives 0 :type integer)
  (respawn-timer 0 :type integer)
  (invincible-timer 0 :type integer))

(defun ship-init ()
  (setf (ship-x (data-ship *DATA*)) (round (- (/ (config-buffer-w *CONFIG*) 2) (/ +SHIP-W+ 2))))
  (setf (ship-y (data-ship *DATA*)) (round (- (/ (config-buffer-h *CONFIG*) 2) (/ +SHIP-H+ 2))))
  (setf (ship-shot-timer (data-ship *DATA*)) 0)
  (setf (ship-lives (data-ship *DATA*)) 3)
  (setf (ship-respawn-timer (data-ship *DATA*)) 0)
  (setf (ship-invincible-timer (data-ship *DATA*)) 120))

(defun ship-update ()
  ;; If no lives left, nothing to check
  (if (< (ship-lives (data-ship *DATA*)) 0)
      (return-from ship-update))
  
  ;; If ship isn't active, nothing to do
  (if (> (ship-respawn-timer (data-ship *DATA*)) 0)
      (progn
        (decf (ship-respawn-timer (data-ship *DATA*)) 1)
        (return-from ship-update)))
  
  ;; Move horizontally if requested
  (if (> (aref (data-key *DATA*) +KEY-LEFT+) 0)
      (decf (ship-x (data-ship *DATA*)) (config-ship-speed *CONFIG*)))
  (if (> (aref (data-key *DATA*) +KEY-RIGHT+) 0)
      (incf (ship-x (data-ship *DATA*)) (config-ship-speed *CONFIG*)))
  
  ;; Move vertically if requested
  (if (> (aref (data-key *DATA*) +KEY-UP+) 0)
      (decf (ship-y (data-ship *DATA*)) (config-ship-speed *CONFIG*)))
  (if (> (aref (data-key *DATA*) +KEY-DOWN+) 0)
      (incf (ship-y (data-ship *DATA*)) (config-ship-speed *CONFIG*)))
  
  ;; Make sure ship stays in the screen area
  (if (< (ship-x (data-ship *DATA*)) 0)
      (setf (ship-x (data-ship *DATA*)) 0))
  (if (< (ship-y (data-ship *DATA*)) 0)
      (setf (ship-y (data-ship *DATA*)) 0))
  (if (> (ship-x (data-ship *DATA*)) (config-ship-max-x *CONFIG*))
      (setf (ship-x (data-ship *DATA*)) (config-ship-max-x *CONFIG*)))
  (if (> (ship-y (data-ship *DATA*)) (config-ship-max-y *CONFIG*))
      (setf (ship-y (data-ship *DATA*)) (config-ship-max-y *CONFIG*)))

  ;; Can ship be shot?
  (if (> (ship-invincible-timer (data-ship *DATA*)) 0)
      ;; Nope, its invincible!
      (decf (ship-invincible-timer (data-ship *DATA*)) 1)
      ;; Has a shot hit the ship?
      (if (shots-collide t (ship-x (data-ship *DATA*)) (ship-y (data-ship *DATA*)) +SHIP-W+ +SHIP-H+)
          (progn
            (let ((x (round (+ (ship-x (data-ship *DATA*)) (/ +SHIP-W+ 2))))
                  (y (round (+ (ship-y (data-ship *DATA*)) (/ +SHIP-H+ 2)))))
              (fx-add nil x y)
              (fx-add nil (+ x 4) (+ y 2))
              (fx-add nil (- x 2) (- y 4))
              (fx-add nil (+ x 1) (- y 5))
              (decf (ship-lives (data-ship *DATA*)) 1)
              (setf (ship-respawn-timer (data-ship *DATA*)) 90)
              (setf (ship-invincible-timer (data-ship *DATA*)) 180)))))
  
  ;; Can ship shoot?
  (if (> (ship-shot-timer (data-ship *DATA*)) 0)
      ;; Nope, check next frame
      (decf (ship-shot-timer (data-ship *DATA*)) 1)
      ;; It can, has one been requested?
      (if (> (aref (data-key *DATA*) +KEY-X+) 0)
          (progn
            (let ((x (round (+ (ship-x (data-ship *DATA*)) (/ +SHIP-W+ 2)))))
              (if (shots-add t nil x (ship-y (data-ship *DATA*)))
                  (progn
                    (setf (ship-shot-timer (data-ship *DATA*)) 5)
                    )))))))

(defun ship-draw ()
  (when (and (> (ship-lives (data-ship *DATA*)) 0)
             (= (ship-respawn-timer (data-ship *DATA*)) 0))
    ;; If ship is invincible, only allow it to be drawn every 3rd frame so it blinks
    ;; TODO:  Use unless ...
    (if (= (mod (/ (ship-invincible-timer (data-ship *DATA*)) 2) 3) 1)
        (return-from ship-draw))
    (al:draw-bitmap (sprites-ship (data-sprites *DATA*)) (ship-x (data-ship *DATA*)) (ship-y (data-ship *DATA*)) 0)))

;; --- Aliens ---

(defstruct ALIEN
  (x 0 :type integer)
  (y 0 :type integer)
  (type 0 :type integer)
  (shot-timer 0 :type integer)
  (blink 0 :type integer)
  (life 0 :type integer)
  (used nil :type boolean))

(defmacro alien-i (i)
  `(aref (data-aliens *DATA*) ,i))

(defun aliens-init ()
  (dotimes (i (config-aliens-count *CONFIG*))
    (setf (alien-i i) (make-alien :x 0 :y 0 :type 0 :shot-timer 0 :blink 0 :life 0 :used nil))))

(defun aliens-update ()
  (let ((new-quota (if (= 0 (mod (data-frames *DATA*) 120)) (between 2 4) 0))
		(new-x (between 10 (- (config-buffer-w *CONFIG*) 50))))
    ;; For each alien
    (dotimes (i (config-aliens-count *CONFIG*))
      (symbol-macrolet ((alien (alien-i i)))
        (block each-alien
          ;; If its not used
          (if (not (alien-used alien))
              ;; If we need to spawn new aliens
              (progn
                (if (> new-quota 0)
                    (progn
                      (incf new-x (between 40 80))
                      (if (> new-x (- (config-buffer-w *CONFIG*) 60))
                          (decf new-x (- (config-buffer-w *CONFIG*) 60)))
                      (setf (alien-x alien) new-x)
                      (setf (alien-y alien) (between -40 -30))
                      (setf (alien-type alien) (between 0 +ALIEN-TYPE-N+))
                      (setf (alien-shot-timer alien) (between 1 99))
                      (setf (alien-blink alien) 0)
                      (setf (alien-used alien) t)
                      (cond
                        ((= (alien-type alien) +ALIEN-TYPE-BUG+) (setf (alien-life alien) 4))
                        ((= (alien-type alien) +ALIEN-TYPE-ARROW+) (setf (alien-life alien) 2))
                        ((= (alien-type alien) +ALIEN-TYPE-LARGE+) (setf (alien-life alien) 12)))
                      (decf new-quota 1)))
                (return-from each-alien)))
          
          ;; Move alien down
          (let ((type (alien-type alien)))
            (cond
              ((= type +ALIEN-TYPE-BUG+) (if (> (mod (data-frames *DATA*) 2) 0)
                                              (incf (alien-y alien) 1)))
              ((= type +ALIEN-TYPE-ARROW+) (incf (alien-y alien) 1))
              ((= type +ALIEN-TYPE-LARGE+) (if (> (mod (data-frames *DATA*) 4))
                                                  (incf (alien-y alien) 1)))))

          ;; If the alien is below bottom, then remove it
          (if (>= (alien-y alien) (config-buffer-w *CONFIG*))
              (progn
                (setf (alien-used alien) nil)
                (return-from each-alien)))
          
          ;; If alien is still blinking...
          (if (> (alien-blink alien) 0)
              (decf (alien-blink alien) 1))
          
          ;; If alien hit by shot
          (if (shots-collide nil (alien-x alien) (alien-y alien)
                             (elt *ALIEN-W* (alien-type alien))
                             (elt *ALIEN-H* (alien-type alien)))
              (progn
                (decf (alien-life alien) 1)
                (setf (alien-blink alien) 4)))
          
          (let ((cx (round (+ (alien-x alien) (/ (elt *ALIEN-W* (alien-type alien)) 2))))
                (cy (round (+ (alien-y alien) (/ (elt *ALIEN-H* (alien-type alien)) 2)))))
            ;; If alien has been killed
            (if (<= (alien-life alien) 0)
                (progn
                  (fx-add nil cx cy)
                  (ccase (alien-type alien)
                    (0 (incf (data-score *DATA*) 200))
                    (1 (incf (data-score *DATA*) 150))
                    (2
                     (incf (data-score *DATA*) 800)
                     (fx-add nil (- cx 10) (- cy 4))
                     (fx-add nil (+ cx 4) (+ cy 10))
                     (fx-add nil (+ cx 8) (+ cy 8))))
                  (setf (alien-used alien) nil)
                  (return-from each-alien)))
            
            ;; Reduce time alien can shoot
            (decf (alien-shot-timer alien) 1)
            
            ;; If alien can shoot this frame
            (if (= (alien-shot-timer alien) 0)
                (progn
                 (ccase (alien-type alien)
                   (0
                    (shots-add nil nil cx cy)
                    (setf (alien-shot-timer alien) 150))
                   (1
                    (shots-add nil t cx (alien-y alien))
                    (setf (alien-shot-timer alien) 80))
                   (2
                    (shots-add nil t (- cx 5) cy)
                    (shots-add nil t (+ cx 5) cy)
                    (shots-add nil t (- cx 5) (+ cy 8))
                    (shots-add nil t (+ cx 5) (+ cy 8))
                    (setf (alien-shot-timer alien) 200)))))))))))

(defun aliens-draw ()
  (dotimes (i (config-aliens-count *CONFIG*))
	(when (alien-used (alien-i i))
	  (when (<= (alien-blink (alien-i i)) 2)
		(al:draw-bitmap (aref (sprites-alien (data-sprites *DATA*)) (alien-type (alien-i i)))
						(alien-x (alien-i i)) (alien-y (alien-i i)) 0)))))

;; --- Stars ---

(defstruct STAR
  (y 0.0 :type float)
  (speed 0.0 :type float))

(defmacro star-i (i)
  `(aref (data-stars *DATA*) ,i))

(defun stars-init ()
  (dotimes (i (config-stars-count *CONFIG*))
    (setf (star-i i) (make-star :y (between-f 0.0 (coerce (config-buffer-h *CONFIG*) 'float))
                                :speed (between-f 0.1 1.0)))))

(defun stars-update ()
  (dotimes (i (config-stars-count *CONFIG*))
	(incf (star-y (star-i i)) (star-speed (star-i i)))
	(when (>= (star-y (star-i i)) (config-buffer-h *CONFIG*))
		  (setf (star-y (star-i i)) 0.0)
		  (setf (star-speed (star-i i)) (between-f 0.1 1.0)))))

(defun stars-draw ()
  (let ((starx 1.5))
	(dotimes (i (config-stars-count *CONFIG*))
	  (let ((l (* (star-speed (star-i i)) 0.8)))
		(al:draw-pixel starx (star-y (star-i i)) (al:map-rgb-f l l l))
		(incf starx 2)))))

;; ;; --- HUD ---

(defun hud-init ()
  (setf (data-font *DATA*) (al:create-builtin-font))
  (must-init (data-font *DATA*) "font")
  (setf (data-score-display *DATA*) 0))

(defun hud-deinit ()
  (al:destroy-font (data-font *DATA*)))

(defun hud-update ()
  (when (= (mod (data-frames *DATA*) 2) 0)
	(dotimes (index 5)
	  (let ((diff (ash 1 index)))
		(if (<= (data-score-display *DATA*) (- (data-score *DATA*) diff))
			(incf (data-score-display *DATA*) diff))))))

(defun hud-draw ()
  (let ((white (al:map-rgb-f 1 1 1)))
    (al:draw-text (data-font *DATA*) white 1 1 0 (format nil "~6d" (data-score-display *DATA*)))
    (let ((spacing (1+ +LIFE-W+)))
      (dotimes (index (ship-lives (data-ship *DATA*)))
        (al:draw-bitmap (sprites-life (data-sprites *DATA*)) (+ 1 (* index spacing)) 10 0)))
    (if (= (ship-lives (data-ship *DATA*)) 0)
        (al:draw-text (data-font *DATA*)
                      white
                      (/ (config-buffer-w *CONFIG*) 2)
                      (/ (config-buffer-h *CONFIG*) 2)
    			      (foreign-enum-value 'al::align-flags :center)
                      "G A M E   O V E R"))))

;;;; Game ---------------------------------------------------------------------

(defun game-init ()
  (setf *CONFIG* (make-instance 'config))
  (setf *DATA* (make-instance 'data))
  (setf (aref *ALIEN-W* 0) +ALIEN-BUG-W+)
  (setf (aref *ALIEN-W* 1) +ALIEN-ARROW-W+)
  (setf (aref *ALIEN-W* 2) +ALIEN-LARGE-W+)
  (setf (aref *ALIEN-H* 0) +ALIEN-BUG-H+)
  (setf (aref *ALIEN-H* 1) +ALIEN-ARROW-W+)
  (setf (aref *ALIEN-H* 2) +ALIEN-LARGE-H+))

;;;; Main ---------------------------------------------------------------------

(defun main ()
  (game-init)
  (must-init (al:init) "allegro")
  (must-init (al:install-keyboard) "keyboard")
  (must-init (al:init-font-addon) "fonts")
  (must-init (al:init-ttf-addon) "ttf")
  (must-init (al:init-image-addon) "image")
  (must-init (al:init-primitives-addon) "primitives")
  (must-init (al:install-audio) "audio")
  (must-init (al:init-acodec-addon) "audio codecs")
  (let ((timer (al:create-timer (/ 1.0 60.0)))
		(queue (al:create-event-queue)))
	(must-init timer "timer")
	(must-init queue "queue")
	(disp-init)
	(audio-init)
	(sprites-init)
	(hud-init)
	(keyboard-init)
	(fx-init)
	(shots-init)
	(ship-init)
	(aliens-init)
	(stars-init)
	(must-init (al:reserve-samples 16) "reserve samples")
	(al:register-event-source queue (al:get-keyboard-event-source))
	(al:register-event-source queue (al:get-display-event-source (data-display *DATA*)))
	(al:register-event-source queue (al:get-timer-event-source timer))
	(setf (data-frames *DATA*) 0)
	(setf (data-score *DATA*) 0)
	(let ((done nil)
		  (redraw t)
		  (event (cffi:foreign-alloc '(:union al:event))))
	  (al:start-timer timer)
	  (block mainloop
		(loop
          (if (> (aref (data-key *DATA*) +KEY-B+) 0)
              (break))
		  (al:wait-for-event queue event)
		  (case (foreign-slot-value event '(:union al:event) 'al::type)
			(:timer
			 (fx-update)
			 (shots-update)
			 (stars-update)
			 (ship-update)
			 (aliens-update)
			 (hud-update)
			 (if (not (= 0 (aref (data-key *DATA*) +KEY-ESC+)))
				 (setf done t))
			 (setf redraw t)
			 (incf (data-frames *DATA*) 1))
			(:display-close (setf done t)))
		  (if done (return-from mainloop))
		  (keyboard-update event)
		  (if (and redraw (al:is-event-queue-empty queue))
			  (progn
				(disp-pre-draw)
				(al:clear-to-color (al:map-rgb 0 0 0))
				(stars-draw)
				(aliens-draw)
				(shots-draw)
				(fx-draw)
				(ship-draw)
				(hud-draw)
				(disp-post-draw)
				(setf redraw nil)))
		  ))
	  (foreign-free event))
	(sprites-deinit)
	(hud-deinit)
	(audio-deinit)
	(disp-deinit)
	(al:destroy-timer timer)
	(al:destroy-event-queue queue)))

