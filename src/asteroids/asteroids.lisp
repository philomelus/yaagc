(in-package :clg-asteroids)

(defun asset (path)
  (namestring (asdf:system-relative-pathname "clg" (concatenate 'string "assets/" "asteroids/" path))))

(deftype display-size () '(integer 0 4000))
(deftype display-float () '(double-float -4000d0 4000d0))
(deftype radiants () `(double-float 0d0 ,(* 2 (coerce pi 'double-float))))
(deftype frequency () `(integer 1 ,most-positive-fixnum))
(deftype duration () `(integer 0 ,most-positive-fixnum))
(deftype resource-size () '(integer 50 50))
(deftype resources (kind) `(simple-array ,kind (50)))
(deftype key-state () '(member :pressed :keep :released))
(deftype game-state () '(member :game :menu :game-over :highscores :start :end))
(deftype highscore () `(integer 0 ,most-positive-fixnum))
(deftype highscores () '(simple-array highscore (5)))
(deftype menu () '(simple-array string (3)))
(deftype menu-index () '(mod 3))

(defconstant +display-width+ 960)
(defconstant +display-height+ 640)

(defconstant +display-half-width+ (/ +display-width+ 2d0))
(defconstant +display-half-height+ (/ +display-height+ 2d0))

(defconstant +display-width-d0+ (coerce +display-width+ 'display-float))
(defconstant +display-height-d0+ (coerce +display-height+ 'display-float))

;;; BITMAPS

(defconstant +asteroid-bitmap-width+ 50)
(defconstant +asteroid-bitmap-height+ 50)
(defconstant +asteroid-bitmap-half-width+ (/ +asteroid-bitmap-width+ 2d0))
(defconstant +asteroid-bitmap-half-height+ (/ +asteroid-bitmap-height+ 2d0))
(defconstant +asteroid-bitmap-half-width-reciprocal+ (/ 1d0 +asteroid-bitmap-half-width+))
(defconstant +asteroid-bitmap-half-height-reciprocal+ (/ 1d0 +asteroid-bitmap-half-height+))

(defconstant +ship-bitmap-width+ 63)
(defconstant +ship-bitmap-height+ 42)
(defconstant +ship-bitmap-half-width+ (/ +ship-bitmap-width+ 2d0))
(defconstant +ship-bitmap-half-height+ (/ +ship-bitmap-height+ 2d0))

(defconstant +fire-bitmap-width+ 28)
(defconstant +fire-bitmap-height+ 27)
(defconstant +fire-bitmap-half-width+ (/ +fire-bitmap-width+ 2d0))
(defconstant +fire-bitmap-half-height+ (/ +fire-bitmap-height+ 2d0))

(defconstant +explosion-1-bitmap-width+ 59)
(defconstant +explosion-1-bitmap-height+ 60)
(defconstant +explosion-1-bitmap-half-width+ (/ +explosion-1-bitmap-width+ 2d0))
(defconstant +explosion-1-bitmap-half-height+ (/ +explosion-1-bitmap-height+ 2d0))

(defconstant +explosion-2-bitmap-width+ 59)
(defconstant +explosion-2-bitmap-height+ 59)
(defconstant +explosion-2-bitmap-half-width+ (/ +explosion-2-bitmap-width+ 2d0))
(defconstant +explosion-2-bitmap-half-height+ (/ +explosion-2-bitmap-height+ 2d0))

(defconstant +star-bitmap-width+ 7)
(defconstant +star-bitmap-height+ 7)

(defconstant +fire-bitmap-distance+ (+ +ship-bitmap-half-height+ +fire-bitmap-half-height+))

;;; FONTS

(defconstant +menu-font-start+ (* 0.25d0 +display-height-d0+))
(defconstant +menu-font-block+ (/ (* 0.5d0 +display-height-d0+) 3d0))
(defconstant +menu-font-size+ (round (* 0.7d0 +menu-font-block+)))

(defconstant +game-over-font-start+ (* 0.25d0 +display-height-d0+))
(defconstant +game-over-font-block+ (/ (* 0.5d0 +display-height-d0+) 2d0))
(defconstant +game-over-font-size+ (round (* 0.7d0 +game-over-font-block+)))

(defconstant +highscores-font-start+ (* 0.10d0 +display-height-d0+))
(defconstant +highscores-font-block+ (/ (* 0.8d0 +display-height-d0+) 5d0))
(defconstant +highscores-font-size+ (round (* 0.8d0 +highscores-font-block+)))

(defconstant +current-highscore-font-size+ (round (* 0.1d0 +display-height-d0+)))

;;; SHIP

(defconstant +ship-initial-direction+ 0d0)
(defconstant +ship-initial-dx+ 0d0)
(defconstant +ship-initial-dy+ 0d0)

(defconstant +ship-thruster-power+ 1d0)
(defconstant +ship-brakes-coefficient+ 0.96d0)
(defconstant +ship-rotation-speed+ (* 5d0 (/ (coerce pi 'double-float) 180d0)))
(defconstant +ship-half-nose-length+ (coerce +ship-bitmap-half-width+ 'display-float))
(defconstant +ship-wing-length+ +ship-bitmap-half-height+)

;;; SHOT

(defconstant +shot-speed+ 25d0)
(defconstant +shot-radius+ 3d0)
(defconstant +shot-frequency+ 3)
(defconstant +shot-duration+ 45)

;;; ASTERIOD

(defconstant +asteroid-rotation-speed+ (* 2d0 (/ (coerce pi 'double-float) 180d0)))

;;; STAR

(defconstant +star-rotation-speed+ (* 2d0 (/ (coerce pi 'double-float) 180d0)))
(defconstant +star-speed+ 0.25d0)

;;; RESOURCES COUNTS

(defconstant +stars-count+ 50)
(defconstant +asteroids-count+ 50)
(defconstant +shots-count+ 50)
(defconstant +explosions-count+ 50)

;;; EXPOLSION STAGES

(defconstant +explosion-stage-1-start+ 10)
(defconstant +explosion-stage-2-start+ 5)

(defconstant +two-pi+ (* 2 (coerce pi 'double-float)))
(defconstant +half-pi+ (/ (coerce pi 'double-float) 2))

;;; ASSETS

(defvar *font-asset* (asset "fonts/simplicity.ttf"))
(defvar *background-bitmap-asset* (asset "images/background.png"))
(defvar *sheet-bitmap-asset* (asset "images/sheet.png"))
(defvar *explosion-1-audio-asset* (asset "audio/explosion1.flac"))
(defvar *explosion-2-audio-asset* (asset "audio/explosion2.flac"))

;;; BITMAPS

(defvar *background-bitmap* (null-pointer))
(defvar *ship-bitmap* (null-pointer))
(defvar *fire-bitmap* (null-pointer))
(defvar *star-bitmap* (null-pointer))
(defvar *asteroid-bitmap* (null-pointer))
(defvar *explosion-1-bitmap* (null-pointer))
(defvar *explosion-2-bitmap* (null-pointer))

;;; FONTS

(defvar *menu-font* (null-pointer))
(defvar *game-over-font* (null-pointer))
(defvar *highscores-font* (null-pointer))
(defvar *current-highscore-font* (null-pointer))

;;; COLORS

(defvar *main-color* (al:map-rgb-f 0.344 0.188 0.509))

;;; SAMPLES

(defvar *explosion-1-sample* (null-pointer))
(defvar *explosion-2-sample* (null-pointer))

;; GAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONTROLS

(defvar *key-up* :released)
(defvar *key-down* :released)
(defvar *key-left* :released)
(defvar *key-right* :released)
(defvar *key-enter* :released)
(defvar *key-escape* :released)

;;; SHIP

(defvar *ship-x* 0d0)
(defvar *ship-y* 0d0)
(defvar *ship-dx* 0d0)
(defvar *ship-dy* 0d0)
(defvar *ship-direction* 0d0)
(defvar *ship-nose-x* 0d0)
(defvar *ship-nose-y* 0d0)
(defvar *ship-right-wing-x* 0d0)
(defvar *ship-right-wing-y* 0d0)
(defvar *ship-left-wing-x* 0d0)
(defvar *ship-left-wing-y* 0d0)

(defvar *frames* 0)
(defvar *state* :start)
(defvar *menu* #("Game" "Highscores" "Exit"))
(defvar *menu-index* 0)
(defvar *current-highscore* 0)
(defvar *highscores* (make-array 5 :element-type 'highscore))

;; RENDERING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass system (al:system)
  ()
  (:default-initargs
   :title "Asteroids"
   :width +display-width+ :height +display-height+
   :display-options '((:sample-buffers 1 :suggest) (:samples 8 :suggest))))


(defstruct (point (:conc-name))
  (x 0d0 :type display-float)
  (y 0d0 :type display-float))

(defstruct (moving-point (:include point) (:conc-name))
  (dx 0d0 :type display-float)
  (dy 0d0 :type display-float))

(defstruct (moving-point-resource (:include moving-point) (:conc-name))
  (usedp nil :type boolean))

(defstruct (moving-circular-resource (:include moving-point-resource) (:conc-name))
  (radius 0d0 :type display-float))

(defstruct (asteroid (:include moving-circular-resource) (:conc-name))
  (radius-squared 0d0 :type display-float)
  (asteroid-direction 0d0 :type radiants)
  (x-scale 0d0 :type display-float)
  (y-scale 0d0 :type display-float))

(defstruct (shot (:include moving-circular-resource) (:conc-name))
  (duration 0 :type duration))

(defstruct (star (:include moving-point) (:conc-name))
  (star-direction 0d0 :type radiants))

(defstruct (explosion (:include point) (:conc-name))
  (explosion-usedp nil :type boolean)
  (explosion-duration 0 :type duration))

(defstruct (asteroid-initials (:conc-name asteroid-))
  (speed 0d0 :type display-float)
  (radius 0d0 :type display-float)
  (frequency 1 :type frequency))

;;; ASTEROID

(defvar *asteroid-initials*
  `#(,(make-asteroid-initials :speed 6d0 :radius 15d0 :frequency 60)
     ,(make-asteroid-initials :speed 3d0 :radius 30d0 :frequency 150)
     ,(make-asteroid-initials :speed 1d0 :radius 45d0 :frequency 375)))

;;; RESOURCES

(defvar *asteroids*
  (loop :with asteroids :of-type (resources asteroid)
          = (make-array +asteroids-count+
                        :element-type 'asteroid
                        :initial-element (make-asteroid))
        :for i fixnum :from 0 :below +asteroids-count+ :do
          (setf (aref asteroids i) (make-asteroid))
        :finally (return asteroids)))

(defvar *shots*
  (loop :with shots :of-type (resources shot)
          = (make-array +shots-count+
                        :element-type 'shot
                        :initial-element (make-shot))
        :for i fixnum :from 0 :below +shots-count+ :do
          (setf (aref shots i) (make-shot))
        :finally (return shots)))

(defvar *stars*
  (loop :with stars :of-type (resources star)
          = (make-array +stars-count+
                        :element-type 'star
                        :initial-element (make-star))
        :for i fixnum :from 0 :below +stars-count+ :do
          (setf (aref stars i)
                (let ((direction (random +two-pi+)))
                  (declare (type radiants direction))
                  (make-star
                   :x (random +display-width-d0+)
                   :y (random +display-height-d0+)
                   :dx (* +star-speed+ (cos direction))
                   :dy (* +star-speed+ (sin direction))
                   :star-direction direction)))
        :finally (return stars)))

(defvar *explosions*
  (loop :with explosions :of-type (resources explosion)
          = (make-array +explosions-count+
                        :element-type 'explosion
                        :initial-element (make-explosion))
        :for i fixnum :from 0 :below +explosions-count+ :do
          (setf (aref explosions i) (make-explosion))
        :finally (return explosions)))

;;;;===========================================================================

(define-modify-macro mulf (multiplicand) *)

(defmacro modincf (place increment modulo)
  (multiple-value-bind (temp-vars temp-vals vars set-form get-form)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temp-vars temp-vals)
            (,(first vars) (mod (+ ,get-form ,increment) ,modulo)))
       ,set-form)))

(defun add-explosion (x y)
  (al:play-sample
   (ecase (random 2)
     (0 *explosion-1-sample*)
     (1 *explosion-2-sample*))
   0.3 0 1.0 :once (null-pointer))

  (loop :for explosion :of-type explosion :across *explosions*
        :unless (explosion-usedp explosion) :do
          (setf (x explosion) x
                (y explosion) y
                (explosion-usedp explosion) t
                (explosion-duration explosion) +explosion-stage-1-start+)

          (return-from add-explosion))
  nil)

(defun add-shot ()
  (when (zerop (mod *frames* +shot-frequency+))
    (loop :for shot :of-type shot :across *shots*
          :unless (usedp shot) :do
            (setf (x shot) *ship-nose-x*
                  (y shot) *ship-nose-y*
                  (dx shot) (* +shot-speed+ (cos *ship-direction*))
                  (dy shot) (* +shot-speed+ (sin *ship-direction*))
                  (radius shot) +shot-radius+
                  (duration shot) +shot-duration+
                  (usedp shot) t)
            (return-from add-shot nil)))
  nil)

(defun add-asteroid (initials)
  (when (zerop (mod *frames* (asteroid-frequency initials)))
    (loop :for asteroid :of-type asteroid :across *asteroids*
          :unless (usedp asteroid) :do
            (let ((direction (random +two-pi+))
                  (speed (asteroid-speed initials))
                  (radius (asteroid-radius initials)))
              (declare (type radiants direction) (type display-float speed radius))
              (setf (dx asteroid) (* speed (cos direction))
                    (dy asteroid) (* speed (sin direction))
                    (radius asteroid) radius
                    (radius-squared asteroid) (expt radius 2)
                    (usedp asteroid) t
                    (x-scale asteroid) (* radius +asteroid-bitmap-half-width-reciprocal+)
                    (y-scale asteroid) (* radius +asteroid-bitmap-half-height-reciprocal+))
              (case (random 2)
                (0 (setf (x asteroid) (random +display-width-d0+)
                         (y asteroid) 0d0))
                (1 (setf (x asteroid) 0d0
                         (y asteroid) (random +display-height-d0+))))
              (return-from add-asteroid nil))))
  nil)

(defun collide-ship-asteroid-p (asteroid)
  (let ((radius-squared (radius-squared asteroid))
        (x (x asteroid)) (y (y asteroid))
        (c1x 0d0) (c1y 0d0) (c2x 0d0) (c2y 0d0) (c3x 0d0) (c3y 0d0)
        (e1x 0d0) (e1y 0d0) (e2x 0d0) (e2y 0d0) (e3x 0d0) (e3y 0d0)
        (c1-squared 0d0) (c2-squared 0d0) (c3-squared 0d0))
    (declare
     (dynamic-extent c1x c1y c2x c2y c3x c3y e1x e1y e2x e2y e3x e3y c1-squared c2-squared c3-squared)
     (type double-float c1x c1y c2x c2y c3x c3y e1x e1y e2x e2y e3x e3y c1-squared c2-squared c3-squared)
     (type double-float radius-squared x y))

    (setf c1x (- x *ship-nose-x*) c1y (- y *ship-nose-y*)
          c1-squared (- (+ (expt c1x 2) (expt c1y 2)) radius-squared))
    (when (<= c1-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf c2x (- x *ship-right-wing-x*) c2y (- y *ship-right-wing-y*)
          c2-squared (- (+ (expt c2x 2) (expt c2y 2)) radius-squared))
    (when (<= c2-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf c3x (- x *ship-left-wing-x*) c3y (- y *ship-left-wing-y*)
          c3-squared (- (+ (expt c3x 2) (expt c3y 2)) radius-squared))
    (when (<= c3-squared 0)
      (return-from collide-ship-asteroid-p t))

    (setf e1x (- *ship-right-wing-x* *ship-nose-x*) e1y (- *ship-right-wing-y* *ship-nose-y*)
          e2x (- *ship-left-wing-x* *ship-right-wing-x*) e2y (- *ship-left-wing-y* *ship-right-wing-y*)
          e3x (- *ship-nose-x* *ship-left-wing-x*) e3y (- *ship-nose-y* *ship-left-wing-y*))

    (when (and (<= (* e1y c1x) (* e1x c1y))
               (<= (* e2y c2x) (* e2x c2y))
               (<= (* e3y c3x) (* e3x c3y)))
      (return-from collide-ship-asteroid-p t))

    (let ((k (+ (* c1x e1x) (* c1y e1y))))
      (declare (type double-float k))
      (when (< 0 k)
        (let ((len (+ (expt e1x 2) (expt e1y 2))))
          (declare (type double-float len))
          (when (and (< k len) (<= (* c1-squared len) (expt k 2)))
            (return-from collide-ship-asteroid-p t)))))

    (let ((k (+ (* c2x e2x) (* c2y e2y))))
      (declare (type double-float k))
      (when (< 0 k)
        (let ((len (+ (expt e2x 2) (expt e2y 2))))
          (declare (type double-float len))
          (when (and (< k len) (<= (* c2-squared len) (expt k 2)))
            (return-from collide-ship-asteroid-p t)))))

    (let ((k (+ (* c3x e3x) (* c3y e3y))))
      (declare (type double-float k))
      (when (< 0 k)
        (let ((len (+ (expt e3x 2) (expt e3y 2))))
          (declare (type double-float len))
          (when (and (< k len) (<= (* c3-squared len) (expt k 2)))
            (return-from collide-ship-asteroid-p t)))))

    nil))

(defun collide-shot-asteroid-p (shot asteroid)
  (<= (+ (expt (- (x asteroid) (x shot)) 2)
         (expt (- (y asteroid) (y shot)) 2))
      (expt (+ (radius asteroid) (radius shot)) 2)))

(defun init-ship ()
  (setf *ship-x* +display-half-width+
        *ship-y* +display-half-height+
        *ship-dx* +ship-initial-dx+
        *ship-dy* +ship-initial-dy+
        *ship-direction* +ship-initial-direction+)
  nil)

(defun init-asteroids ()
  (loop :for asteroid :of-type asteroid :across *asteroids* :do
    (setf (usedp asteroid) nil))
  nil)

(defun init-shots ()
  (loop :for shot :of-type shot :across *shots* :do
    (setf (usedp shot) nil))
  nil)

(defun init-game ()
  (init-ship)
  (init-asteroids)
  (init-shots)
  (setf *current-highscore* 0)
  nil)

(defun render-explosions ()
  (loop :for explosion :of-type explosion :across *explosions*
        :when (explosion-usedp explosion) :do
          (cond ((<= 0 (explosion-duration explosion) +explosion-stage-2-start+)
                 (al:draw-bitmap
                  *explosion-2-bitmap*
                  (- (x explosion) +explosion-2-bitmap-half-width+)
                  (- (y explosion) +explosion-2-bitmap-half-height+)
                  nil))
                ((<= +explosion-stage-2-start+ (explosion-duration explosion) +explosion-stage-1-start+)
                 (al:draw-bitmap
                  *explosion-1-bitmap*
                  (- (x explosion) +explosion-1-bitmap-half-width+)
                  (- (y explosion) +explosion-1-bitmap-half-height+)
                  nil))))
  nil)

(defun render-stars ()
  (loop :for star :of-type star :across *stars* :do
    (al:draw-rotated-bitmap
     *star-bitmap*
     0 0 (x star) (y star)
     (star-direction star)
     nil))
  nil)

(defun render-ship ()
  (al:draw-rotated-bitmap
   *ship-bitmap*
   +ship-bitmap-half-width+ +ship-bitmap-half-height+
   *ship-x* *ship-y*
   *ship-direction*
   nil)

  (unless (and (eq :released *key-up*) (eq :released *key-down*)
               (eq :released *key-left*) (eq :released *key-right*))
    (al:draw-rotated-bitmap
     *fire-bitmap*
     +fire-bitmap-half-width+ +fire-bitmap-half-height+
     (- *ship-x* (* +fire-bitmap-distance+ (cos *ship-direction*)))
     (- *ship-y* (* +fire-bitmap-distance+ (sin *ship-direction*)))
     *ship-direction*
     nil))
  nil)

(defun render-asteroids ()
  (loop :for asteroid :of-type asteroid :across *asteroids*
        :when (usedp asteroid) :do
          (al:draw-scaled-rotated-bitmap
           *asteroid-bitmap*
           +asteroid-bitmap-half-width+
           +asteroid-bitmap-half-height+
           (x asteroid) (y asteroid)
           (x-scale asteroid)
           (y-scale asteroid)
           (asteroid-direction asteroid)
           nil))
  nil)

(defun render-shots ()
  (loop :for shot :of-type shot :across *shots*
        :when (usedp shot) :do
          (al:draw-filled-circle
           (x shot) (y shot) (radius shot)
           *main-color*))
  nil)

(defun render-game ()
  (al:draw-text *current-highscore-font* *main-color* 1 1
                (foreign-enum-value 'al::align-flags :left)
                (write-to-string *current-highscore*))
  (render-ship)
  (render-shots)
  nil)

(defun render-menu ()
  (loop :for entry :of-type string :across *menu*
        :for i :of-type menu-index :from 0
        :for y :of-type display-float = +menu-font-start+ :then (+ y +menu-font-block+)
        :do (al:draw-text *menu-font* *main-color*
                          +display-half-width+ y
                          (foreign-enum-value 'al::align-flags :center)
                          (if (= i *menu-index*)
                              (concatenate 'string "< " entry " >")
                              entry))))

(defun render-game-over ()
  (al:draw-text *game-over-font* *main-color*
                +display-half-width+ +game-over-font-start+
                (foreign-enum-value 'al::align-flags :center)
                "G A M E  O V E R")
  (al:draw-text *game-over-font* *main-color*
                +display-half-width+ (+ +game-over-font-start+ +game-over-font-block+)
                (foreign-enum-value 'al::align-flags :center)
                (concatenate 'string "Score: " (write-to-string *current-highscore*))))

(defun render-highscores ()
  (loop :for highscore :of-type highscore :across *highscores*
        :for y :of-type display-float = +highscores-font-start+ :then (+ y +highscores-font-block+) :do
          (al:draw-text *highscores-font* *main-color*
                        +display-half-width+ y
                        (foreign-enum-value 'al::align-flags :center)
                        (write-to-string highscore))))

(defun update-explosions ()
  (loop :for explosion :of-type explosion :across *explosions*
        :when (explosion-usedp explosion) :do
          (if (plusp (explosion-duration explosion))
              (decf (explosion-duration explosion))
              (setf (explosion-usedp explosion) nil)))
  nil)

(defun update-stars ()
  (loop :for star :of-type star :across *stars* :do
    (modincf (x star) (dx star) +display-width-d0+)
    (modincf (y star) (dy star) +display-height-d0+)
    (modincf (star-direction star) +star-rotation-speed+ +two-pi+))
  nil)

(defun update-asteroids ()
  (loop :for asteroid :of-type asteroid :across *asteroids*
        :when (usedp asteroid) :do
          (modincf (x asteroid) (dx asteroid) +display-width-d0+)
          (modincf (y asteroid) (dy asteroid) +display-height-d0+)
          (modincf (asteroid-direction asteroid) +asteroid-rotation-speed+ +two-pi+))
  nil)

(defun update-shots ()
  (loop :for shot :of-type shot :across *shots*
        :when (usedp shot)
          :if (plusp (decf (duration shot)))
            :do (modincf (x shot) (dx shot) +display-width-d0+)
                (modincf (y shot) (dy shot) +display-height-d0+)
        :else :do (setf (usedp shot) nil))
  nil)

(defun update-ship ()
  (mulf *ship-dx* +ship-brakes-coefficient+)
  (mulf *ship-dy* +ship-brakes-coefficient+)

  (unless (eq :released *key-right*)
    (modincf *ship-direction* +ship-rotation-speed+ +two-pi+))

  (unless (eq :released *key-left*)
    (modincf *ship-direction* (- +ship-rotation-speed+) +two-pi+))

  (let ((direction-x (cos *ship-direction*))
        (direction-y (sin *ship-direction*)))
    (declare (type (double-float -1d0 1d0) direction-x direction-y))

    (unless (eq :released *key-up*)
      (incf *ship-dx* (* +ship-thruster-power+ direction-x))
      (incf *ship-dy* (* +ship-thruster-power+ direction-y)))

    (unless (eq :released *key-down*)
      (decf *ship-dx* (* +ship-thruster-power+ direction-x))
      (decf *ship-dy* (* +ship-thruster-power+ direction-y)))

    (modincf *ship-x* *ship-dx* +display-width-d0+)
    (modincf *ship-y* *ship-dy* +display-height-d0+)

    (let* ((nose-dx (* +ship-half-nose-length+ direction-x))
           (nose-dy (* +ship-half-nose-length+ direction-y))
           (back-x (- *ship-x* nose-dx))
           (back-y (- *ship-y* nose-dy))
           (right-wing-direction (mod (- *ship-direction* +half-pi+) +two-pi+))
           (right-wing-dx (* +ship-wing-length+ (cos right-wing-direction)))
           (right-wing-dy (* +ship-wing-length+ (sin right-wing-direction))))
      (declare
       (type radiants right-wing-direction)
       (type display-float back-x back-y nose-dx nose-dy right-wing-dx right-wing-dy))

      (setf *ship-nose-x* (+ *ship-x* nose-dx)
            *ship-nose-y* (+ *ship-y* nose-dy)
            *ship-right-wing-x* (+ back-x right-wing-dx)
            *ship-right-wing-y* (+ back-y right-wing-dy)
            *ship-left-wing-x* (- back-x right-wing-dx)
            *ship-left-wing-y* (- back-y right-wing-dy))))
  nil)

(defun update-controls ()
  (when (eq :pressed *key-up*)
    (setf *key-up* :keep))
  (when (eq :pressed *key-down*)
    (setf *key-down* :keep))
  (when (eq :pressed *key-left*)
    (setf *key-left* :keep))
  (when (eq :pressed *key-right*)
    (setf *key-right* :keep))
  (when (eq :pressed *key-enter*)
    (setf *key-enter* :keep))
  (when (eq :pressed *key-escape*)
    (setf *key-escape* :keep))
  nil)

(defun update-game ()
  (if (eq :pressed *key-escape*)
      (setf *state* :game-over)
      (progn
        (update-ship)
        (update-shots)

        (add-shot)
        (loop :for asteroid-initials :of-type asteroid-initials :across *asteroid-initials* :do
          (add-asteroid asteroid-initials))

        (loop :for asteroid :of-type asteroid :across *asteroids*
              :when (usedp asteroid) :do
                (loop :for shot :of-type shot :across *shots*
                      :when (and (usedp shot) (collide-shot-asteroid-p shot asteroid)) :do
                        (setf (usedp shot) nil (usedp asteroid) nil)
                        (incf *current-highscore* (round (radius asteroid)))
                        (add-explosion (x asteroid) (y asteroid)))
              :when (and (usedp asteroid) (collide-ship-asteroid-p asteroid)) :do
                (setf *state* :game-over)
                (add-explosion *ship-x* *ship-y*)
                (loop-finish))

        (incf *frames*)))
  nil)

(defun update-menu ()
  (cond ((eq :pressed *key-up*)
         (modincf *menu-index* -1 (length *menu*)))
        ((eq :pressed *key-down*)
         (modincf *menu-index* 1 (length *menu*)))
        ((eq :pressed *key-enter*)
         (setf *state* (ecase *menu-index*
                         (0 (init-game) :game)
                         (1 :highscores)
                         (2 :end)))))
  nil)

(defun sorted-insert (element array)
  (let ((position (position element array :test #'>)))
    (declare (type (or null fixnum) position))
    (when position
      (setf (subseq array (1+ position)) (subseq array position)
            (aref array position) element)))
  nil)

(defun update-game-over ()
  (when (or (eq :pressed *key-enter*) (eq :pressed *key-escape*))
    (sorted-insert *current-highscore* *highscores*)
    (setf *state* :menu))
  nil)

(defun update-highscores ()
  (when (or (eq :pressed *key-enter*) (eq :pressed *key-escape*))
    (setf *state* :menu))
  nil)

(defmacro with-pointer ((var expression cleanup) &body body)
  `(let ((,var ,expression))
     (declare (type foreign-pointer ,var))
     (when (null-pointer-p ,var)
       (error ,(format nil "Error initializing pointer: (~A ~A ~A)" var expression cleanup)))
     (unwind-protect ,(if (cdr body)
                          (cons 'progn body)
                          (car body))
       (,cleanup ,var))))

(defmacro with-bitmap ((var expression) &body body)
  `(with-pointer (,var ,expression al:destroy-bitmap) ,@body))

(defmacro with-font ((var expression) &body body)
  `(with-pointer (,var ,expression al:destroy-font) ,@body))

(defmacro with-sample ((var expression) &body body)
  `(with-pointer (,var ,expression al:destroy-sample) ,@body))

(defmacro with-pointers (((kind variable expression) &rest binds) &body body)
  `(,(ecase kind (:bitmap 'with-bitmap) (:font 'with-font) (:sample 'with-sample))
    (,variable ,expression)
    ,@(if binds
          (list `(with-pointers ,binds ,@body))
          body)))

(defmethod al:system-loop :around ((sys system))
  (al:reserve-samples 128)
  (with-pointers
      ((:font *menu-font* (al:load-ttf-font *font-asset* +menu-font-size+ 0))
       (:font *game-over-font* (al:load-ttf-font *font-asset* +game-over-font-size+ 0))
       (:font *highscores-font* (al:load-ttf-font *font-asset* +highscores-font-size+ 0))
       (:font *current-highscore-font* (al:load-ttf-font *font-asset* +current-highscore-font-size+ 0))
       (:bitmap *background-bitmap* (al:load-bitmap *background-bitmap-asset*))
       (:bitmap sheet-bitmap* (al:load-bitmap *sheet-bitmap-asset*))
       (:bitmap *asteroid-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 15 78 +asteroid-bitmap-width+ +asteroid-bitmap-height+))
       (:bitmap *ship-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 0 4 +ship-bitmap-width+ +ship-bitmap-height+))
       (:bitmap *fire-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 11 48 +fire-bitmap-width+ +fire-bitmap-height+))
       (:bitmap *star-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 47 58 +star-bitmap-width+ +star-bitmap-height+))
       (:bitmap *explosion-1-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 64 0 +explosion-1-bitmap-width+ +explosion-1-bitmap-height+))
       (:bitmap *explosion-2-bitmap*
                (al:create-sub-bitmap sheet-bitmap* 64 60 +explosion-2-bitmap-width+ +explosion-2-bitmap-height+))
       (:sample *explosion-1-sample* (al:load-sample *explosion-1-audio-asset*))
       (:sample *explosion-2-sample* (al:load-sample *explosion-2-audio-asset*)))
    (call-next-method)))

(defmethod al:key-down-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (unless (eq :keep *key-up*) (setf *key-up* :pressed)))
    (:down (unless (eq :keep *key-down*) (setf *key-down* :pressed)))
    (:left (unless (eq :keep *key-left*) (setf *key-left* :pressed)))
    (:right (unless (eq :keep *key-right*) (setf *key-right* :pressed)))
    (:enter (unless (eq :keep *key-enter*) (setf *key-enter* :pressed)))
    (:escape (unless (eq :keep *key-escape*) (setf *key-escape* :pressed)))))

(defmethod al:key-up-handler ((sys system))
  (case (foreign-slot-value (al:event sys) '(:struct al:keyboard-event) 'al::keycode)
    (:up (setf *key-up* :released))
    (:down (setf *key-down* :released))
    (:left (setf *key-left* :released))
    (:right (setf *key-right* :released))
    (:enter (setf *key-enter* :released))
    (:escape (setf *key-escape* :released))))

(defmethod al:update ((sys system))
  (update-stars)
  (update-explosions)
  (update-asteroids)
  (ecase *state*
    (:game (update-game))
    (:menu (update-menu))
    (:game-over (update-game-over))
    (:highscores (update-highscores))
    (:start (setf *state* :menu))
    (:end (setf *state* :start (al:system-loop-running-p sys) nil)))
  (update-controls))

(defmethod al:render ((sys system))
  (al:draw-bitmap *background-bitmap* 0 0 0)
  (render-stars)
  (render-explosions)
  (render-asteroids)
  (ecase *state*
    (:game (render-game))
    (:menu (render-menu))
    (:game-over (render-game-over))
    (:highscores (render-highscores))
    ((:start :end)))
  (al:flip-display))

(defun main ()
  (al:run-system (make-instance 'system))
  nil)

