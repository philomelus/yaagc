(in-package :yaagc)

;;;; CONSTANTS ----------------------------------------------------------------

(defconstant +KEY-MAX+ (foreign-enum-value 'al::keycodes :key-max))

(defconstant +KEY-DOWN+ (foreign-enum-value 'al::keycodes :down))
(defconstant +KEY-LEFT+ (foreign-enum-value 'al::keycodes :left))
(defconstant +KEY-RIGHT+ (foreign-enum-value 'al::keycodes :right))
(defconstant +KEY-UP+ (foreign-enum-value 'al::keycodes  :up))
(defconstant +KEY-X+ (foreign-enum-value 'al::keycodes :x))
(defconstant +KEY-ESC+ (foreign-enum-value 'al::keycodes  :escape))
(defconstant +KEY-B+ (foreign-enum-value 'al::keycodes :b))

;; (defmacro must-init (test desc)
;;   (typecase test
;;     (foreign-pointer
;;      `(when (null-pointer-p ,test)
;;         (if (> (al:get-errno) 0)
;;             (error "Failed to initialize ~s (~d)" ,desc (al:get-errno))
;;             (error "Failed to initialize ~s" ,desc))))
;;     (boolean
;;      `(unless ,test
;;         (if (> (al:get-errno) 0)
;;             (error "Failed to initialize ~s (~d)" ,desc (al:get-errno))
;;             (error "Failed to initialize ~s" ,desc))))
;;     (cons) <<<--- Don't know how to deal with this ... (turns out ... its a list ...)
;;     (otherwise
;;      (error "MUST-INIT unknown type: ~a (~a)" (type-of test) test))))

(defun color2assoc (color)
  "Return r, g, b, and a of allegro color as association list.

(color2assoc (map-rgba-f 0.2 0.4 0.6 0.8)
returns
((:r 0.2) (:g 0.4) (:b 0.6) (:a 0.8))"
  (let ((rgba (color2list color)))
    `((:r ,(first rgba)) (:g ,(second rgba)) (:b ,(third rgba)) (:a ,(fourth rgba)))))

(defun color2list (color)
  "Returns r, g, b, and a of allegro color as list.

(color2list (map-rgba-f 0.2 0.4 0.6 0.8)
returns
(0.2 0.4 0.6 0.8)"
  (list
   (nth (1+ (position 'al::r color)) color)
   (nth (1+ (position 'al::g color)) color)
   (nth (1+ (position 'al::b color)) color)
   (nth (1+ (position 'al::a color)) color)))

(defun color-a (color)
  "Returns a of allegro color."
  (fourth (color2list color)))

(defun color-b (color)
  "Returns b of allegro color."
  (third (color2list color)))

(defun color-g (color)
  "Returns g of allegro color."
  (second (color2list color)))

(defun color-inverse (color)
  "Returns simple inverse of color."
  (al:map-rgb-f (- 1 (color-r color))
                (- 1 (color-g color))
                (- 1 (color-b color))))

(defun color-r (color)
  "Returns r of allegro color."
  (first (color2list color)))

(defgeneric must-init (test desc))

(defmethod must-init ((test sb-sys::system-area-pointer) desc)
  (when (null-pointer-p test)
    (if (>  (al:get-errno) 0)
        (error "Couldn't initialize ~s (~d)." desc (al:get-errno))
        (error "Couldn't initialize ~s." desc))))

(defmethod must-init ((test t) desc)
  (unless test
    (if (>  (al:get-errno) 0)
        (error "Couldn't initialize ~s (~d)." desc (al:get-errno))
        (error "Couldn't initialize ~s." desc))))

(declaim (ftype (function (integer integer) integer) between))
(defun between (lo hi)
  (let* ((r (+ lo (* (random 1.0) (- hi lo))))
		 (result (truncate r)))
	result))

(declaim (ftype (function (float float) float) between-f))
(defun between-f (lo hi)
  (+ lo (* (random 1.0) (- hi lo))))

(declaim (ftype (function (integer integer integer integer integer integer integer integer) boolean) rect-collide))
(defun rect-collide (left1 top1 right1 bottom1 left2 top2 right2 bottom2)
  (cond ((> left1 right2) (return-from rect-collide nil))
		((< right1 left2) (return-from rect-collide nil))
		((> top1 bottom2) (return-from rect-collide nil))
		((< bottom1 top2) (return-from rect-collide nil)))
  t)

