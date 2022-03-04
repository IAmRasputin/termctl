(in-package :termctl)

(defvar *server-started* nil)

(defvar *console-io*)
(defvar *console*)

(defparameter *message-french* "Debout, les damnés de la terre!")
(defparameter *message-english* "Arise, ye prisoners of starvation!")

(cffi:defcfun (enable-raw "enable_raw")
    :pointer)
(cffi:defcfun (disable-raw "disable_raw")
    :void
  (handler :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *key-resolvers* (make-hash-table))
  (defconstant +delete+ (code-char #x7f)
    "The DEL character (#\Rubout), last in the ASCII table")
  (defconstant +escape+ (code-char #x1b)
    "The ESC character (#\esc).")
  (defconstant  +c1-mod+   16)
  (defconstant  +meta-mod+  8)
  (defconstant  +ctrl-mod+  4)
  (defparameter +alt-mod+   2)
  (defconstant  +alt-mod*+  2)
  (defconstant  +shift-mod+ 1))

(defclass console ()
  ((ios :initarg :ios :accessor ios :documentation "The terminal's I/O stream")
   (fgc :initarg :fgc :accessor fgc :documentation "Foreground color")
   (bgc :initarg :bgc :accessor bgc :documentation "Background color")
   (pos :initarg :pos :accessor pos :documentation "Cursor position")
   (cvp :initarg :cvp :accessor cvp :documentation "Cursor visibility")
   (fps :initarg :fps :accessor fps :documentation "Desired framerate")
   (app :initarg :app :accessor app :documentation "Application state")
   (hnd               :accessor hnd :documentation "Terminal handler"))
  (:default-initargs
   :ios (error "Must specify I/O stream")
   :fgc '(#xff #xa0 #xa0)
   :bgc '(#x00 #x22 #x22)
   :pos '(1 . 1)
   :cvp nil
   :fps 60
   :app nil))

(defmethod initialize-instance :after ((instance console) &key ios fgc bgc cvp)
  (setf (hnd instance) (init-console))
  (apply #'set-foreground-color fgc)
  (apply #'set-background-color bgc)
  (setf (cursor-visibility) cvp))



(defclass gesture ()
  ((key  :initarg :key  :accessor gesture-key)
   (mods :initarg :mods :accessor gesture-mods)))

(defmethod print-object ((o gesture) s)
  (print-unreadable-object (o s :type nil :identity nil)
    (let ((key (gesture-key o))
          (mods (gesture-mods o)))
      (format s "~s ~s"
              key
              (loop for p in (list +c1-mod+
                                   +meta-mod+
                                   +ctrl-mod+
                                   +alt-mod+
                                   +shift-mod+)
                    for k in '(:C1 :Meta :Ctrl :Alt :Shift)
                    unless (zerop (logand mods p))
                      collect k)))))

;; Input
(defmacro define-key-resolver (group terminator (num1 num2) &body body)
  `(setf (gethash ,(+ (char-code terminator)
                      (ash (char-code group) 8))
                  (progn *key-resolvers*))
         (lambda (,num1 ,num2)
           (declare (ignorable ,num1 ,num2))
           ,@body)))

(defun maybe-combo (key num2)
  (if-let ((ctrl (and (characterp key) (controlp key))))
    (prog1 ctrl
      (setf (getsure-mods ctrl) (logior (1- num2) +ctrl-mod+)))
    (or (and (= num2 1) key)
        (make-instance 'getsure :key key :mods (1- num2)))))

(define-key-resolver #\[ #\~ (num1 num2)
  (let ((key (case num1
               (1 :home) (2  :insert) (3    :delete)
               (4  :end) (5 :page-up) (6 :page-down)
               (11  :f1) (12 :f2)  (13  :f3) (14  :f4) ; deprecated
               (15  :f5) (17 :f6)  (18  :f7) (19  :f8)
               (20  :f9) (21 :f10) (23 :f11) (24 :f12)
               (25 :f13) (26 :f14) (28 :f15) (29 :f16)
               (31 :f17) (32 :f18) (33 :f19) (34 :f20))))
    (maybe-combo key num2)))

(define-key-resolver #\[ #\A (num1 num2) (maybe-combo :key-up    num2))
(define-key-resolver #\[ #\B (num1 num2) (maybe-combo :key-down  num2))
(define-key-resolver #\[ #\C (num1 num2) (maybe-combo :key-right num2))
(define-key-resolver #\[ #\D (num1 num2) (maybe-combo :key-left  num2))

(define-key-resolver #\O #\P (num1 num2) (maybe-combo :f1 num2))
(define-key-resolver #\O #\Q (num1 num2) (maybe-combo :f2 num2))
(define-key-resolver #\O #\R (num1 num2) (maybe-combo :f3 num2))
(define-key-resolver #\O #\S (num1 num2) (maybe-combo :f4 num2))

(defun resolve-key (group num1 num2 |Hasta la vista, baby|)
  (if (null |Hasta la vista, baby|)
      ;; When there is no terminating character, then it is probably a
      ;; result of pressing ALT+<char>. This is ambiguous, i.e ALT+[
      ;; generates CSI. We try to be as robust as we can here.
      (maybe-combo (case group
                     (#.+escape+ :escape)
                     (#.+delete+ :delete)
                     (t group))
                   (1+ +alt-mod+))
      (funcall (gethash (+ (char-code |Hasta la vista, baby|)
                           (ash (char-code group) 8))
                        *key-resolvers*
                        #'(lambda (num1 num2)
                            (let ((k (format nil
                                             "Unknown sequence: ESC ~c ~d ~d ~c"
                                             group num1 num2
                                             |Hasta la vista, baby|)))
                              (make-instance 'gesture :key k :mods 0))))
               num1 num2)))

(defun control-char-p (ch &aux (code (char-code ch)))
  (or (<= 0 code 31)
      (<= 128 code 159)))

(defun controlp (ch &aux (code (char-code ch)))
  "Predicate determining if the character is a control character.
Returns a generalized boolean (when true returns a gesture)."
  (cond ((<= 0 code 31)
         (make-instance 'gesture
                        :mods +ctrl-mod+
                        :key (code-char (+ code 64))))
        ((<= 128 code 159)
         (make-instance 'gesture
                        :mods +c1-mod+
                        :key (code-char (- code 64))))))

(defun deletep (ch)
  (when (char= ch +delete+)
    :delete))

(defun parse-escape-sequence ()
  (let ((char (read-char-no-hang *console-io*))
        (num1 1)
        (num2 1))
    (labels ((read-num ()
               (loop while (and char (digit-char-p char))
                     collecting char into num
                     do (setf char (read-char-no-hang *console-io*))
                     finally (when num
                               (return (parse-integer (coerce num 'string)))))))
      (setf num1 (or (read-num) 1))
      (when (null char)
        (return-from parse-escape-sequence (values num1 num2 char)))
      (when (char= char #\;)
        (setf char (read-char-no-hang *console-io*)
              num2 (or (read-num) 1)))
      (values num1 num2 char))))

(defun escapep (ch)
  (unless (char= ch +escape+)
    (return-from escapep nil))
  (if-let ((next-ch (read-char-no-hang *console-io*)))
    ;; escape sequence grammar: [\[NO](<num>)(;<num>)[~A-Z].
    (multiple-value-bind (num1 num2 terminator)
        (parse-escape-sequence)
      (resolve-key next-ch num1 num2 terminator))
    :escape))

(defun put (&rest args)
  "Put a raw string on the console."
  (format *console-io* "~{~a~}" args)
  (finish-output *console-io*))

(defun esc (&rest args)
  "Escape sequence"
  (apply #'put (code-char #x1b) args))

(defun csi (&rest args)
  "Control sequence introducer"
  (apply #'esc #\[ args))

(defun sgr (&rest args)
  "Select graphic rendition"
  (apply #'csi (append args '("m"))))

(defun reset-console ()
  "clear screen, attrs, cursor position, etc"
  (esc "c"))

(defun clear-console (&optional (mode 2))
  "Erase in display"
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the display
  ;; 1 - clear from cursor to the start of the display
  ;; 2 - clear entire display
  (csi mode "J"))

(defun clear-line (&optional (mode 2))
  "Erase in line."
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the line
  ;; 1 - clear from cursor to the start of the line
  ;; 2 - clear entire line
  (csi mode "K"))

(defun set-foreground-color (r g b)
  (sgr "38;2;" r ";" g ";" b))

(defun set-background-color (r g b)
  (sgr "48;2;" r ";" g ";" b))

(defun save-cursor-position ()
  (csi "s"))

(defun restore-cursor-position ()
  (csi "u"))

(defun set-cursor-position (row col)
  (cond ((and row col)
         (csi row ";" col "H"))
        ((not (null col))
         (csi row ";H"))
        ((not (null row))
         (csi ";" col "H"))))

(defmacro with-cursor-position ((row col) &body body)
  `(progn
     (save-cursor-position)
     (set-cursor-position ,row ,col)
     (unwind-protect (progn ,@body)
       (restore-cursor-position))))

(defun (setf cursor-visibility) (visiblep)
  (if visiblep
      (csi "?" 2 5 "h")
      (csi "?" 2 5 "l")))

(defun (setf alt-is-meta) (bool)
  (if bool
      (setf +alt-mod+ +meta-mod+)
      (setf +alt-mod+ +alt-mod*+)))

(defmethod (setf fgc) :after (rgb (instance console))
  (apply #'set-foreground-color rgb))

(defmethod (setf bgc) :after (rgb (instance console))
  (apply #'set-background-color rgb))

(defmethod (setf pos) :before (pos (instance console))
  (check-type (car pos) (integer 1))
  (check-type (cdr pos) (integer 1)))

(defmethod (setf pos) :after (pos (instance console))
  (set-cursor-position (car pos) (cdr pos)))

(defmethod (setf cvp) :after (cvp (instance console))
  (setf (cursor-visibility) (not (null cvp))))

(defun keyp (ch key &rest mods)
  (if (null mods)
      (eql ch key)
      (and (typep ch 'gesture)
           (eql (gesture-key ch) key)
           (eql (gesture-mods ch)
                (loop for m in mods
                      summing (ecase m
                                (:c1 +c1-mod+)
                                (:m +meta-mod+)
                                (:c +ctrl-mod+)
                                (:a +alt-mod*+)
                                (:s +shift-mod+)))))))

;;;; Thing-starters
(defun init-console ()
  (prog1 (enable-raw)
    (reset-console)))

(defun close-console (handler)
  (disable-raw handler)
  (reset-console))

(defmacro with-console ((&rest args
                         &key ios fgc bgc cvp fps &allow-other-keys)
                        &body body)
  (declare (ignorable fgc bgc cvp fps))
  `(let ((*console-io* ,ios)
         (*console* (make-instance 'console ,@args)))
     (unwind-protect (progn ,@body)
       (close-console (hnd *console*)))))

(defun show-screen ()
  (loop for ch = (read-input)
        until (null ch)
        do (push ch (app *console*))
           (cond
             ((keyp ch #\Q :c) (cl-user::quit))
             ((keyp ch #\R :c)
              (setf (app *console*) nil)
              (clear-console))
             ((keyp ch #\U :c) (ignore-errors (user-action)))))
  (let ((ch (app *console*)))
    (setf (app *console*)
          (subseq ch 0 (min 12 (length ch)))))
  (set-cursor-position (1+ (random 12))
                       (1+ (random 40)))
  (if (zerop (random 2))
      (put "+")
      (put "-"))
  (with-cursor-position (1 44)
    (loop for row from 1
          for ch in (app *console*)
          do (set-cursor-position row 44)
             (format *console-io* (format nil "Read: ~s" ch))
          (clear-line 0))))

;; for example
(defun user-action ()
  (setf (fgc *console*) (list (random 255) (random 255) (random 255)))
  (setf (bgc *console*) (list (random 255) (random 255) (random 255)))
  (clear-console))

(defun start-display ()
  (unless *server-started*
    (ignore-errors
     (setf *server-started* t)
     (slynk:create-server :port 5555 :dont-close t)))
  (loop
    (with-simple-restart (run-again "Run again")
      (with-console (:ios *terminal-io*)
        (display-loop)))))

(defun read-input (&aux (ch (read-char-no-hang *console-io*)))
  (cond ((or (null ch) (graphic-char-p ch))
         (return-from read-input ch))
        ((deletep ch))
        ((escapep ch))
        ((controlp ch))
        (t (error "Unknown input sequence, char code 0xx~%." (char-code ch)))))

(defun display-loop ()
  (clear-console)
  (loop
    (sleep (/ (fps *console*)))
    (show-screen)))
