;;; modules/dwim-delim.el --- DWIM delimiter text objects -*- lexical-binding: t; -*-
;;
;; Select inside the nearest enclosing delimiter without specifying
;; which one.  Reduces keystrokes and hand strain for the most common
;; case of "grab what's inside the thing I'm in."
;;
;; Provides:
;;   - `id` / `ad` evil text objects (inner/around nearest delimiter)
;;   - SPC i d   normal-mode binding to visually select inner delimiter

(defun +dwim-delim--try-text-object (obj)
  "Call evil text object function OBJ, return range or nil."
  (save-excursion
    (condition-case nil
        (funcall obj 1)
      (error nil))))

(defun +dwim-delim--range-distance (range pos)
  "Return the distance from POS to RANGE.
Zero if POS is inside the range, otherwise the distance to the
nearest edge."
  (let ((beg (evil-range-beginning range))
        (end (evil-range-end range)))
    (cond
     ((and (>= pos beg) (<= pos end)) 0)
     ((< pos beg) (- beg pos))
     (t (- pos end)))))

(defun +dwim-delim--nearest-range (inclusive)
  "Return the nearest evil range across all delimiter and quote pairs.
Prefers the range whose opening delimiter is closest to point.
Ties are broken by range size (tightest wins).
INCLUSIVE controls whether delimiters are included in the range."
  (let ((best nil)
        (best-dist most-positive-fixnum)
        (best-size most-positive-fixnum)
        (pos (point))
        (objects (if inclusive
                     (list #'evil-a-paren #'evil-a-bracket #'evil-a-curly
                           #'evil-a-double-quote #'evil-a-single-quote
                           #'evil-a-back-quote)
                   (list #'evil-inner-paren #'evil-inner-bracket #'evil-inner-curly
                         #'evil-inner-double-quote #'evil-inner-single-quote
                         #'evil-inner-back-quote))))
    (dolist (obj objects)
      (let ((range (+dwim-delim--try-text-object obj)))
        (when range
          (let ((dist (+dwim-delim--range-distance range pos))
                (size (- (evil-range-end range) (evil-range-beginning range))))
            (when (or (< dist best-dist)
                      (and (= dist best-dist) (< size best-size)))
              (setq best-dist dist
                    best-size size
                    best range))))))
    best))

(evil-define-text-object +dwim-delim-inner (count &optional beg end type)
  "Select inside the nearest enclosing delimiter."
  :extend-selection nil
  (+dwim-delim--nearest-range nil))

(evil-define-text-object +dwim-delim-around (count &optional beg end type)
  "Select around the nearest enclosing delimiter."
  :extend-selection nil
  (+dwim-delim--nearest-range t))

(define-key evil-inner-text-objects-map "d" #'+dwim-delim-inner)
(define-key evil-outer-text-objects-map "d" #'+dwim-delim-around)

(map! :leader
      (:prefix ("i" . "inner")
       :desc "Select inner delimiter" "d" (cmd!
                                           (let ((range (+dwim-delim--nearest-range nil)))
                                             (when range
                                               (evil-visual-char
                                                (evil-range-beginning range)
                                                (1- (evil-range-end range))))))))
