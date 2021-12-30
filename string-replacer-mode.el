;;; string-replacer-mode.el -*- lexical-binding: t; -*-
;;;
;; Copyright (C) 2020, 2021 Free Software Foundation, Inc. - IDK how to change this

;; Author: Franco Rivera <franco@wemake.pe>
;; Homepage: https://github.com/francorivera/svg-tag-mode
;; Keywords: convenience
;; Version: 0.1.0

;; Package-Requires: ((emacs "27.1")) // not tested really

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This minor mode replaces keywords or expressions with SVG tags
;; that are fully customizable and activable.
;;
;; Usage example:
;; --------------
;;
;; (setq string-replacer-strings '((":TODO:"  ((lambda (tag)
;;                                      (svg-tag-make "TODO"))))))
;;
;; Each item has the form '(KEYWORD (TAG COMMAND HELP)) where:
;;  - KEYWORD is a regular expression including a matched group of
;;    the form "\\(xxx\\)". If this is not the case the whole
;;    string will be used a the matched group.
;;  - TAG is either a SVG image that will be displayed using the
;;    'display property or a function that accepts a unique string
;;    argument (match-string 1) and returns an SVG image.
;;  - COMMAND is a command to be executed when user clicks on the tag.
;;    It can be nil if no command is associated with the tag.
;;  - HELP is a string to be displayed when mouse pointer is over
;;    the tag. It can be nil if no command is associated with the tag.
;;
;; A lot of minor-mode code setup is inspired by svg-tag-mode by @rougier
;;
;;; NEWS:
;;
;; Version 0.1:
;; - Proof of concept
;;

;;; Code:

(defgroup string-replacer nil
  "Replaces any string with another string, of any length"
  :group 'convenience
  :prefix "string-replacer-")

(defun string-replacer--get-buffer-windows ()
  "Return a list of windows displaying the current buffer."
  (get-buffer-window-list (current-buffer) 'no-minibuf nil))

(defun string-replacer--run-meme-in-all-windows ()
  "Redraw the fill-column rule in all windows showing the current buffer."
  (dolist (win (string-replacer--get-buffer-windows))
    (string-replacer--my-meme (window-start win) (window-end win t))))

(defvar string-replacer--active-overlays nil
 "Container for the overlays.")

;; create/delete overlays

; ----

(defun string-replacer--fci-delete-overlays-region (start end)
  "Delete overlays displaying the fill-column rule between START and END."
  (mapc #'(lambda (o) (if (overlay-get o 'fci) (delete-overlay o)))
        (overlays-in start end)))

(defsubst string-replacer--fci-posn-visible-p (posn ranges)
  "Return true if POSN falls within an interval in RANGES."
  (memq t (mapcar #'(lambda (range) (and (<= (car range) posn)
                                         (< posn (cdr range))))
                  ranges)))


(defun string-replacer--remove-all-boy ()
  "Delete all pretty tags overlays created."
  (while string-replacer--active-overlays
    (delete-overlay (pop string-replacer--active-overlays))))

;; (defvar meme-replace-string "-"
;;   "The string thats gonna be used to replace the buffer, only the first char is gonna get set tho so set a single unicode symbol")
(setq-default string-replacer--meme-replace-string "-")
(setq string-replacer--meme-replace-string "a")
(setq string-replacer--meme-replace-string "◌")
(setq string-replacer--meme-replace-string "❯")
(setq string-replacer--meme-replace-string "⎓")
(setq string-replacer--meme-replace-string "🌵")

(defvar string-replacer--font-height 2.0)
(setq string-replacer--font-height 2.0)
(defvar string-replacer--font-width string-replacer--font-height) ;; default value is the height
(setq string-replacer--font-width 4.0) ;; set a custom width value

(defun string-replacer--get-face-list ()
      (list :family "SF Pro Display"          ; can be any "Monaco"
        :height string-replacer--font-height
        ;:width ultra-condensed
        ; :background "#21242b"         ; dark gray like topbar
        ;:foreground "#586e75"         ; medium dark gray
        :foreground "#93a1a1"           ; light dark gray
        ; :slant italic                 ; doesnt work with the chevron symbol
        ; :weight ultra-light           ; doesnt work with the chevron symbol
        ;:box t
        ;; See other options at:
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
        )
  )


(defun string-replacer--my-get-window-width ()
  "Get width of current window in."
  (round (/ (window-total-width) string-replacer--font-width)))

; ---------
; (string-replacer--my-meme 0 (point-max))

(defun string-replacer--my-meme (start end)
  ; remove all stored display memes and replace them at every execution, not the most efficient meme pero it owrks
  (string-replacer--remove-all-boy)
  (let (
        (stored-point (point))
        ;(my-font-size string-replacer--font-height)
        )
    ; go back to where we stared after finishing the function
    (save-excursion
      (goto-char start)
      (while (re-search-forward "-----[-]+\n" end t)
        (if
            (and (<= (point-at-bol 0) stored-point)
                 (<= stored-point (point-at-eol 0)))
        ;; If the cursor is at this line, then just skip the expansion
        ;; (probably should change the current overlay to "invisible" instead of deleting every)
        ;; see docs at https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html
            (progn)
          ;; if its not in the line then execute the expansion
            (progn
              (push (make-overlay
                     (save-excursion (beginning-of-line) (match-beginning 0))
                     ;(match-beginning 0)
                     ;(point-at-bol) ; start
                     (- (match-end 0) 1)) ; remove new line
                    string-replacer--active-overlays)
              ;; display icon
              ;; find appropiate length of buffer taking in consideration the height/width of the font as well
              (let (
                    (buffer-width ;'40
                     (-  (string-replacer--my-get-window-width) 1
                         ;(round (/  (- (match-end 0) (match-beginning 0)) string-replacer--font-height))
                         )  ; random variable padding for org mode
                     ))
                (overlay-put (car string-replacer--active-overlays) 'display
                             (make-string buffer-width
                                          (aref string-replacer--meme-replace-string 0))))
              ;; change color as well
              (overlay-put (car string-replacer--active-overlays) 'face (string-replacer--get-face-list))
              ;(overlay-put (car string-replacer--active-overlays) 'font-lock-mode 'black)
              ;; do other stuff
                    ))))))

;--------------------------------------------------------------------------------

(defun string-replacer-mode-on ()
  "Activate string-replacer-mode"
  (add-hook 'post-command-hook #'string-replacer--run-meme-in-all-windows nil t)
  (add-hook 'window-configuration-change-hook #'string-replacer--run-meme-in-all-windows nil t)

  ;; Redisplay everything to show tags
  (message "String replacer mode on")
  (cursor-sensor-mode 1)
  (font-lock-flush))

(defun string-replacer-mode-off ()
  "Deactivate string-replacer-mode"
  (remove-hook 'post-command-hook #'string-replacer--run-meme-in-all-windows t)
  (remove-hook 'window-configuration-change-hook #'string-replacer--run-meme-in-all-windows t)
  (string-replacer--remove-all-boy)
  ;; Redisplay everything to hide tags
  (message "String replacer mode off")
  (cursor-sensor-mode -1)
  (font-lock-flush))

;(add-hook 'org-mode-hook 'string-replacer-mode-on)

(define-minor-mode string-replacer-mode
  "Minor mode for replace strings"
  :group 'string-replacer
  (if string-replacer-mode
      (string-replacer-mode-on)
    (string-replacer-mode-off)))

(define-globalized-minor-mode
   global-string-replacer-mode string-replacer-mode string-replacer-mode-on)

(provide 'string-replacer-mode)

;;; string-replacer-mode.el ends here
