;;; evil-iedit.el --- Simple iedit integration for evil  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/evil-iedit
;; Version: 0
;; Keywords:
;; Package-Requires: ((emacs "24.5") (evil "1.2.12") (iedit "0.97"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a work in progress, but the aim of this package is to provide a
;; simple integration of iedit and evil without the use of new states like
;; evil-multiedit and evil-iedit-state. Instead a minor mode is used, which is
;; the same mechanism used by iedit itself.

;;; Code:

(require 'evil)
(require 'iedit)
(require 'cl-lib)

(defcustom evil-iedit-use-symbols evil-symbol-word-search
  "Prefer to grab symbols instead of words when selecting
occurrences.")

(defcustom evil-iedit-use-punctuation t
  "Allow selection of \"punctuation\" as an occurrence for
 iedit. Punctuation is defined by the major-mode's syntax table.")

(defcustom evil-iedit-use-urls t
  "Allow selection of urls as an occurrence for iedit.")


(evil-define-text-object evil-iedit-inner-occurrence
  (count &optional begin end type)
  ""
  (let ((ov (iedit-find-current-occurrence-overlay)))
    (when ov
      (evil-range (overlay-start ov)
                  (overlay-end ov)
                  'exclusive :expanded t))))

(define-key evil-inner-text-objects-map "i" 'evil-iedit-inner-occurrence)

;; (evil-define-text-object evil-iedit-a-occurrence
;;   (count &optional begin end type)
;;   ""
;;   (let ((ov (iedit-find-current-occurrence-overlay)))
;;     (when ov
;;       (evil-range (overlay-start ov)
;;                   (overlay-end ov)
;;                   'inclusive :expanded t))))

(defun evil-iedit-get-occurrence ()
  (let (bounds occurrence-str)
    (cond
     ((evil-visual-state-p)
      (setq bounds (cons evil-visual-beginning evil-visual-end))
      (setq occurrence-str (buffer-substring-no-properties
                            evil-visual-beginning
                            evil-visual-end))
      (setq iedit-occurrence-type-local 'selection))
     ((and evil-iedit-use-punctuation
           (looking-at "\\s.+")
           (skip-syntax-backward ".")
           (looking-at "\\s.+"))
      (setq bounds (cons (match-beginning 0) (match-end 0)))
      (setq occurrence-str (match-string 0))
      (setq iedit-occurrence-type-local 'symbol))
     ((and evil-iedit-use-urls
           (require 'ffap nil t)
           (thing-at-point 'url))
      (setq bounds (bounds-of-thing-at-point 'url))
      (setq occurrence-str (thing-at-point 'url))
      (setq iedit-occurrence-type-local 'url))
     ((and evil-iedit-use-symbols
           (thing-at-point 'symbol))
      (setq bounds (bounds-of-thing-at-point 'symbol))
      (setq occurrence-str (thing-at-point 'symbol))
      (setq iedit-occurrence-type-local 'symbol))
     ((thing-at-point 'word)
      (setq bounds (bounds-of-thing-at-point 'evil-word))
      (setq occurrence-str (thing-at-point 'evil-word))
      (setq iedit-occurrence-type-local 'word)))
    (when occurrence-str
      (list occurrence-str (car-safe bounds) (cdr-safe bounds)))))

(defun evil-iedit-start (occurrence-exp &optional beg end)
  (unless occurrence-exp
    (user-error "Cannot find occurrence to use."))
  (setq iedit-initial-string-local occurrence-exp)
  (iedit-start (iedit-regexp-quote occurrence-exp)
               (or beg (point-min)) (or end (point-max)))
  (evil-iedit-mode 1)
  (evil-normal-state)
  (isearch-update-ring occurrence-exp t))

;;;###autoload
(defun evil-iedit-add-all ()
  "Start iedit and add all occurrences based on point and
state.

In visual state, the selected region is used. The \"punctuation\"
at point is used if `evil-iedit-use-punctuation' is
non-nil. Otherwise the symbol or the word at point is used
depending on the value of `evil-iedit-use-symbols'."
  (interactive)
  (when evil-iedit-mode (evil-iedit-mode -1))
  (evil-iedit-start (car-safe
                     (evil-iedit-get-occurrence))))

;;;###autoload
(defun evil-iedit-add-all-above ()
  "Start iedit and add all occurrences based on point and
state.

In visual state, the selected region is used. The \"punctuation\"
at point is used if `evil-iedit-use-punctuation' is
non-nil. Otherwise the symbol or the word at point is used
depending on the value of `evil-iedit-use-symbols'."
  (interactive)
  (when evil-iedit-mode (evil-iedit-mode -1))
  (let ((occurrence (evil-iedit-get-occurrence)))
    (when occurrence
      (evil-iedit-start (car occurrence)
                        nil
                        (caddr occurrence)))))

;;;###autoload
(defun evil-iedit-add-all-below ()
  "Start iedit and add all occurrences based on point and
state.

In visual state, the selected region is used. The \"punctuation\"
at point is used if `evil-iedit-use-punctuation' is
non-nil. Otherwise the symbol or the word at point is used
depending on the value of `evil-iedit-use-symbols'."
  (interactive)
  (when evil-iedit-mode (evil-iedit-mode -1))
  (let ((occurrence (evil-iedit-get-occurrence)))
    (when occurrence
      (evil-iedit-start (car occurrence)
                        (cadr occurrence)))))

;;;###autoload
(defun evil-iedit-add-all-in-function ()
  "Start iedit and add all occurrences based on point and
state, restricting to the current function.

In visual state, the selected region is used. The \"punctuation\"
at point is used if `evil-iedit-use-punctuation' is
non-nil. Otherwise the symbol or the word at point is used
depending on the value of `evil-iedit-use-symbols'."
  (interactive)
  (when evil-iedit-mode (evil-iedit-mode -1))
  (evil-iedit-start (car-safe
                     (evil-iedit-get-occurrence)))
  (iedit-restrict-function))

;;;###autoload
(defun evil-iedit-add-down (&optional up)
  "When iedit is active, add the next occurrence in the buffer to
the currently active occurrences. Otherwise, start iedit and add
the first occurrence according to the following rules.

In visual state, the selected region is used. The \"punctuation\"
at point is used if `evil-iedit-use-punctuation' is
non-nil. Otherwise the symbol or the word at point is used
depending on the value of `evil-iedit-use-symbols'."
  (interactive)
  (if (and evil-iedit-mode
           (iedit-current-occurrence-string))
      (progn
        (if up
            (iedit-expand-up-to-occurrence)
          (iedit-expand-down-to-occurrence)))
    (apply #'evil-iedit-start (evil-iedit-get-occurrence)))) 

;;;###autoload
(defun evil-iedit-add-up ()
  "This is the same as `evil-iedit-add-down', but it adds
occurrences that occur before point."
  (interactive)
  (evil-iedit-add-down t))

(define-minor-mode evil-iedit-mode
  "A minor-mode that facilitates integration of iedit mode and
evil mode. It is not meant to be enabled directly by the user."
  nil nil nil
  (cond (evil-iedit-mode
         (define-key evil-inner-text-objects-map
           "i" 'evil-iedit-inner-occurrence))
        (t
         (when (eq (lookup-key evil-inner-text-objects-map "i")
                   'evil-iedit-inner-occurrence)
           (define-key evil-inner-text-objects-map "i" nil))
         (iedit-done))))

(defun turn-off-evil-iedit-mode ()
  "Turn off `evil-iedit-mode'."
  (interactive)
  (evil-iedit-mode -1))

(evil-define-minor-mode-key 'normal 'evil-iedit-mode
  [escape]    'turn-off-evil-iedit-mode
  (kbd "RET") 'iedit-toggle-selection
  (kbd "M-d") 'evil-iedit-add-down
  (kbd "M-D") 'evil-iedit-add-up
  (kbd "M-n") 'iedit-toggle-unmatched-lines-visible
  "n"         'iedit-next-occurrence
  "N"         'iedit-prev-occurrence
  "F"         'iedit-restrict-function
  "L"         'iedit-restrict-current-line)

(defvar evil-iedit-occurrence-keymap
  (let ((map (make-sparse-keymap)))
    ;;  (set-keymap-parent map iedit-lib-keymap)
    ;; (define-key map (kbd "M-U") 'iedit-upcase-occurrences)
    ;; (define-key map (kbd "M-L") 'iedit-downcase-occurrences)
    ;; (define-key map (kbd "M-R") 'iedit-replace-occurrences)
    ;; (define-key map (kbd "M-SPC") 'iedit-blank-occurrences)
    ;; (define-key map (kbd "M-D") 'iedit-delete-occurrences)
    (define-key map (kbd "M-N") 'iedit-number-occurrences)
    (define-key map (kbd "M-B") 'iedit-toggle-buffering)
    (define-key map (kbd "M-<") 'iedit-goto-first-occurrence)
    (define-key map (kbd "M->") 'iedit-goto-last-occurrence)
    (define-key map (kbd "C-?") 'iedit-help-for-occurrences)
    map))

(when evil-iedit-occurrence-keymap
  (setq iedit-occurrence-keymap-default evil-iedit-occurrence-keymap)
  (setq iedit-mode-occurrence-keymap evil-iedit-occurrence-keymap)
  (setq iedit-occurrence-keymap evil-iedit-occurrence-keymap))

(provide 'evil-iedit)
;;; evil-iedit.el ends here
