;;; rivet-mode.el -- A major mode for Apache Rivet -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright 2019
;; Version: 3.0.0
;; Package-Requires ((emacs "25") (web-mode) (tcl))
;; URL: https://gitlab.com/thornjad/rivet
;;
;; This file is not part of GNU Emacs
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties
;; with regard to this software including all implied warranties of
;; merchantability and fitness. In no event shall the author be liable for
;; any special, direct, indirect, or consequential damages or any damages
;; whatsoever resulting from loss of use, data or profits, whether in an
;; action of contract, negligence or other tortious action, arising out of
;; or in connection with the use or performance of this software.
;;
;;; Commentary;
;;
;; Rivet mode enables mode-switching within Apache Rivet files, preserving
;; indentation and other functionality. Rivet mode is based on work done on
;; two-mode-mode by the Apache Software Foundation in 1999.
;;
;;; Code:


(require 'tcl)
(require 'web-mode)

(defvar host-mode (list "Web" 'web-mode))
(defvar inner-mode (list "TCL" "<?" "?>" 'tcl-mode))

(defvar rivet-mode-update nil)
(defvar rivet-mode-idle-timer nil)
(defvar rivet-mode-p nil)
(defvar rivet-mode-delay 0)

(defvar rivet-hook nil
  "*Hook called by `rivet-mode'.")
(defvar rivet-switch-hook nil
  "*Hook called upon mode switching.")


;;; Setup and funs

(defun rivet-mode-setup ()
  (add-hook 'post-command-hook 'rivet-mode-need-update nil t)
  (make-local-variable 'rivet-mode-p)
  (setq rivet-mode-p t)
  (when rivet-mode-idle-timer
    (cancel-timer rivet-mode-idle-timer))
  (setq rivet-mode-idle-timer
        (run-with-idle-timer rivet-mode-delay t 'rivet-mode-update-mode))
  (make-local-variable 'minor-mode-alist)
  (or (assq 'rivet-mode-p minor-mode-alist)
     (setq minor-mode-alist
           (cons '(rivet-mode-p " rivet-mode") minor-mode-alist))))

(defun rivet-mode-need-update ()
  (setq rivet-mode-update t))

(defun rivet-change-mode (to-mode func)
  (let ((mode (if (listp mode-name) (car (last mode-name)) mode-name)))
    (if (string= to-mode mode)
        t
      (funcall func)
      ;; After the mode was set, we reread the "Local Variables" section.
      (hack-local-variables)

      (rivet-mode-setup)
      (if rivet-switch-hook
          (run-hooks 'rivet-switch-hook))
      (if (eq font-lock-mode t)
          (font-lock-ensure))
      (if (fboundp 'turn-on-font-lock-if-enabled)
          (turn-on-font-lock-if-enabled)
        (turn-on-font-lock-if-desired)))))

(defun rivet-mode-update-mode ()
  (when (and rivet-mode-p rivet-mode-update)
    (setq rivet-mode-update nil)
    (let ((lm -1) (rm -1))
      (save-excursion
        (if (search-backward (cadr inner-mode) nil t)
            (setq lm (point))))
      (save-excursion
        (if (search-backward (car (cddr inner-mode)) nil t)
            (setq rm (point))))
      (if (and (not (and (= lm -1) (= rm -1))) (>= lm rm))
          (rivet-change-mode (car inner-mode) (car (cdr (cddr inner-mode))))
        (rivet-change-mode (car host-mode) (cadr host-mode))))))

;;;###autoload
(defun rivet-mode ()
  "Turn on Rivet mode"
  (interactive)
  (setq rivet-mode-update t)
  (funcall (cadr host-mode))
  (rivet-mode-setup)
  (rivet-mode-update-mode)
  (if rivet-hook
      (run-hooks 'rivet-hook)))

(provide 'rivet-mode)

;;; rivet-mode.el ends here
