;;; rivet-mode.el -- A major mode for Apache Rivet
;;
;; Author: Jade Michael Thornton
;; Copyright 2019
;; Version: 3.0.0
;; Package-Requires ((emacs "25") (polymode "0.1.5") (web-mode) (tcl))
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

(defvar default-mode (list "HTML" 'html-mode))
(defvar second-mode (list "Tcl" "<?" "?>" 'tcl-mode))

(defvar rivet-update 0)
(defvar rivet-mode-idle-timer nil)
(defvar rivet-bool nil)
(defvar rivet-mode-delay (/ (float 1) (float 8)))

;; Rivet mode hook
(defvar rivet-hook nil
  "*Hook called by `rivet-mode'.")
(setq rivet-hook nil)

;; Mode switching hook
(defvar rivet-switch-hook nil
  "*Hook called upon mode switching.")
(setq rivet-switch-hook nil)

(defun rivet-mode-setup ()
  (add-hook 'post-command-hook 'rivet-mode-need-update nil t)
  (make-local-variable 'minor-mode-alist)
  (make-local-variable 'rivet-bool)
  (setq rivet-bool t)
  (when rivet-mode-idle-timer
    (cancel-timer rivet-mode-idle-timer))
  (setq rivet-mode-idle-timer
				(run-with-idle-timer rivet-mode-delay t
														 'rivet-mode-update-mode))
  (or (assq 'rivet-bool minor-mode-alist)
      (setq minor-mode-alist
						(cons '(rivet-bool " rivet-mode") minor-mode-alist))))

(defun rivet-mode-need-update ()
  (setq rivet-update 1))

(defun rivet-change-mode (to-mode func)
  (if (string= to-mode mode-name)
      t
    (progn
      (funcall func)
      ;; After the mode was set, we reread the "Local Variables" section.
      ;; We do need this for example in SGML-mode if "sgml-parent-document"
      ;; was set, or otherwise it will be reset to nil when sgml-mode is left.
      (hack-local-variables)

      (rivet-mode-setup)
      (if rivet-switch-hook
					(run-hooks 'rivet-switch-hook))
      (if (eq font-lock-mode t)
					(font-lock-fontify-buffer))
			(turn-on-font-lock-if-desired))))

(defun rivet-mode-update-mode ()
  (when (and rivet-bool rivet-update)
    (setq rivet-update 0)
		(let ((flag 0)
					(mode second-mode)
					(lm -1)
					(rm -1))
			(save-excursion
				(if (search-backward (cadr mode) nil t)
						(setq lm (point))
					(setq lm -1)))
			(save-excursion
				(if (search-backward (car (cddr mode)) nil t)
						(setq rm (point))
					(setq rm -1)))
			(if (and (not (and (= lm -1) (= rm -1))) (>= lm rm))
					(progn
						(setq flag 1)
						(rivet-change-mode (car mode) (car (cdr (cddr mode))))))
			(if (= flag 0)
					(rivet-change-mode (car default-mode) (cadr default-mode))))))

;;;###autoload
(defun rivet-mode ()
  "Turn on rivet-mode"
  (interactive)
  (funcall (cadr default-mode))
  (rivet-mode-setup)
  (if rivet-hook
			(run-hooks 'rivet-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rvt$" . rivet-mode))

(provide 'rivet-mode)

;;; rivet-mode.el ends here
