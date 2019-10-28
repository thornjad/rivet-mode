;;; rivet-mode.el --- Minor mode for editing Apache Rivet -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2019 Jade Michael Thornton
;; Package-Requires: ((emacs "24") (web-mode "16"))
;; URL: https://gitlab.com/thornjad/rivet
;; Version: 3.4.0
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
;;; Commentary:
;;
;; Rivet mode is a minor mode which enables mode-switching within Apache Rivet
;; files, preserving indentation and other functionality. Note that hooks are
;; not a focus right now and may not work. They will get more attention later.
;;
;; Rivet mode was originally based in part on work done on two-mode-mode by the
;; Apache Software Foundation in 1999.
;;
;;; Code:


(require 'tcl)
(require 'web-mode)

(defvar rivet-mode-host-mode '("Web" #'web-mode)
  "The host mode is the 'outer' mode, used for HTML.

Format is '(NAME MAJOR-MODE).")

(defvar rivet-mode-inner-mode '("TCL" #'tcl-mode)
  "The inner mode is contained within the rivet-mode-delimiters, used for TCL.

Format is '(NAME MAJOR-MODE).")

(defvar rivet-mode-delimiters '("<?" "?>")
  "These delimiters (left and right) denote the boundaries of the inner mode
  (TCL).

Format is '(LEFT-DELIMITER RIGHT-DELIMITER). Note that the '<?=' syntax is still included since it begins with '<?'.")

;; TODO is this even necessary?
(make-variable-buffer-local
 (defvar rivet-mode-p t))

(make-variable-buffer-local
 (defvar rivet-mode--last-position 0
   "Cursor postion from the last time an update was attempted.

This provides a nice way to keep the update from running after /every/
command."))

(defvar rivet-hook nil
  "*Hook called by `rivet-mode'.")
(defvar rivet-switch-hook nil
  "*Hook called upon mode switching.")


;;; Setup and funs

(defun rivet-mode-change-mode (to-mode)
  "Call TO-MODE, then set up the hook again and run rivet-switch-hook."

  ;; call our new mode function
  (funcall (cadr to-mode))

  ;; HACK this is crappy, but for some reason that funcall removes us from the
  ;; post-command hook, so let's put us back in.
  (add-hook 'post-command-hook 'rivet-maybe-update-mode nil t)
  (setq rivet-mode-p t)

  ;; After the mode was set, we reread the "Local Variables" section.
  (hack-local-variables)

  (if rivet-switch-hook (run-hooks 'rivet-switch-hook)))

(defun rivet-mode-change-mode-if-different (to-mode)
  "Change to TO-MODE if current mode is not TO-MODE."
  (unless (string= major-mode (cadr to-mode))
    (rivet-mode-change-mode to-mode)))

(defun rivet-maybe-update-mode ()
  (when (and rivet-mode-p
           (not (region-active-p))
           (not (equal (point) rivet-mode-last-position)))

    ;; cache our position for the next call
    (setq rivet-mode-last-position (point))

    (let ((last-left-delim -1) (last-right-delim -1))
      (save-excursion
        (if (search-backward (caddr rivet-mode-inner-mode) nil t)
            (setq last-left-delim (point))))
      (save-excursion
        (if (search-backward (cadddr rivet-mode-inner-mode) nil t)
            (setq last-right-delim (point))))
      (if (and (not (and (= last-left-delim -1)
                     (= last-right-delim -1)))
             (>= last-left-delim last-right-delim))
          (rivet-mode-change-mode-if-different rivet-mode-inner-mode)
        (rivet-mode-change-mode-if-different rivet-mode-host-mode)))))

;;;###autoload
(defun rivet-mode ()
  "Turn on Rivet mode."
  (interactive)

  (funcall (cadr rivet-mode-host-mode))
  (setq rivet-mode-p t)

  ;; TODO need a way to make this take less time, and/or not call on EVERY post
  ;; command
  (add-hook 'post-command-hook 'rivet-maybe-update-mode nil t)
  (make-local-variable 'minor-mode-alist)

  (or (assq 'rivet-mode-p minor-mode-alist)
     (setq minor-mode-alist
           (cons '(rivet-mode-p " rivet-mode") minor-mode-alist)))

  (rivet-maybe-update-mode)

  (if rivet-hook
      (run-hooks 'rivet-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rvt\\'" . rivet-mode))

(provide 'rivet-mode)

;;; rivet-mode.el ends here
