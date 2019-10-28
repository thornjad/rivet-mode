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
;; Rivet mode is a minor mode for editing Apache Rivet files. It automatically
;; detects whether TCL or HTML is currently being edited and uses the major
;; modes tcl-mode and web-mode, respectively.
;;
;;; Code:

(require 'tcl)
(require 'web-mode)

;;; Variables

(defvar rivet-mode-host-mode '("Web" web-mode)
  "The host mode is the 'outer' mode, i.e. HTML, CSS and JS.

Format is '(NAME MAJOR-MODE).")

(defvar rivet-mode-inner-mode '("TCL" tcl-mode)
  "The inner mode is contained within the `rivet-mode-delimiters', used for TCL.

Format is '(NAME MAJOR-MODE). See `rivet-mode-delimiters' for more on the
demarcation between the inner and host modes.")

(defvar rivet-mode-delimiters '("<?" "?>")
  "These delimiters denote the boundaries of the 'inner' mode, i.e. TCL.

The car and cadr are the left and right delimiters. That is to say the format is
'(LEFT-DELIMITER RIGHT-DELIMITER). Note that the '<?=' output syntax is included
since it begins with '<?'.")

(make-variable-buffer-local
 (defvar rivet-mode--last-position 0
   "Value of point from the last time an update was attempted.

This buffer-local variable allows `rivet-mode' to tell if the point has moved,
and if, therefore, the current mode should be re-evaluated. This variable should
not be changed manually."))

(defvar rivet-mode-hook nil
  "*Hook called upon running minor mode function `rivet-mode'.")
(defvar rivet-mode-change-hook nil
  "*Hook called upon changing between inner and host modes.")


;;; The parts that do the real work

(defun rivet-mode--change-mode (to-mode)
  "Call TO-MODE, then set up the hook again and run rivet-mode-change-hook."

  ;; call our new mode function
  (funcall (cadr to-mode))

  ;; HACK this is crappy, but for some reason that funcall removes us from the
  ;; post-command hook, so let's put us back in.
  (add-hook 'post-command-hook 'rivet-mode--maybe-change-mode nil t)

  ;; After the mode was set, we reread the "Local Variables" section.
  (hack-local-variables)

  (if rivet-mode-change-hook (run-hooks 'rivet-mode-change-hook)))

(defun rivet-mode-change-mode-if-different (to-mode)
  "Change to TO-MODE if current mode is not TO-MODE."
  (unless (equal major-mode (cadr to-mode))
    (rivet-mode-change-mode to-mode)))
(defun rivet-mode--maybe-change-mode ()
  "Change switch between inner and host modes if appropriate.

If there is no active region and point has changed, then determine if point is
in a host or inner section. If point has moved to a different section, change to
that section's major mode."
  (when (and (not (region-active-p))
           (not (equal (point) rivet-mode--last-position)))

    ;; cache our position for the next call
    (setq rivet-mode--last-position (point))

    (let ((last-left-delim -1) (last-right-delim -1))
      (save-excursion
        (if (search-backward (car rivet-mode-delimiters) nil t)
            (setq last-left-delim (point))))
      (save-excursion
        (if (search-backward (cadr rivet-mode-delimiters) nil t)
            (setq last-right-delim (point))))
      (if (and (not (and (= last-left-delim -1)
                     (= last-right-delim -1)))
             (>= last-left-delim last-right-delim))
          (rivet-mode-change-mode-if-different rivet-mode-inner-mode)
        (rivet-mode-change-mode-if-different rivet-mode-host-mode)))))


;;; Minor mode and auto-mode setup


;;;###autoload
(define-minor-mode rivet-mode
  "Minor mode for editing Apache Rivet files.

Rivet mode intelligently switches between TCL and Web major modes for editing
Rivet files."
  :lighter " Rivet"

  ;; Chances are we are at position 1 because the file has just be opened cold.
  ;; Since the inner mode requires delimiters and we could not possibly be
  ;; within a delimiter at position 1 (because the delimiters are at least two
  ;; characters), we must be in the host mode. If, however, we are not at
  ;; position 1, we need to check.
  (if (eql (point) 1)
      (progn
        (funcall (cadr rivet-mode-host-mode))
        (add-hook 'post-command-hook 'rivet-mode--maybe-change-mode nil t))
    (rivet-mode--maybe-change-mode))

  (if rivet-mode-hook (run-hooks 'rivet-mode-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rvt\\'" . rivet-mode))

(provide 'rivet-mode)

;;; rivet-mode.el ends here
