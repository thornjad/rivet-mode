;;; rivet-mode.el -- A major mode for Apache Rivet -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright 2019
;; Version: 3.2.0
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
;; Rivet mode is a minor mode which enables mode-switching within Apache Rivet
;; files, preserving indentation and other functionality. Rivet mode is based in
;; some part on work done on two-mode-mode by the Apache Software Foundation in
;; 1999.
;;
;; Note that hooks are not a focus right now and may not work. They will get
;; more attention later.
;;
;; There is a known issue where commands in rivet-mode buffers sometimes lag.
;; This is mostly due to inneficiencies in this code, and the cost of loading
;; major modes repeatedly. This is being investigated.
;;
;;; Code:


(require 'tcl)
(require 'web-mode)

(defvar rivet-mode-host-mode (list "Web" #'web-mode))
(defvar rivet-mode-inner-mode (list "TCL" #'tcl-mode "<?" "?>"))

(defvar-local rivet-mode-p nil)

(defvar rivet-hook nil
  "*Hook called by `rivet-mode'.")
(defvar rivet-switch-hook nil
  "*Hook called upon mode switching.")


;;; Setup and funs

(defun rivet-mode-change-mode (to-mode)
  (funcall (cadr to-mode))

  ;; HACK this is crappy, but for some reason that funcall removes us from the
  ;; post-command hook, so let's put us back in.
  (add-hook 'post-command-hook 'rivet-mode-update-mode nil t)
  (setq-local rivet-mode-p t)

  ;; After the mode was set, we reread the "Local Variables" section.
  (hack-local-variables)

  (if rivet-switch-hook
      (run-hooks 'rivet-switch-hook))
  (if font-lock-mode (font-lock-ensure))
  (if (fboundp 'turn-on-font-lock-if-enabled)
      (turn-on-font-lock-if-enabled)
    (turn-on-font-lock-if-desired)))

(defun rivet-mode-maybe-change-mode (to-mode)
  "Change to TO-MODE if current mode is not TO-MODE."
  (unless (string= major-mode (car to-mode))
    (rivet-mode-change-mode to-mode)))

(defun rivet-mode-update-mode ()
  (when (and rivet-mode-p (not (region-active-p)))
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
          (rivet-mode-maybe-change-mode rivet-mode-inner-mode)
        (rivet-mode-maybe-change-mode rivet-mode-host-mode)))))

;;;###autoload
(defun rivet-mode ()
  "Turn on Rivet mode"
  (interactive)

  (funcall (cadr rivet-mode-host-mode))
  (setq-local rivet-mode-p t)

  ;; TODO need a way to make this take less time, and/or not call on EVERY post
  ;; command
  (add-hook 'post-command-hook 'rivet-mode-update-mode nil t)
  (make-local-variable 'minor-mode-alist)

  (or (assq 'rivet-mode-p minor-mode-alist)
     (setq minor-mode-alist
           (cons '(rivet-mode-p " rivet-mode") minor-mode-alist)))

  (rivet-mode-update-mode)

  (if rivet-hook
      (run-hooks 'rivet-hook)))

(provide 'rivet-mode)

;;; rivet-mode.el ends here
