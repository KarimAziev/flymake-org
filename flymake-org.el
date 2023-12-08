;;; flymake-org.el --- Perform on-the-fly linting in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymake-org
;; Version: 0.1.0
;; Keywords: outlines convenience
;; Package-Requires: ((emacs "29.1") (org "9.6.11"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Perform on-the-fly linting in Org mode

;;; Code:


(require 'org)
(require 'org-lint)

(defcustom flymake-org-checkers (mapcar (lambda (c)
                                          (org-lint-checker-name c))
                                        org-lint--checkers)
  "List of all available checkers."
  :group 'flymake-org
  :type `(set :inline t ,@(mapcar (lambda (c)
                                    (let ((name (org-lint-checker-name
                                                 c)))
                                     `(const :tag ,(format "%s (%s)"
                                                    (or (org-lint-checker-summary
                                                         c)
                                                     "")
                                                    name)
                                       ,name)))
                           org-lint--checkers)
          (repeat :inline t symbol)))

(defvar flymake-org-load-filename (or load-file-name buffer-file-name))
(defvar flymake-org-directory nil)

(defvar-local flymake-org--flymake-process nil
  "Buffer-local process started by `flymake-org-flymake-report'.")

(defun flymake-org-feature-symbols (feature)
  "Return the file and list of definitions associated with FEATURE.
The value is actually the element of `load-history'
for the file that did (provide FEATURE)."
  (catch 'foundit
    (let ((element (cons 'provide feature)))
      (dolist (x load-history nil)
  (when (member element (cdr x))
    (throw 'foundit x))))))

(defun flymake-org-feature-file (feature)
  "Return the file name from which a given FEATURE was loaded.
Actually, return the load argument, if any; this is sometimes the name of a
Lisp file without an extension.  If the feature came from an `eval-buffer' on
a buffer with no associated file, or an `eval-region', return nil."
  (if (not (featurep feature))
      (error "%S is not a currently loaded feature" feature)
    (car (flymake-org-feature-symbols feature))))

(defun flymake-org-find-symb-feature (symb)
  "Find the library providing the symbol SYMB.

Argument SYMB is the symbol to search for in the `load-history'."
  (catch 'foundit
    (pcase-dolist (`(,_file . ,items) load-history)
      (dolist (it items)
        (let ((sym (if (consp it)
                       (cdr it)
                     it)))
          (when (eq sym symb)
            (when-let ((lib (cdr (assq 'provide items))))
              (throw 'foundit lib))))))))


(defun flymake-org-get-org-mode-hook-features ()
  "Extract features from `org-mode-hook'."
  (delete-dups
   (delq nil
         (mapcar (lambda (it)
                   (and (symbolp it)
                        (not (or (eq it 'flymake-org-mode)
                                 (eq it 'flymake-org-flymake-init)
                                 (eq it 'flymake-org-on)))
                        (cons it
                              (flymake-org-find-symb-feature it))))
                 org-mode-hook))))

(defun flymake-org-get-eval-string ()
  "Generate string to load and hook `org-mode' features."
  (when-let ((feats (flymake-org-get-org-mode-hook-features)))
    (mapconcat
     (pcase-lambda (`(,sym . ,lib))
       (prin1-to-string `(progn
                           (require ',lib nil t)
                           (when (fboundp ',sym)
                            (add-hook 'org-mode-hook ',sym)))))
     feats)))


(defun flymake-org-flymake-report (report-fn &rest _args)
  "Report linting results for Org files using Flymake.

Optional argument REPORT-FN is a function to call with the linting results.

Remaining arguments _ARGS are ignored and not used within the function."
  (when flymake-org--flymake-process
    (when (process-live-p flymake-org--flymake-process)
      (kill-process flymake-org--flymake-process)))
  (let ((temp-file (make-temp-file "flymake-org"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min)
                    (point-max) temp-file nil 'nomessage))
    (unless flymake-org-directory
      (setq flymake-org-directory (file-name-directory (flymake-org-feature-file 'org))))
    (let* ((output-buffer (generate-new-buffer " *flymake-org*")))
      (setq flymake-org--flymake-process
            (make-process
             :name "flymake-org"
             :buffer output-buffer
             :command (delq nil
                            `(,(expand-file-name invocation-name invocation-directory)
                              "-Q"
                              "--batch"
                              ,@(mapcan (lambda (path)
                                          (list "-L" path))
                                 (append (list "./")
                                  load-path))
                              "-l" ,flymake-org-load-filename
                              "--eval" ,(or (flymake-org-get-eval-string) "nil")
                              "--eval" ,(format "(progn (require 'org) (org-babel-do-load-languages 'org-babel-load-languages '%s))" org-babel-load-languages)
                              "-f" "flymake-org-collect"
                              ,temp-file))
             :connection-type 'pipe
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (unwind-protect
                     (cond
                      ((not (and (buffer-live-p source-buffer)
                                 (eq proc (with-current-buffer source-buffer
                                            flymake-org--flymake-process))))
                       (flymake-log :warning
                                    "flymake-org process %s obsolete" proc))
                      ((zerop (process-exit-status proc))
                       (let* ((data
                               (with-current-buffer output-buffer
                                 (goto-char (point-min))
                                 (search-forward ":flymake-org-flymake-output-start")
                                 (read (point-marker))))
                              (result (mapcar (lambda (it)
                                                (apply #'flymake-make-diagnostic
                                                       source-buffer
                                                       it))
                                              data)))
                         (funcall report-fn result)))
                      (t
                       (funcall report-fn
                                :panic
                                :explanation
                                (format "flymake-org process %s died" proc))))
                   (ignore-errors (delete-file temp-file))
                   (kill-buffer output-buffer))))
             :stderr " *stderr of flymake-org"
             :noquery t)))))

;;;###autoload
(defun flymake-org-collect (&optional file)
  "Collect linting errors from an Org file.

Optional argument FILE is the name of the file to be checked. If not provided,
the current buffer's FILE name is used."
  (interactive (list buffer-file-name))
  (require 'org)
  (require 'org-lint)
  (require 'ob)
  (let* ((file (or file (car command-line-args-left)))
         (result))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (org-mode)
        (goto-char (point-min))
        (let ((ast (org-element-parse-buffer nil nil 'defer)))
          (setq result (mapcan
                        (lambda (c)
                          (let* ((trust
                                  (symbol-name (org-lint-checker-trust c)))
                                 (type (pcase trust
                                         ('high :error)
                                         ('low :warning)
                                         (_ :note)))
                                 (categories (org-lint-checker-categories c))
                                 (category-prefix (if categories (format "%s " categories) "")))
                            (mapcar
                             (lambda (report)
                               (let* ((beg (if (markerp (car report))
                                               (marker-position (car report))
                                             (car report)))
                                      (end (save-excursion
                                             (goto-char beg)
                                             (line-end-position)))
                                      (text (car (split-string (nth 1 report) "\n" t))))
                                 (list beg
                                       end
                                       type
                                       (concat category-prefix text))))
                             (save-excursion
                               (funcall (org-lint-checker-function c)
                                        ast)))))
                        (seq-filter (lambda (c) (memq (org-lint-checker-name c)
                                                      flymake-org-checkers))
                                    org-lint--checkers))))))
    (prin1 :flymake-org-flymake-output-start)
    (terpri)
    (pp result)))

(defun flymake-org-flymake-init ()
  "Add Flymake diagnostic function for Org linting."
  (add-hook 'flymake-diagnostic-functions
            #'flymake-org-flymake-report
            nil
            t))

;;;###autoload
(defun flymake-org-on ()
  "Enable Flymake for Org linting and start it."
  (interactive)
  (require 'flymake)
  (flymake-org-flymake-init)
  (unless (bound-and-true-p flymake-mode)
    (flymake-mode 1))
  (when (fboundp 'flymake-start)
    (flymake-start)))

;;;###autoload
(defun flymake-org-off ()
  "Disable Flymake for Org linting and start it."
  (interactive)
  (remove-hook 'flymake-diagnostic-functions
               #'flymake-org-flymake-report
               t)
  (when (and (bound-and-true-p flymake-mode)
             (not flymake-diagnostic-functions))
    (flymake-mode -1)))

;;;###autoload
(define-minor-mode flymake-org-mode
  "Toggle Flymake linting for Org files.

Enable or disable Flymake for Org mode files to perform on-the-fly linting."
  :lighter " flymake-org-flymake"
  :global nil
  (if flymake-org-mode
      (flymake-org-on)
    (flymake-org-off)))


(provide 'flymake-org)
;;; flymake-org.el ends here