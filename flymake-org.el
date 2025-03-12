;;; flymake-org.el --- Perform on-the-fly linting in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/flymake-org
;; Version: 0.1.0
;; Keywords: outlines convenience
;; Package-Requires: ((emacs "29.1") (org "9.7.15"))
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
                                     `(const :tag
                                       ,(format "%s (%s)"
                                         (or (org-lint-checker-summary
                                              c)
                                          "")
                                         name)
                                       ,name)))
                           org-lint--checkers)
          (repeat :inline t symbol)))

(defvar flymake-org-load-filename (if (bound-and-true-p load-file-name)
                                      load-file-name
                                    (buffer-file-name)))
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
    (pcase-dolist (`(,file . ,items) load-history)
      (dolist (it items)
        (let ((sym (if (consp it)
                       (cdr it)
                     it)))
          (when (eq sym symb)
            (when-let* ((lib (cdr (assq 'provide items))))
              (throw 'foundit (list file lib)))))))))


(defun flymake-org-get-org-mode-hook-features ()
  "Extract features from `org-mode-hook'."
  (delete-dups
   (delq nil
         (mapcar (lambda (it)
                   (and (symbolp it)
                        (not (or (eq it 'flymake-org-mode)
                                 (eq it 'flymake-org-in-file)
                                 (eq it 'flymake-org-flymake-init)
                                 (eq it 'flymake-org-on)))
                        (when-let* ((found (flymake-org-find-symb-feature it)))
                          (append (list it) found))))
                 org-mode-hook))))

(defun flymake-org--get-hook-features-sexps ()
  "Generate a list of expressions to load features for `org-mode-hook'."
  (when-let* ((feats (flymake-org-get-org-mode-hook-features)))
    (let ((result))
      (pcase-dolist (`(,sym ,file ,_) feats)
        (setq result (append `((load ,file t t t)
                               (when (fboundp ',sym)
                                (add-hook 'org-mode-hook ',sym)))
                             result)))
      result)))



(defun flymake-org--get-all-babel-languages ()
  "Retrieve all languages supported by Babel in Org mode."
  (delete-dups
   (mapcar (lambda (it)
             (if (stringp it)
                 (make-symbol it)
               it))
           (append
            (mapcar #'car org-babel-load-languages)
            (mapcar #'car org-src-lang-modes)
            (mapcar (pcase-lambda (`(,_t ,_k ,_label ,sym)) sym)
                    (cdadr (memq :key-type
                                 (get
                                  'org-babel-load-languages
                                  'custom-type))))
            (flymake-org--get-all-external-babel-languages)
            (flymake-org--ob-packages)))))

(defun flymake-org--ob-packages ()
  "List all `org-babel' languages in `features' excluding core and autoloads."
  (let ((langs))
    (dolist (it features)
      (let ((name (symbol-name it)))
        (when (and
               (string-prefix-p "ob-" name)
               (not (member name '("ob-core" "ob-eval" "ob-table" "ob-tangle"
                                   "ob-comint"
                                   "ob-exp"
                                   "ob-lob"
                                   "ob-ref")))
               (not (string-suffix-p "autoloads" name)))
          (push (substring-no-properties name 3) langs))))
    langs))

(defun flymake-org--get-all-external-babel-languages ()
  "Retrieve all external Babel languages supported by Org mode."
  (let* ((suffixes (find-library-suffixes))
         (regexp (concat (regexp-opt suffixes) "\\'"))
         (dirs (seq-remove
                (apply-partially #'string-suffix-p "org")
                load-path))
         (files nil))
    (dolist (dir dirs)
      (dolist (file (ignore-errors (directory-files dir nil regexp t)))
        (when (string-match regexp file)
          (let ((name (substring file 0 (match-beginning 0))))
            (when (and (string-prefix-p "ob-" name)
                       (not
                        (or (string-suffix-p "-autoloads" name)
                            (string-suffix-p "-mode" name))))
              (push (substring name 3) files))))))
    files))


(defun flymake-org--get-eval-sexps ()
  "Generate a list of evaluation expressions for Org mode setup."
  (append
   `(progn
      (setq-default org-element-cache-persistent nil
       org-element-use-cache nil))
   (flymake-org--get-hook-features-sexps)
   `((org-babel-do-load-languages 'org-babel-load-languages
      ',org-babel-load-languages)
     (require 'org)
     (print org-mode-hook))))

(defun flymake-org-get-eval-string ()
  "Generate string to load and hook `org-mode' features."
  (prin1-to-string (flymake-org--get-eval-sexps)))

(defun flymake-org-cleanup-tmp-dirs (start-time)
  "Delete empty temporary directories created after START-TIME.

Argument START-TIME is the time before which temporary directories should not be
deleted."
  (let ((default-directory (temporary-file-directory)))
    (dolist
        (short-dir (directory-files default-directory nil "\\`org-persist-"))
      (let ((dir (file-name-as-directory (expand-file-name short-dir
                                                           default-directory))))
        (when (and
               (file-directory-p dir)
               (directory-empty-p dir))
          (let* ((created-time (file-attribute-modification-time
                                (file-attributes dir)))
                 (is-start-time-less (time-less-p start-time created-time)))
            (when is-start-time-less
              (delete-directory dir))))))))


(defun flymake-org-flymake-report (report-fn &rest _args)
  "Report linting results for Org files using Flymake.

Optional argument REPORT-FN is a function to call with the linting results.

Remaining arguments _ARGS are ignored and not used within the function."
  (when flymake-org--flymake-process
    (when-let* ((start-time
                 (process-get flymake-org--flymake-process 'start-time)))
      (flymake-org-cleanup-tmp-dirs start-time))
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
      (setq flymake-org-directory
            (file-name-directory (flymake-org-feature-file 'org))))
    (let* ((output-buffer (generate-new-buffer " *flymake-org*")))
      (setq flymake-org--flymake-process
            (make-process
             :name "flymake-org"
             :buffer output-buffer
             :command (delq nil
                            `(,(expand-file-name invocation-name
                                invocation-directory)
                              "-Q"
                              "--batch"
                              ,@(mapcan (lambda (path)
                                          (list "-L" path))
                                 (append (list "./")
                                  load-path))
                              "--eval" ,(flymake-org-get-eval-string)
                              "-l" ,flymake-org-load-filename
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
                   (ignore-errors
                     (delete-file temp-file))
                   (kill-buffer output-buffer))))
             :stderr " *stderr of flymake-org"
             :noquery t))
      (process-put flymake-org--flymake-process 'start-time (current-time))
      flymake-org--flymake-process)))

;;;###autoload
(defun flymake-org-collect (&optional file)
  "Collect linting errors from an Org file.

Optional argument FILE is the name of the file to be checked. If not provided,
the current buffer's FILE name is used."
  (interactive (list buffer-file-name))
  (let* ((file (or file (car command-line-args-left)))
         (result))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (require 'org)
        (require 'org-lint)
        (require 'ob)
        (org-mode)
        (run-hooks 'org-mode-hook)
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
                                 (category-prefix (if categories
                                                      (format "%s " categories)
                                                    "")))
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
(defun flymake-org-in-file ()
  "Enable Flymake for Org linting if the current buffer is associated with a file."
  (interactive)
  (when buffer-file-name
    (flymake-org-on)))

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