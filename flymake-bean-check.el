;;; flymake-bean-check.el --- A Flymake backend for bean-check -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages tools
;; URL: https://github.com/akirak/flymake-bean-check

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a flymake backend for bean-check
;; <https://beancount.github.io/docs/running_beancount_and_generating_reports.html#bean-check>.

;; To use this library, run `flymake-bean-check-enable' in the file buffer of a
;; beancount journal. A recommended way is to add the function to
;; `beancount-mode-hook'.

;;; Code:

(defgroup flymake-bean-check nil
  "A Flymake backend for bean-check."
  :group 'flymake
  :group 'beancount-mode)

(defconst flymake-bean-check-location-regexp
  (rx bol (not (any blank)) (*? anything)
      ":" (group (+ digit)) ":"
      (+ blank)
      (group (+ nonl))))

(defcustom flymake-bean-check-executable "bean-check"
  "Executable file of bean-check."
  :type 'file)

(defvar-local flymake-bean-check-process nil)

(defun flymake-bean-check-cache-filename (file)
  "Create the bean-check cache file name for FILE."
  (let* ((backup-directory-alist (list (cons ".*" temporary-file-directory)))
	 (name (make-backup-file-name-1 file)))
    (concat name ".~flymake-bean-check~")))

;;;###autoload
(defun flymake-bean-check-enable ()
  "Enable checking via flymake."
  (interactive nil beancount-mode)
  (when (buffer-file-name)
    (flymake-mode t)
    (add-hook 'flymake-diagnostic-functions 'flymake-bean-check--run nil t)))

(defun flymake-bean-check--run (report-fn &rest _ignored)
  "Perform checking.

This function should be added to `flymake-diagnostic-functions'
in the enabled buffer. REPORT-FN is a function."
  (unless (executable-find flymake-bean-check-executable)
    (error "The executable %s doesn't exist. See `flymake-bean-check-executable'"
           flymake-bean-check-executable))
  (when (and flymake-bean-check-process
             (process-live-p flymake-bean-check-process))
    (kill-process flymake-bean-check-process))
  (let* ((source (current-buffer))
         (buffer (generate-new-buffer "*flymake-bean-check*"))
         (cache-file (flymake-bean-check-cache-filename (buffer-file-name))))
    (setq flymake-bean-check-process
          (make-process :buffer buffer
                        :name "flymake-bean-check"
                        :noquery t
                        :connection-type 'pipe
                        :command (list flymake-bean-check-executable
                                       "/dev/stdin"
                                       "--cache-filename" cache-file)
                        :sentinel
                        (lambda (proc _event)
                          (when (memq (process-status proc) '(exit signal))
                            (unwind-protect
                                (with-current-buffer buffer
                                  (goto-char (point-min))
                                  (let (result)
                                    (while (re-search-forward flymake-bean-check-location-regexp
                                                              nil t)
                                      (pcase-let*
                                          ((message (match-string 2))
                                           (`(,begin . ,end) (flymake-diag-region
                                                              source
                                                              (string-to-number (match-string 1)))))
                                        (push (flymake-make-diagnostic source begin end
                                                                       :error message)
                                              result)))
                                    (funcall report-fn (nreverse result))))
                              (kill-buffer buffer))))))
    (process-send-region flymake-bean-check-process (point-min) (point-max))
    (process-send-eof flymake-bean-check-process)))

(provide 'flymake-bean-check)
;;; flymake-bean-check.el ends here
