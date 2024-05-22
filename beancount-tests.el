;;; beancount-test.el --- ERT for beancount-mode -*- lexical-binding: t -*-

;; Copyright 2019 Daniele Nicolodi <daniele@grinta.net>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.


(require 'ert)
(require 'eieio)
(require 'beancount)
(require 'imenu)

(ert-deftest beancount/smoke-001 ()
  :tags '(regress)
  (with-temp-buffer
    (beancount-mode)
    (font-lock-ensure)))

(ert-deftest beancount/number-regexp-001 ()
  :tags '(regexp regress)
  (let ((r (concat "\\`" beancount-number-regexp "\\'")))
    (should (string-match r "1"))
    (should (string-match r "1."))
    (should (string-match r "1.0"))
    (should (string-match r "10000.00"))
    (should (string-match r "1,234.56"))
    (should (string-match r "10,000.00"))
    (should (string-match r "1,000,000.00"))
    (should-not (string-match r "1.00.00"))
    (should-not (string-match r "1,00.00"))
    (should-not (string-match r ",000.00"))
    (should-not (string-match r ".00"))
    (should-not (string-match r "."))
    (should-not (string-match r ","))))

(defun beancount-test-fontify-string (string)
  "Fontify STRING in beancount-mode."
  (with-temp-buffer
    (insert string)
    (beancount-mode)
    (font-lock-ensure)
    (buffer-string)))

(defun beancount-test-face-groups (fontified)
  "Group a fontified string by face.
Return a list of substrings each followed by its face."
  (cl-loop for start = 0 then end
           while start
           for end   = (next-single-property-change start 'face fontified)
           for prop  = (get-text-property start 'face fontified)
           for text  = (substring-no-properties fontified start end)
           if prop
           append (list text prop)))

(defun beancount-test-group-str-by-face (str)
  "Fontify `str' in beancount-mode and group it by face.
Return a list of substrings each followed by its face."
  (beancount-test-face-groups (beancount-test-fontify-string str)))

(defun beancount-test-font-lock (source face-groups)
  "Test that `source' fontifies to the expected `face-groups'."
  (should (equal (beancount-test-group-str-by-face source) face-groups)))

(ert-deftest beancount/fontify-001 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 * \"Example\"
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "*"                beancount-narrative-cleared
     "\"Example\""      beancount-narrative-cleared
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-002 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 ! \"Example\"
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "!"                beancount-narrative-pending
     "\"Example\""      beancount-narrative-pending
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-003 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 A \"Example\"
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "A"                beancount-narrative
     "\"Example\""      beancount-narrative
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-004 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 * \"Example\"
  #foo
  ^bar
  Expenses:Example  1.00 USD
  Assets:Checking
"
   '("2019-01-01"       beancount-date
     "*"                beancount-narrative-cleared
     "\"Example\""      beancount-narrative-cleared
     "#foo"             beancount-tag
     "^bar"             beancount-link
     "Expenses:Example" beancount-account
     "1.00 USD"         beancount-amount
     "Assets:Checking"  beancount-account)))

(ert-deftest beancount/fontify-005 ()
  :tags '(font regress)
  (beancount-test-font-lock "
2019-01-01 open Assets:TD:TDB900 TDB900
"
   '("2019-01-01"       beancount-date
     "open"             beancount-directive
     "Assets:TD:TDB900" beancount-account)))

(ert-deftest beancount/indent-001 ()
  :tags '(indent regress)
  (with-temp-buffer
    (insert "
2019-01-01 * \"Example\"
  #foo
    ^bar
  Expenses:Example  1.00 USD
    Assets:Checking           1.00 USD
")
    (beancount-mode)
    (forward-line -1)
    (beancount-indent-transaction)
    (should (equal (buffer-string) "
2019-01-01 * \"Example\"
  #foo
  ^bar
  Expenses:Example                              1.00 USD
  Assets:Checking                               1.00 USD
"))))

(ert-deftest beancount/options-001 ()
  "Verify that beancount-mode recognises all options implemented
in beancount. Use the output of bean-doctor to get a list of
known option nmaes."
  :tags '(options)
  (let (options)
    (with-temp-buffer
      (shell-command "bean-doctor list_options" t)
      (goto-char (point-min))
      (while (re-search-forward "^option\\s-+\"\\([a-z_]*\\)\"" nil t)
        (setq options (cons (match-string-no-properties 1) options))))
    (should (equal (sort options #'string<) beancount-option-names))))

(ert-deftest beancount/completion-001 ()
  :tags '(completion regress)
  (with-temp-buffer
    (insert "
2019-01-01 * \"Example\"
  Expenses:Test    1.00 USD
  Assets:Checking

2019-01-01 * \"Example\"
  Expenses:T
")
    (beancount-mode)
    (forward-line -1)
    (move-end-of-line 1)
    (completion-at-point)
    (should (equal (buffer-string) "
2019-01-01 * \"Example\"
  Expenses:Test    1.00 USD
  Assets:Checking

2019-01-01 * \"Example\"
  Expenses:Test
"))
    (should (equal beancount-accounts '("Assets:Checking" "Expenses:Test")))))

(ert-deftest beancount/outline-001 ()
  :tags '(outline)
  (with-temp-buffer
    (insert "
* A
** B
*** C
")
    (beancount-mode)
    (outline-minor-mode)
    (forward-line -1)
    (should (looking-at beancount-outline-regexp))
    (should (equal (beancount-outline-level) 3))))

(ert-deftest beancount/outline-002 ()
  :tags '(outline)
  (with-temp-buffer
    (insert "
;;; A
;;;; B
;;;;; C
")
    (beancount-mode)
    (outline-minor-mode)
    (forward-line -1)
    (should (looking-at beancount-outline-regexp))
    (should (equal (beancount-outline-level) 3))))

(ert-deftest beancount/outline-fontify-001 ()
  :tags '(outline)
  (let ((fontified
         (with-temp-buffer
           (insert "
* A
** B
*** C
")
           (beancount-mode)
           (outline-minor-mode)
           (font-lock-ensure)
           (buffer-string))))
    (should (equal (beancount-test-face-groups fontified)
                   '("* A"   beancount-outline-1
                     "** B"  beancount-outline-2
                     "*** C" beancount-outline-3)))))

(ert-deftest beancount/account-currency-001 ()
  :tags '(regress)
  (with-temp-buffer
    (insert "
2019-12-22 open Assets:Test:One USD
2019-12-22 open Assets:Test:Two USD,EUR
2019-12-22 open Assets:Test:Three USD \"STRICT\"
2019-12-22 open Assets:Test:Four USD,EUR \"STRICT\"
2019-12-22 open Assets:Test:Five \"STRICT\"
")
    (should (equal (beancount--account-currency "Assets:Test:One") "USD"))
    (should (equal (beancount--account-currency "Assets:Test:Two") nil))
    (should (equal (beancount--account-currency "Assets:Test:Three") "USD"))
    (should (equal (beancount--account-currency "Assets:Test:Four") nil))
    (should (equal (beancount--account-currency "Assets:Test:Five") nil))))

(ert-deftest beancount/imenu-001 ()
  :tags '(regress imenu)
  (with-temp-buffer
    (insert "
;;; 2019
;;;; 2019 January
;;;; 2019 February
;;; 2020
;;;; 2020 January

2020-01-01 * \"Example\"
  Expenses:Test    1.00 USD
  Assets:Checking

;;;; 2020 February
")
    (beancount-mode)
    (outline-minor-mode)
    (let* ((imenu-use-markers nil) ; makes testing easier
           (index (funcall imenu-create-index-function)))
      (should (equal index '(("2019" . 2)
                             ("2019 January" . 11)
                             ("2019 February" . 29)
                             ("2020" . 48)
                             ("2020 January" . 57)
                             ("2020 February" . 146)))))))

(ert-deftest beancount/imenu-002 ()
  :tags '(regress imenu)
  (with-temp-buffer
    (insert "
* 2019
** 2019 January

2019-01-01 * \"Example\"
  Expenses:Test    1.00 USD
  Assets:Checking

** 2019 February
* 2020
** 2020 January
** 2020 February
")
    (beancount-mode)
    (outline-minor-mode)
    (let* ((imenu-use-markers nil) ; makes testing easier
           (index (funcall imenu-create-index-function)))
      (should (equal index '(("2019" . 2)
                             ("2019 January" . 9)
                             ("2019 February" . 96)
                             ("2020" . 113)
                             ("2020 January" . 120)
                             ("2020 February" . 136)))))))

(ert-deftest beancount/link-at-point-001 ()
  :tags '(regress thing-at-point)
  (with-temp-buffer
    (insert "^link")
    (should (equal (thing-at-point 'beancount-link) "^link"))))

(ert-deftest beancount/link-at-point-002 ()
  :tags '(regress thing-at-point)
  (with-temp-buffer
    (insert "not-a-link")
    (should (equal (thing-at-point 'beancount-link) nil))))

(ert-deftest beancount/link-at-point-003 ()
  :tags '(regress thing-at-point)
  (with-temp-buffer
    (insert "foo ^link baz")
    (goto-char 7)
    (should (equal (thing-at-point 'beancount-link) "^link"))))

;;; Date shifting

(ert-deftest beancount/date-shift-up-day ()
  :tags '(date-shift)
  (with-temp-buffer
    (insert "2024-05-11\n")
    (goto-char 0)
    (beancount-date-up-day)
    (should (equal (thing-at-point 'line) "2024-05-12\n"))))

(ert-deftest beancount/date-shift-down-day ()
  :tags '(date-shift)
  (with-temp-buffer
    (insert "2024-05-11\n")
    (goto-char 0)
    (beancount-date-down-day)
    (should (equal (thing-at-point 'line) "2024-05-10\n"))))

;;; Xref backend

(defun beancount-test-xref-definition-pos (identifier position)
  "Check if IDENTIFIER's position is the same is the same as
POSITION provided by Beancount's xref-backend-definitions lookup."
  (let ((defs (xref-backend-definitions 'beancount identifier)))
    (should (equal (length defs) 1))
    (let* ((def (car (xref-backend-definitions 'beancount identifier)))
           (loc (xref-item-location def))
           ;; Pre Emacs-28.1, defclass was used for
           ;; xref-buffer-location.
           (pos (if (version< emacs-version "28.1")
                    (oref loc position)
                  (xref-buffer-location-position loc))))
      (should (equal pos position)))))

(ert-deftest beancount/xref-backend-definitions ()
  :tags '(xref)
  (with-temp-buffer
    (insert "
2019-01-01 open Assets:Account1 TDB900
2019-01-01 open Assets:Account2 TDB900
2019-01-01 open Assets:Account3 TDB900

2019-01-10  * \"Opening Balances\"
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
")
    (beancount-test-xref-definition-pos "Assets:Account1" 2)
    (beancount-test-xref-definition-pos "Assets:Account2" 41)
    (beancount-test-xref-definition-pos "Assets:Account3" 80)))

(defmacro beancount-with-temp-file (&rest body)
  "Generate a temporary file and open it as a current buffer.
Run BODY forms in the buffer's context. Remove both the buffer
and a backing file having completed the test."
  (declare (indent 1))
  `(let ((file (make-temp-file "beancount-test-"))
        buf)
    (unwind-protect
        (progn (setq buf (find-file-literally file))
               ,@body)
      (ignore-errors (delete-file file))
      (ignore-errors
        (with-current-buffer buf
          (set-buffer-modified-p nil))
        (kill-buffer buf)))))

(ert-deftest beancount/xref-backend-references ()
  :tags '(xref)
  ;; Creating Xref file locations assumes a buffer backed by a file.
  (beancount-with-temp-file
      (insert "
2019-01-01 open Assets:Account1 TDB900
2019-01-01 open Assets:Account2 TDB900
2019-01-01 open Assets:Account3 TDB900

2019-01-10  * \"Opening Balances\"
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
  Assets:Account2        2.00 TDB900

2019-01-10  * \"More Balances\"
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

")
    (should (equal (length (xref-backend-references 'beancount "Assets:Account1")) 3))
    (should (equal (length (xref-backend-references 'beancount "Assets:Account2")) 2))
    (should (equal (length (xref-backend-references 'beancount "Assets:Account3")) 1))))

(ert-deftest beancount/xref-backend-apropos ()
  :tags '(xref)
  ;; Creating Xref file locations assumes a buffer backed by a file.
  (beancount-with-temp-file
      (insert "
2019-01-01 open Assets:Account1 TDB900
2019-01-01 open Assets:Account2 TDB900
2019-01-01 open Assets:Account3 TDB900

2019-01-10  * \"Opening Balances\"
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
  Assets:Account2        2.00 TDB900

2019-01-10  * \"More Balances\"
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

")
    (should (equal (length (xref-backend-apropos 'beancount "Assets")) 6))
    (should (equal (length (xref-backend-apropos 'beancount "Assets Account")) 6))
    (should (equal (length (xref-backend-apropos 'beancount "Assets Account1")) 3))
    (should (equal (length (xref-backend-apropos 'beancount "Equity")) 2))
    (should (equal (length (xref-backend-apropos 'beancount "Opening")) 2))
    (should (equal (length (xref-backend-apropos 'beancount "Opening Assets")) 0))))
