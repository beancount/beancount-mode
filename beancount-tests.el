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

;; Account-type customization

(defun beancount-set-account-categories (assets liabilities equity income expenses)
  "Set account defcustoms and trigger `:set' rebuilds via
`customize-set-variable'."
  (customize-set-variable 'beancount-assets assets)
  (customize-set-variable 'beancount-liabilities liabilities)
  (customize-set-variable 'beancount-equity equity)
  (customize-set-variable 'beancount-income income)
  (customize-set-variable 'beancount-expenses expenses))

(defun beancount-reset-account-categories ()
  "Reset account defcustoms to `:init-value' and trigger `:set' rebuilds via
`custom-reevaluate-setting'."
  (custom-reevaluate-setting 'beancount-assets)
  (custom-reevaluate-setting 'beancount-liabilities)
  (custom-reevaluate-setting 'beancount-equity)
  (custom-reevaluate-setting 'beancount-income)
  (custom-reevaluate-setting 'beancount-expenses))

(defmacro beancount-with-custom-accounts (accounts &rest body)
  "Set account categories to CATEGORIES, run BODY, then reset to default values.
ACCOUNTS = (assets liabilities equity income expenses)"
  (declare (indent 1))
  `(unwind-protect
       (progn
         (beancount-set-account-categories ,@accounts)
         ,@body)
     (beancount-reset-account-categories)))

(ert-deftest beancount/account-categories-defaults ()
  "Test that beancount-account-categories uses default values."
  :tags '(account customization regress)
  (should (equal beancount-account-categories
                 '("Assets" "Liabilities" "Equity" "Income" "Expenses"))))

(ert-deftest beancount/account-categories-custom ()
  "Test that beancount-account-categories respects custom variable values."
  :tags '(account customization regress)
  (beancount-with-custom-accounts ("Actif" "Passività" "Собственный-капитал" "Ingresos" "Koszty")
    (should (equal beancount-account-categories
                   '("Actif" "Passività" "Собственный-капитал" "Ingresos" "Koszty")))))

(ert-deftest beancount/account-customization-regexps ()
  :tags '(account customization regexp regress)
  (beancount-with-custom-accounts ("Actif" "Passività" "Собственный-капитал" "Ingresos" "Koszty")
    (should (string-match beancount-account-regexp
                          "Actif:Foo"))
    (should (string-match beancount-posting-regexp
                          "  Passività:Foo 1.00 EUR"))
    (should (string-match beancount-balance-regexp
                          "2026-01-01 balance Собственный-капитал:Foo 100.00 RUB"))
    (should (string-match beancount-open-directive-regexp
                          "2026-01-01 open Ingresos:Foo"))
    (should (string-match beancount-xref-symbol-regexp
                          "Koszty:Bar"))))

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

(ert-deftest beancount/completion-custom-accounts-french ()
  "Verify beancount-mode auto-completes accounts with French root names."
  :tags '(completion customization regress)
  (beancount-with-custom-accounts ("Actif" "Passif" "Capitaux-propres" "Produits" "Charges")
    (with-temp-buffer
      (insert "
2026-01-01 * \"Example\"
  Charges:Test    1.00 EUR
  Actif:Checking

2026-01-01 * \"Example\"
  Charges:T
")
      (beancount-mode)
      (forward-line -1)
      (move-end-of-line 1)
      (completion-at-point)
      (should (equal (buffer-string) "
2026-01-01 * \"Example\"
  Charges:Test    1.00 EUR
  Actif:Checking

2026-01-01 * \"Example\"
  Charges:Test
"))
      (should (equal beancount-accounts '("Actif:Checking" "Charges:Test"))))))

(ert-deftest beancount/completion-custom-accounts-italian ()
  "Verify beancount-mode auto-completes accounts with Italian root names."
  :tags '(completion customization regress)
  (beancount-with-custom-accounts ("Attività" "Passività" "Patrimonio-netto" "Ricavi" "Costi")
    (with-temp-buffer
      (insert "
2026-01-01 * \"Example\"
  Costi:Test    1.00 EUR
  Attività:Checking

2026-01-01 * \"Example\"
  Costi:T
")
      (beancount-mode)
      (forward-line -1)
      (move-end-of-line 1)
      (completion-at-point)
      (should (equal (buffer-string) "
2026-01-01 * \"Example\"
  Costi:Test    1.00 EUR
  Attività:Checking

2026-01-01 * \"Example\"
  Costi:Test
"))
      (should (equal beancount-accounts '("Attività:Checking" "Costi:Test"))))))

(ert-deftest beancount/completion-custom-accounts-russian ()
  "Verify beancount-mode auto-completes accounts with Russian root names."
  :tags '(completion customization regress)
  (beancount-with-custom-accounts ("Активы" "Обязательства" "Собственный-капитал" "Доходы" "Расходы")
    (with-temp-buffer
      (insert "
2026-01-01 * \"Example\"
  Расходы:Test    1.00 RUB
  Активы:Checking

2026-01-01 * \"Example\"
  Расходы:T
")
      (beancount-mode)
      (forward-line -1)
      (move-end-of-line 1)
      (completion-at-point)
      (should (equal (buffer-string) "
2026-01-01 * \"Example\"
  Расходы:Test    1.00 RUB
  Активы:Checking

2026-01-01 * \"Example\"
  Расходы:Test
"))
      (should (equal beancount-accounts '("Активы:Checking" "Расходы:Test"))))))

(ert-deftest beancount/completion-custom-accounts-spanish ()
  "Verify beancount-mode auto-completes accounts with Spanish root names."
  :tags '(completion customization regress)
  (beancount-with-custom-accounts ("Activos" "Pasivos" "Patrimonio-neto" "Ingresos" "Gastos")
    (with-temp-buffer
      (insert "
2026-01-01 * \"Example\"
  Gastos:Test    1.00 EUR
  Activos:Checking

2026-01-01 * \"Example\"
  Gastos:T
")
      (beancount-mode)
      (forward-line -1)
      (move-end-of-line 1)
      (completion-at-point)
      (should (equal (buffer-string) "
2026-01-01 * \"Example\"
  Gastos:Test    1.00 EUR
  Activos:Checking

2026-01-01 * \"Example\"
  Gastos:Test
"))
      (should (equal beancount-accounts '("Activos:Checking" "Gastos:Test"))))))

(ert-deftest beancount/completion-custom-accounts-polish ()
  "Verify beancount-mode auto-completes accounts with Polish root names."
  :tags '(completion customization regress)
  (beancount-with-custom-accounts ("Aktywa" "Pasywa" "Kapitał-własny" "Przychody" "Koszty")
    (with-temp-buffer
      (insert "
2026-01-01 * \"Example\"
  Koszty:Test    1.00 PLN
  Aktywa:Checking

2026-01-01 * \"Example\"
  Koszty:T
")
      (beancount-mode)
      (forward-line -1)
      (move-end-of-line 1)
      (completion-at-point)
      (should (equal (buffer-string) "
2026-01-01 * \"Example\"
  Koszty:Test    1.00 PLN
  Aktywa:Checking

2026-01-01 * \"Example\"
  Koszty:Test
"))
      (should (equal beancount-accounts '("Aktywa:Checking" "Koszty:Test"))))))

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
           (pos (if (< emacs-major-version 28)
                    (oref loc position)
                  (xref-buffer-location-position loc))))
      (should (equal pos position)))))

(ert-deftest beancount/xref-backend-definitions-accounts ()
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

(ert-deftest beancount/xref-backend-definitions-tags ()
  :tags '(xref)
  (with-temp-buffer
    (insert "
2019-01-10  * \"Opening Balances\" #tag1
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

2019-01-10  * \"Opening Balances\" #tag2
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
")
    (beancount-test-xref-definition-pos "#tag1" 35)
    (beancount-test-xref-definition-pos "#tag2" 138)))

(ert-deftest beancount/xref-backend-definitions-links ()
  :tags '(xref)
  (with-temp-buffer
    (insert "
2019-01-10  * \"Opening Balances\" #link1
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

2019-01-10  * \"Opening Balances\" #link2
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
")
    (beancount-test-xref-definition-pos "#link1" 35)
    (beancount-test-xref-definition-pos "#link2" 139)))


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

(ert-deftest beancount/xref-backend-references-accounts ()
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

(ert-deftest beancount/xref-backend-references-tags ()
  :tags '(xref)
  ;; Creating Xref file locations assumes a buffer backed by a file.
  (beancount-with-temp-file
      (insert "
2019-01-10  * \"More Balances\" #tag1
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

2019-01-10  * \"Opening Balances\" #tag2
  Assets:Account1        1.00 TDB900
  Assets:Account2        2.00 TDB900

2019-01-10  * \"More Balances\" #tag2
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
")
    (should (equal (length (xref-backend-references 'beancount "#tag1")) 1))
    (should (equal (length (xref-backend-references 'beancount "#tag2")) 2))))

(ert-deftest beancount/xref-backend-references-links ()
  :tags '(xref)
  ;; Creating Xref file locations assumes a buffer backed by a file.
  (beancount-with-temp-file
      (insert "
2019-01-10  * \"More Balances\" ^link1
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

2019-01-10  * \"Opening Balances\" ^link1
  Assets:Account1        1.00 TDB900
  Assets:Account2        2.00 TDB900

2019-01-10  * \"More Balances\" ^link2
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
")
    (should (equal (length (xref-backend-references 'beancount "^link1")) 2))
    (should (equal (length (xref-backend-references 'beancount "^link2")) 1))))

(ert-deftest beancount/xref-backend-apropos ()
  :tags '(xref)
  ;; Creating Xref file locations assumes a buffer backed by a file.
  (beancount-with-temp-file
      (insert "
2019-01-01 open Assets:Account1 TDB900
2019-01-01 open Assets:Account2 TDB900
2019-01-01 open Assets:Account3 TDB900

2019-01-10  * \"Opening Balances\" #tag ^link1
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900
  Assets:Account2        2.00 TDB900

2019-01-10  * \"More Balances\" #tag ^link2
  Equity:Opening-Balances
  Assets:Account1        1.00 TDB900

")
    (should (equal (length (xref-backend-apropos 'beancount "Assets")) 6))
    (should (equal (length (xref-backend-apropos 'beancount "Assets Account")) 6))
    (should (equal (length (xref-backend-apropos 'beancount "Assets Account1")) 3))
    (should (equal (length (xref-backend-apropos 'beancount "Equity")) 2))
    (should (equal (length (xref-backend-apropos 'beancount "Opening")) 2))
    (should (equal (length (xref-backend-apropos 'beancount "Opening Assets")) 0))
    (should (equal (length (xref-backend-apropos 'beancount "tag")) 2))
    (should (equal (length (xref-backend-apropos 'beancount "link1")) 1))
    (should (equal (length (xref-backend-apropos 'beancount "link2")) 1))))

;;; Main file

(defconst beancount-test-main-file-dir-locals
  "((beancount-mode . ((beancount-main-file . \"main.beancount\"))))")

(defmacro beancount-with-ledger (dir-locals &rest body)
  "Run BODY in a buffer visiting a file included by a temporary ledger.
The ledger's main file is main.beancount, which includes
sub/2024.beancount; BODY runs in the buffer visiting the latter.  When
DIR-LOCALS is non-nil, it is written to the ledger's .dir-locals.el
file.  ROOT is bound to the ledger directory."
  (declare (indent 1) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "beancount-test-" t)))
          (enable-local-variables :all)
          buf)
     (unwind-protect
         (progn
           (make-directory (expand-file-name "sub" root))
           (with-temp-file (expand-file-name "main.beancount" root)
             (insert "include \"sub/2024.beancount\"\n"))
           (with-temp-file (expand-file-name "sub/2024.beancount" root)
             (insert "2024-01-01 open Assets:Cash\n"
                     "2024-01-02 * \"Shop\"\n"
                     "  Assets:Cash  -5 USD\n"
                     "  Expenses:Food\n"))
           (when ,dir-locals
             (with-temp-file (expand-file-name ".dir-locals.el" root)
               (insert ,dir-locals)))
           (setq buf (find-file-noselect (expand-file-name "sub/2024.beancount" root)))
           (with-current-buffer buf
             ,@body))
       (when (buffer-live-p buf)
         (kill-buffer buf))
       (delete-directory root t))))

(defun beancount-test-compile-command (command &rest args)
  "Return the command line that COMMAND, called with ARGS, passes to `compile'."
  (let (compiled)
    (cl-letf (((symbol-function 'compile)
               (lambda () (interactive) (setq compiled compile-command))))
      (apply command args))
    compiled))

(ert-deftest beancount/main-file-unset ()
  :tags '(main-file)
  (beancount-with-ledger nil
    (should (equal (beancount-main-file-name) buffer-file-name))
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (beancount-test-compile-command #'beancount-context)
                   "bean-doctor context 2024.beancount 3"))
    ;; A file that does not exist yet still gets a bare line number.
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "new.beancount" root))
      (should (equal (beancount--location 3) "3")))))

(ert-deftest beancount/main-file-relative-name ()
  :tags '(main-file)
  (beancount-with-ledger beancount-test-main-file-dir-locals
    ;; A value from .dir-locals.el is relative to its directory.
    (should (equal (beancount-main-file-name) (expand-file-name "main.beancount" root)))
    ;; A file-local value is relative to the file.
    (let ((file (expand-file-name "sub/2025.beancount" root)))
      (with-temp-file file
        (insert "; Local Variables:\n"
                "; beancount-main-file: \"../main.beancount\"\n"
                "; End:\n"))
      (with-current-buffer (find-file-noselect file)
        (unwind-protect
            (should (equal (beancount-main-file-name)
                           (expand-file-name "main.beancount" root)))
          (kill-buffer)))))
  (beancount-with-ledger nil
    ;; So is a value set in the buffer.
    (setq-local beancount-main-file "../main.beancount")
    (should (equal (beancount-main-file-name) (expand-file-name "main.beancount" root)))))

(ert-deftest beancount/main-file-commands ()
  :tags '(main-file)
  (beancount-with-ledger beancount-test-main-file-dir-locals
    (should (equal (beancount-test-compile-command #'beancount-check)
                   "bean-check ../main.beancount"))
    (goto-char (point-min))
    (forward-line 2)
    (should (equal (beancount-test-compile-command #'beancount-context)
                   "bean-doctor context ../main.beancount 2024.beancount:3"))
    (should (equal (beancount-test-compile-command #'beancount-linked)
                   "bean-doctor linked ../main.beancount 2024.beancount:3"))
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (forward-line 1)
      (push-mark (point) t t)
      (goto-char (point-max))
      (should (equal (beancount-test-compile-command
                      #'beancount-region-default (region-beginning) (region-end))
                     "bean-doctor region ../main.beancount 2024.beancount:2:4")))
    ;; In the main file itself, locations are bare line numbers.
    (with-current-buffer (find-file-noselect (expand-file-name "main.beancount" root))
      (unwind-protect
          (should (equal (beancount-test-compile-command #'beancount-context)
                         "bean-doctor context main.beancount 1"))
        (kill-buffer)))))
