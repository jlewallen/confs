;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq jacob-finance-packages
      '(
        company
        ledger-mode
        ))

(defun jacob-ledger-find-accounts-in-buffer ()
  (setq pcomplete-args '(""))
  (ledger-find-accounts-in-buffer)
  )

(defun jacob-strip-booleans (tree)
  (if (atom tree)
      tree
    (mapcar (lambda (node) (jacob-strip-booleans node))
            (remove t tree))))

(defun jacob-expand-account-paths (tree)
  (cond
   ((and (eq 1 (length tree)) (atom (car tree))) tree)
   ((and (eq 1 (length tree)) t) (jacob-expand-account-paths (car tree)))
   ((atom (car tree))
    (mapcar (lambda (node) (concat (car tree) ":" node)) (jacob-expand-account-paths (cdr tree)))
    )
   (t (apply 'append
             (mapcar (lambda (node) (jacob-expand-account-paths node)) tree)
             ))))

(defun jacob-ledger-accounts ()
  (jacob-expand-account-paths (jacob-strip-booleans (jacob-ledger-find-accounts-in-buffer)))
  )

(defun jacob-finance/customize ()
  (defvar ledger-report-balance
    (list "bal" (concat "hledger" " -f %(ledger-file) bal")))

  (defvar ledger-report-reg
    (list "reg" (concat "hledger" " -f %(ledger-file) reg")))

  (defvar ledger-report-account
    (list "account" (concat "hledger" " -f %(ledger-file) reg %(account)")))

  (setq ledger-reports
        (list ledger-report-balance
              ledger-report-reg
              ledger-report-account))
  )

(defun ledger-verify ()
  (interactive)
  (let* ((buffer (current-buffer))
         (balance (with-temp-buffer
                    (ledger-exec-ledger buffer (current-buffer) "--pedantic" "stats")
                    (buffer-substring-no-properties (point-min) (1- (point-max))))))
    (when balance
      (message balance))))

(defun ledger-add-food (title in amount out)
  (interactive
   (let ((accounts (mapcar 'list (jacob-ledger-accounts))))
     (list (read-string "Entry: " (format-time-string "%Y-%m-%d " (current-time)))
           (let ((completion-regexp-list '("^expenses:")))
             (completing-read "What did you pay for? " accounts))
           (read-string "How much did you pay? " " ")
           (let ((completion-regexp-list '("^\\(assets\\|liabilities\\):")))
             (completing-read "Where did the money come from? " accounts)))))
  (message "%s" (type-of amount))
  (insert title)
  (newline)
  (indent-to 4)
  (insert in "  " amount)
  (newline)
  (indent-to 4)
  (insert out "  " (number-to-string (- (string-to-number amount)))))

(defun jacob-finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (jacob-finance/customize)
      (setq ledger-post-amount-alignment-column 62)
      (push 'company-capf company-backends-ledger-mode)
      (spacemacs/set-leader-keys-for-major-mode 'ledger-mode
        "hd" 'ledger-delete-current-transaction
        "a" 'ledger-add-transaction
        "b" 'ledger-post-edit-amount
        "c" 'ledger-toggle-current
        "C" 'ledger-mode-clean-buffer
        "l" 'ledger-display-ledger-stats
        "p" 'ledger-display-balance-at-point
        "q" 'ledger-post-align-xact
        "r" 'ledger-reconcile
        "R" 'ledger-report
        "t" 'ledger-insert-effective-date
        "y" 'ledger-set-year
        "v" 'ledger-verify
        "f" 'ledger-add-food
        "RET" 'ledger-set-month)
      ;; temporary hack to work-around an issue with evil-define-key
      ;; more info: https://bitbucket.org/lyro/evil/issues/301/evil-define-key-for-minor-mode-does-not
      ;; TODO remove this hack if the limitation is removed upstream
      (add-hook 'ledger-mode-hook 'evil-normalize-keymaps)
      (evilified-state-evilify ledger-report-mode ledger-report-mode-map))))

(defun jacob-finance/post-init-company ()
  (spacemacs|add-company-hook ledger-mode))
