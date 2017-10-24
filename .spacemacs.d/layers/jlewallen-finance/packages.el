;;; packages.el --- Finance Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq jlewallen-finance-packages
  '(
    company
    (flycheck-ledger :requires flycheck)
    ledger-mode
    ))

(defun jlewallen-finance/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes ledger-mode))

(defun jlewallen-finance/init-flycheck-ledger ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger)))

(defun jlewallen-ledger-add-food (title in amount out)
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

(defun jlewallen-ledger-duplicate-current-transaction (date)
  "Ask for a new DATE and copy the transaction under point to that date.  Leave point on the first amount.  Toggle if cleared."
  (interactive  (list
                 (ledger-read-date "Copy to date: ")))
  (let* ()
    (ledger-copy-transaction-at-point date)
    (if (eq (ledger-transaction-state) 'cleared)
        (ledger-toggle-current-transaction)
        )
    )
  )

(defun jlewallen-finance/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
      (spacemacs/set-leader-keys-for-major-mode 'ledger-mode
         "hd" 'ledger-delete-current-transaction
         "a" 'ledger-add-transaction
         "b" 'ledger-post-edit-amount
         "c" 'ledger-toggle-current
         "D" 'jlewallen-ledger-duplicate-current-transaction
         "C" 'ledger-mode-clean-buffer
         "l" 'ledger-display-ledger-stats
         "p" 'ledger-display-balance-at-point
         "q" 'ledger-post-align-xact
         "r" 'ledger-reconcile
         "R" 'ledger-report
         "t" 'ledger-insert-effective-date
         "y" 'ledger-set-year
         "RET" 'ledger-set-month)
      (spacemacs/set-leader-keys-for-major-mode 'ledger-reconcile-mode
        (or dotspacemacs-major-mode-leader-key ",") 'ledger-reconcile-toggle
        "a" 'ledger-reconcile-add
        "q" 'ledger-reconcile-quit
        "t" 'ledger-reconcile-change-target
        "RET" 'ledger-reconcile-finish)
      ;; temporary hack to work-around an issue with evil-define-key
      ;; more info: https://github.com/emacs-evil/evil/issues/301
      ;; TODO remove this hack if the limitation is removed upstream
      (add-hook 'ledger-mode-hook 'evil-normalize-keymaps)
      (evilified-state-evilify ledger-report-mode ledger-report-mode-map))))
