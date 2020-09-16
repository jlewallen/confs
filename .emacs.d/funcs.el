
(defun my/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

;; from magnars
(defun my/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (configuration-layer/package-used-p 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

;; from magnars
(defun my/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
            (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (when (and (configuration-layer/package-used-p 'projectile)
                       (projectile-project-p))
              (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun my/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

;; from https://gist.github.com/3402786
(defun my/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;; https://tsdh.wordpress.com/2007/03/28/deleting-windows-vertically-or-horizontally/
(defun my/maximize-horizontally ()
  "Delete all windows to the left and right of the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-left) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-right) (error nil))
      (delete-window))))

(defun my/maximize-vertically ()
  "Delete all windows above and below the current window."
  (interactive)
  (require 'windmove)
  (save-excursion
    (while (condition-case nil (windmove-up) (error nil))
      (delete-window))
    (while (condition-case nil (windmove-down) (error nil))
      (delete-window))))

(defun my/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

(defun my/find-dotfile-keys ()
  "Edit the keys.el `dotfile', in the current window."
  (interactive)
  (find-file-existing "~/.emacs.d/keys.el"))

(defun my/navigate-left ()
  (interactive)
  (ccls-navigate "L"))

(defun my/navigate-right ()
  (interactive)
  (ccls-navigate "R"))

(defun my/navigate-down ()
  (interactive)
  (ccls-navigate "D"))

(defun my/navigate-up ()
  (interactive)
  (ccls-navigate "U"))

(defun my/jlewallen-open-notes ()
  (interactive)
  (find-file-existing "~/dropbox/notes/notes.org"))

(defun my/jlewallen-open-capture ()
  (interactive)
  (find-file-existing "~/dropbox/notes/capture.org"))

(defun my/jlewallen-open-agenda ()
  (interactive)
  (find-file-existing "~/dropbox/notes/agenda.org"))

(defun my/ledger-duplicate-current-transaction (date)
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

(defun my/magit-status-fullscreen ()
  (interactive)
  (let ((pop-up-windows nil))
    (call-interactively 'magit-status)))


(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
  (interactive "f")
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
							(org-read-date t nil "now")))
		 (date (org-date-to-gregorian datetree-date))
		 )
	(with-current-buffer (current-buffer)
	  (save-excursion
		(org-cut-subtree)
		(if file (find-file file))
		(org-datetree-find-date-create date)
		(org-narrow-to-subtree)
		(show-subtree)
		(org-end-of-subtree t)
		(newline)
		(goto-char (point-max))
		(org-paste-subtree 4)
		(widen)
		))
	)
  )
