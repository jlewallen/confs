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

(setq jlewallen-shackle-packages
      '(
        (shackle  :location local)
        )
      )

(defun jlewallen-shackle/init-shackle ()
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  )

(defun jlewallen-shackle/post-init-shackle ())
