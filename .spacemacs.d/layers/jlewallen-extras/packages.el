;;; packages.el --- jlewallen-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Jacob Lewallen <jlewallen@JACOB-WORK>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `jlewallen-extras-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `jlewallen-extras/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `jlewallen-extras/pre-init-PACKAGE' and/or
;;   `jlewallen-extras/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst jlewallen-extras-packages
  '(nimbus-theme))

(defun jlewallen-extras/init-nimbus-theme ())

(defun jlewallen-extras/post-init-numbus-theme ())

;;; packages.el ends here
