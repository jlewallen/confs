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
  '(
    nimbus-theme
    badger-theme
    distinguished-theme
    arjen-grey-theme
    melancholy-theme
    reykjavik-theme
    paganini-theme
    night-owl-theme
    srcery-theme
    klere-theme
    boron-theme
    firecode-theme
    ; kooteen-theme
    ; reverse-theme
    waher-theme
    ))

(defun jlewallen-extras/init-nimbus-theme ())
(defun jlewallen-extras/init-badger-theme ())
(defun jlewallen-extras/init-distinguished-theme ())
(defun jlewallen-extras/init-arjen-grey-theme ())
(defun jlewallen-extras/init-melancholy-theme ())
(defun jlewallen-extras/init-reykjavik-theme ())
(defun jlewallen-extras/init-paganini-theme ())
(defun jlewallen-extras/init-night-owl-theme ())
(defun jlewallen-extras/init-srcery-theme ())
(defun jlewallen-extras/init-klere-theme ())
(defun jlewallen-extras/init-boron-theme ())
(defun jlewallen-extras/init-firecode-theme ())
(defun jlewallen-extras/init-kooteen-theme ())
(defun jlewallen-extras/init-reverse-theme ())
(defun jlewallen-extras/init-waher-theme ())

;;; packages.el ends here
