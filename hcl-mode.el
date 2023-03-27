;;; hcl-mode.el --- Major mode for editing HashiCorp Configuration Language (HCL)

;; Author: Jason Walsh <jwalsh1@chewy.com>
;; URL: [URL for the source code]
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: infrastructure, aws, terraform

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the Apache License, Version 2.0.

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; `hcl-mode' is a major mode for editing HashiCorp Configuration Language (HCL)
;; files used in tools like Terraform, Packer, and Consul.

;; To use this mode, save this file to a directory in your Emacs `load-path', then
;; add the following code to your Emacs initialization file (e.g. ~/.emacs.d/init.el):

;; (require 'hcl-mode)

;;; Code:

(defgroup hcl-mode nil
  "Major mode for editing HashiCorp Configuration Language (HCL) files."
  :group 'languages)

(defcustom hcl-mode-extensions '(".hcl" ".tf")
  "List of file extensions associated with `hcl-mode'."
  :type '(repeat string)
  :group 'hcl-mode)

(defcustom hcl-mode-comment-start "# "
  "String used to start a comment in `hcl-mode'."
  :type 'string
  :group 'hcl-mode)

(defvar hcl-mode-syntax-table nil "Syntax table for `hcl-mode'.")

(setq hcl-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; Single-line comments start with "#"
        (modify-syntax-entry ?# "<" syn-table)
        (modify-syntax-entry ?\n ">" syn-table)
        syn-table))

(defvar hcl-mode-highlights
  '(("#.*" . font-lock-comment-face) ; comments
    ("\".*?\"" . font-lock-string-face) ; strings
    ("^\\s-*\\([a-z0-9_]+\\)\\s-*=" 1 font-lock-variable-name-face) ; variables
    ("^\\s-*\\([a-z0-9_]+\\)\\s-*$" 1 font-lock-keyword-face))) ; keywords

;;;###autoload
(define-derived-mode hcl-mode prog-mode "HCL"
  "Major mode for editing HashiCorp Configuration Language (HCL) files."
  :syntax-table hcl-mode-syntax-table

  (setq-local comment-start hcl-mode-comment-start)
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(hcl-mode-highlights)))

;;;###autoload
(dolist (ext hcl-mode-extensions)
  (add-to-list 'auto-mode-alist `(,ext . hcl-mode)))

(provide 'hcl-mode)

;;; hcl-mode.el ends here
