(require 'ert)
(require 'hcl-mode)

(ert-deftest hcl-mode-test-syntax-table ()
  "Test syntax table in `hcl-mode'."
  (with-temp-buffer
    (setq major-mode 'hcl-mode)
    (insert "# This is a comment.\n"
            "variable = \"value\"\n"
            "resource \"aws_instance\" \"example\" {\n"
            "  ami           = \"${var.ami}\"\n"
            "  instance_type = \"${var.instance_type}\"\n"
            "}\n")
    (hcl-mode)
    (let ((table (syntax-table)))
      (should (eq (aref table ?#) "<"))
      (should (eq (aref table ?\n) ">")))))

(ert-deftest hcl-mode-test-font-lock ()
  "Test font lock in `hcl-mode'."
  (with-temp-buffer
    (setq major-mode 'hcl-mode)
    (insert "# This is a comment.\n"
            "variable = \"value\"\n"
            "resource \"aws_instance\" \"example\" {\n"
            "  ami           = \"${var.ami}\"\n"
            "  instance_type = \"${var.instance_type}\"\n"
            "}\n")
    (hcl-mode)
    (let ((colors (list (
