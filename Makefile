EMACS = emacs
DESTDIR = ~/.emacs.d/
EMACS_LISP_DIR = lisp/

all: build

build:
	@echo "Compiling hcl-mode.el..."
	@$(EMACS) -batch -f batch-byte-compile hcl-mode.el

lint:
	@echo "Running flycheck on hcl-mode.el..."
	@$(EMACS) -batch -L . -f package-initialize -f flycheck-mode hcl-mode.el -f flycheck-buffer

test:
	@echo "Running tests for hcl-mode.el..."
	@$(EMACS) -batch -L . -f ert-run-tests-batch hcl-mode-test.el

install: build
	@echo "Installing hcl-mode.el..."
	@mkdir -p $(DESTDIR)$(EMACS_LISP_DIR)
	@cp hcl-mode.el $(DESTDIR)$(EMACS_LISP_DIR)
	@chmod 644 $(DESTDIR)$(EMACS_LISP_DIR)hcl-mode.el

clean:
	@echo "Cleaning up..."
	@rm -f *.elc

.PHONY: all build lint test install clean
