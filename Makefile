EMACS = emacs

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

install:
	@echo "Installing hcl-mode.el..."
	@cp hcl-mode.el ~/.emacs.d/lisp/
	@chmod 644 ~/.emacs.d/lisp/hcl-mode.el

clean:
	@echo "Cleaning up..."
	@rm -f *.elc

.PHONY: all build lint test install clean
