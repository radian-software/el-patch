VERSION ?=
CMD ?=

SHELL := bash

EMACS ?= emacs

# The order is important for compilation.
for_compile := el-patch.el el-patch-template.el
for_checkdoc := el-patch.el el-patch-template.el
for_longlines := $(wildcard *.el *.md *.yml) Makefile
for_checkindent := $(wildcard *.el)

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		sed 's/%/LANG/'			| \
		column -t -s'|' >&2

.PHONY: lint
lint: compile checkdoc longlines checkindent toc ## Run all the linters

.PHONY: compile
compile: ## Byte-compile
	@for file in $(for_compile); do \
	    echo "[compile] $$file" ;\
	    $(EMACS) -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkdoc
checkdoc: ## Check docstring style
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" ;\
	    $(EMACS) -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(setq sentence-end-double-space nil)" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: longlines
longlines: ## Check for long lines
	@for file in $(for_longlines); do \
	    echo "[longlines] $$file"; \
	    cat "$$file" \
	        | sed '/[l]onglines-start/,/longlines-stop/d' \
	        | grep -E '.{80}' \
	        | grep -E -v '\[.+\]: (#|http)' \
	        | sed "s/^/$$file:long line: /" \
	        | grep . && exit 1 || true ;\
	done

.PHONY: checkindent
checkindent: ## Ensure that indentation is correct
	@tmpdir="$$(mktemp -d)"; for file in $(for_checkindent); do \
	    echo "[checkindent] $$file" >&2; \
	    emacs -Q --batch \
	        --eval "(setq inhibit-message t)" \
	        --eval "(load (expand-file-name \"el-patch.el\") nil t)" \
	        --eval "(find-file \"$$file\")" \
	        --eval "(indent-region (point-min) (point-max))" \
	        --eval "(write-file \"$$tmpdir/$$file\")"; \
	    (diff <(cat          "$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") \
	          <(cat "$$tmpdir/$$file" | nl -v1 -ba | \
                           sed "s/\t/: /" | sed "s/^ */$$file:/") ) \
	        | grep -F ">" | grep -o "[a-z].*" | grep . && exit 1 || true; \
	done

.PHONY: toc
toc: README.md ## Update table of contents in README
	@echo "[toc] $^"
	@if command -v markdown-toc >/dev/null; then \
	    markdown-toc -i $^ ; \
	else \
	    echo "  --> markdown-toc missing, skipping" ; \
	fi

.PHONY: clean
clean: ## Remove build artifacts
	@echo "[clean]" *.elc
	@rm -f *.elc

.PHONY: docker
docker: ## Start a Docker shell; e.g. make docker VERSION=25.3
	@scripts/docker.bash "$(VERSION)" "$(CMD)"

BUTTERCUP_VER := 1.34
BUTTERCUP := vendor/buttercup-$(BUTTERCUP_VER)

$(BUTTERCUP):
	@rm -rf $(BUTTERCUP) && mkdir -p $(BUTTERCUP)
	@curl -fsSL https://github.com/jorgenschaefer/emacs-buttercup/archive/refs/tags/v$(BUTTERCUP_VER).tar.gz -o $(BUTTERCUP).tar.gz
	@tar -xf $(BUTTERCUP).tar.gz --strip-components=1 -C $(BUTTERCUP)
	@rm $(BUTTERCUP).tar.gz

.PHONY: unit
unit: $(BUTTERCUP) ## Run unit tests
	@$(BUTTERCUP)/bin/buttercup test -L $(BUTTERCUP) -L .
