.PHONY: all
all: compile checkdoc toc

.PHONY: travis
travis: compile checkdoc

.PHONY: compile
compile:
	! emacs -Q --batch --eval                  \
            "(progn                                \
               (setq byte-compile-error-on-warn t) \
               (push default-directory load-path)  \
               (batch-byte-compile))"              \
            el-patch.el                            \
            2>&1 | grep .

.PHONY: checkdoc
checkdoc:
	! emacs --batch --eval                      \
            "(progn                                 \
               (setq sentence-end-double-space nil) \
               (checkdoc-file \"el-patch.el\"))"    \
            2>&1 | grep .

.PHONY: toc
toc:
	markdown-toc -i README.md

.PHONY: clean
clean:
	rm -f straight.elc
