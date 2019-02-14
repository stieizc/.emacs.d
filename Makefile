RELOAD_INIT = "(progn (setq user-init-file (expand-file-name \"init.el\")) (straight-transaction (straight-mark-transaction-as-init) (message \"Reloading init.el...\") (load user-init-file nil 'nomessage) (message \"Reloading init.el... done.\")))" 

default: init-packages

init-packages:
	emacs -Q --batch -l init.el \
		--eval $(RELOAD_INIT)

freeze:
	emacs -Q --batch -l init.el \
		--eval $(RELOAD_INIT) \
		-f straight-freeze-versions

fetch-all:
	emacs -Q --batch -l init.el \
		--eval $(RELOAD_INIT) \
		--eval "(let ((current-prefix-arg t)) (straight-fetch-all))"

pull-all:
	emacs -Q --batch -l init.el \
		--eval $(RELOAD_INIT) \
		--eval "(let ((current-prefix-arg t)) (straight-pull-all))"

linux-fonts:
	emacs -Q --batch -l init.el \
		--eval $(RELOAD_INIT) \
		--eval "(setq window-system 'x)" \
		-f all-the-icons-install-fonts

clean:
	rm -f *.elc

test-run:
	emacs -Q -l init.el

.PHONY: default compile clean freeze
