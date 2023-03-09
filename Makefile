default: init-packages

rebuild-init:
	emacs --batch -l early-init.el \
		--eval '(straight-rebuild-package "0init")'
rebuild-all:
	emacs --batch -l early-init.el \
		--eval '(straight-rebuild-all)'

init-packages:
	emacs -Q --batch -l init.el \
		--eval "(all-the-icons-install-fonts t)"

freeze:
	emacs -Q --batch -l init.el \
		-f straight-freeze-versions

fetch-all:
	emacs -Q --batch -l init.el \
		--eval "(let ((current-prefix-arg t)) (straight-fetch-all))"

pull-all:
	emacs -Q --batch -l init.el \
		--eval "(let ((current-prefix-arg t)) (straight-pull-all))"

linux-fonts:
	emacs -Q --batch -l init.el \
		--eval "(setq window-system 'x)" \
		-f all-the-icons-install-fonts

clean:
	rm -f *.elc

test-run:
	emacs -Q -l init.el

.PHONY: default compile clean freeze
