install:
	mkdir -p ~/.emacs.d
	ln -sf `pwd`/init.el ~/.emacs.d/init.el
	ln -sf `pwd`/lisp ~/.emacs.d/lisp
uninstall:
	rm -rf ~/.emacs.d
