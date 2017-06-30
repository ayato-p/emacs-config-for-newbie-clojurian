CURRENT_DATE = $(shell date +"%Y%m%d")

clean:
	rm -rf ./elpa

install: clean
	EMACS_USER_DIRECTORY="$(shell pwd)" emacs -Q --batch -l ./init.el -f clojure-mode
