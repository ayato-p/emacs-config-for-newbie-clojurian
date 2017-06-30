CURRENT_DATE = $(shell date +"%Y%m%d")

clean:
	rm -rf ./elpa
	rm ./init.elc

build: clean
	emacs -Q --batch -l ./init.el
	find ./ -name '*.el' | xargs emacs -Q --batch -l ./init.el -f batch-byte-compile

deploy: build
	git add .
	git commit -m ":fire:"
	git push origin master
	git tag $(CURRENT_DATE)
	git push origin $(CURRENT_DATE)
