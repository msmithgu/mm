dev:
	elm reactor 

deploy:
	elm make --optimize src/Main.elm && mv index.html ../msmithgu.github.io/mmk.html

.PHONY: dev deploy