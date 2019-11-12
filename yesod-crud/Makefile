all: build

RUN_IN_DOCKER = docker-compose run --user $$(id -u)

bash:
	${RUN_IN_DOCKER} haskell bash

ghci:
	${RUN_IN_DOCKER} haskell stack ghci

build-image:
	docker-compose build

build: build-image
	${RUN_IN_DOCKER} haskell stack build -j4

build-watch:
	${RUN_IN_DOCKER} haskell stack build --file-watch -j4 --fast

run:
	${RUN_IN_DOCKER} --publish 8000:8000 haskell stack run -j4

run-watch:
	${RUN_IN_DOCKER} --publish 8000:8000 --publish 3000:3000 haskell stack exec -- yesod devel

tests:
	${RUN_IN_DOCKER} haskell stack test -j4 --fast

tests-watch:
	${RUN_IN_DOCKER} haskell stack test --file-watch --fast -j4
