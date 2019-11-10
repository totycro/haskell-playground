all: build

RUN_IN_DOCKER = docker-compose run --user $$(id -u)

bash:
	${RUN_IN_DOCKER} haskell bash

build-image:
	docker-compose build

build: build-image
	${RUN_IN_DOCKER} haskell stack build -j4

build-watch:
	${RUN_IN_DOCKER} haskell stack build --file-watch -j4

run:
	${RUN_IN_DOCKER} --publish 8000:8000 haskell stack run

run-watch:
	${RUN_IN_DOCKER} --publish 8000:8000 haskell stack exec -- yesod devel

tests:
	${RUN_IN_DOCKER} haskell stack test

tests-watch:
	${RUN_IN_DOCKER} haskell stack test --file-watch -j4
