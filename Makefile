RUN_IN_DOCKER = docker-compose run --user $$(id -u)

bash:
	${RUN_IN_DOCKER} haskell bash

build-image:
	docker-compose build

build-binary: build-image
	${RUN_IN_DOCKER} haskell stack build -j4

run:
	${RUN_IN_DOCKER} --publish 8000:8000 haskell stack run

watch-run:
	${RUN_IN_DOCKER} haskell bash -c "inotifywait --monitor -e close_write Main.hs | while read event; do time stack run ; done;"
