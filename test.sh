#!/usr/bin/bash -e

function run () {
    echo "========================================"
    echo "= $2 ($1)"
    echo "========================================"
    http "localhost:8000$1" $3
}


# TODO: This is a quite poor an ad-hoc testing script, integrate it in proper haskell framework

docker-compose exec db psql -U postgres postgres --command "drop table words;" || true
make run&
server_pid=$!

function cleanup() {
    kill $server_pid
}

trap cleanup EXIT INT

sleep 2

run "/does-not-exist" "404"
run "/word" "list"
run "/word/2" "detail"
run "/word/2333" "detail does not exist"
run "/word" "create" text=test_$(date --iso-8601=seconds)
run "/word" "create missing data" other_key=other_value
