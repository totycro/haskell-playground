#!/usr/bin/env bash

function run () {
    echo "========================================"
    echo "= $2 ($1)"
    echo "========================================"
    http "localhost:8000$1"
}

run "/does-not-exist" "404"
run "/word" "list"
run "/word/2" "detail"
run "/word/2333" "detail does not exist"
