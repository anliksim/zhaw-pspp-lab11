#!/bin/bash

CURRENT_DIR=$(pwd)
WORK_DIR="/usr/local/share/common-lisp/source"

docker run --rm -it \
  -v "${CURRENT_DIR}":"${WORK_DIR}" \
  -w "${WORK_DIR}" \
  daewok/lisp-devel:latest sbcl --script "$1"