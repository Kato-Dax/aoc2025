#!/usr/bin/env bash

set -xe

SOCKET=$(git rev-parse --show-toplevel)/.guile-repl.socket
if [ -f $SOCKET ]; then rm $SOCKET; fi
GUILE_LOAD_PATH=".:..." guile --listen=$SOCKET
rm $SOCKET
