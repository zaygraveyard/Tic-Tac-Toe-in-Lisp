#!/bin/sh

if [ $# -gt 0 ]; then
    __CLISP__=`echo $( cd "$(dirname "$1")"; pwd)/$(basename "$1")`;
else
    __CLISP__=clisp;
fi

pushd "`dirname "$0"`" > /dev/null
"$__CLISP__" tic-tac-toe.lisp
popd