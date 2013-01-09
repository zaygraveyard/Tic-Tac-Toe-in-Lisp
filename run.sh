#!/bin/sh
pushd `dirname $0`
$1 tic-tac-toe.lisp
popd