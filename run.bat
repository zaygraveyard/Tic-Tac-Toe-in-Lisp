@ECHO OFF
SETLOCAL

IF "%1" EQU "" (SET __CLISP__="clisp") ELSE (SET __CLISP__=%~f1)

PUSHD %~dp0
%__CLISP__% tic-tac-toe.lisp
POPD

ENDLOCAL
