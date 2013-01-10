; This file is part of Tic-Tac-Toe-in-Lisp Project. It is subject to the license
; terms in the LICENSE file found in the top-level directory of this distribution
; and at https://github.com/zaygraveyard/Tic-Tac-Toe-in-Lisp/raw/master/LICENSE.
; No part of Tic-Tac-Toe-in-Lisp Project, including this file, may be copied,
; modified, propagated, or distributed except according to the terms contained
; in the LICENSE file.

(load "tic-tac-toe-lib.lisp")

(defun get-yes-no (msg &optional (default t))
  (let ((answer (progn (format t msg) (char-downcase (read-char)))))
    (cond ((eq answer #\y) t)
          ((eq answer #\Newline) default)
          (t nil))))

(format t "~%===========================")
(format t "~%= Welcome to Tic-Tac-Toe! =")
(format t "~%===========================~%")

(setf human (create-human-player *X*))
(setf ai (create-ai-player *O*))
(loop as answer = t then (get-yes-no "~%Do you want to play again? [Y,n] ")
      while answer
      do (setf start? (get-yes-no "Do you want to start? [Y,n] "))
         (setf winner (ttt-game-start (create-ttt-game (if start? human ai) (if start? ai human))))
         (if winner
           (format t "~%~%The winner is ~A!~%" (player-symbol winner))
           (format t "~%~%Its a tie!!~%")))