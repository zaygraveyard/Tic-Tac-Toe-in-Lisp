; This file is part of Tic-Tac-Toe-in-Lisp Project. It is subject to the license
; terms in the LICENSE file found in the top-level directory of this distribution
; and at https://github.com/zaygraveyard/Tic-Tac-Toe-in-Lisp/raw/master/LICENSE.
; No part of Tic-Tac-Toe-in-Lisp Project, including this file, may be copied,
; modified, propagated, or distributed except according to the terms contained
; in the LICENSE file.

(defun all-equal (l x)
  (= (length (remove-if #'(lambda (a) (equal a x)) l)) 0))

(defun all-same (l)
  (or (null l) (all-equal l (first l))))

(defmacro equal* (&rest l)
  `(all-same ,l))

(defun elements-at (l &rest positions)
  (mapcar #'(lambda (position) (nth position l)) positions))

(defun random-element (l-or-element &rest elements)
  (let ((l (if (null elements) l-or-element (cons l-or-element elements))))
    (nth (random (length l)) l)))

(defun all-positions (item sequence &key (test nil) (test-not nil) (start 0) (end nil) (key nil))
  (let ((positions nil))
    (do ((pos (1- start) (position item sequence :test test :test-not test-not :start (1+ pos) :end end :key key)))
        ((null pos) (reverse positions))
      (unless (< pos start) (push pos positions)))))



(defconstant *?* nil)
(defconstant *X* 'x)
(defconstant *O* 'o)


(defstruct (modifier
             (:constructor create-modifier (map)))
  map)
(defun modifier-apply (modifier l)
  (let* ((map (modifier-map modifier))
         (new-list (copy-list l)))
    (dotimes (i 9)
      (setf (nth i new-list) (nth (nth i map) l)))
    new-list))
(defun modifier-add (modifier new-modifier)
  (let* ((map (modifier-map new-modifier))
         (old-map (modifier-map modifier))
         (new-map (copy-list old-map)))
    (dotimes (i 9)
      (setf (nth i new-map) (nth (nth i map) old-map)))
    (create-modifier new-map)))
(defun modifier-inverse (modifier)
  (let* ((old-map (modifier-map modifier))
         (new-map (copy-list old-map)))
    (dotimes (i 9)
      (setf (nth (position i old-map) new-map) i))
    (create-modifier new-map)))

(setf   n-modifier (create-modifier '(0 1 2 3 4 5 6 7 8)))
(setf  fh-modifier (create-modifier '(2 1 0 5 4 3 8 7 6)))
(setf  fv-modifier (create-modifier '(6 7 8 3 4 5 0 1 2)))
(setf fd--modifier (create-modifier '(0 3 6 1 4 7 2 5 8)))
(setf fd+-modifier (create-modifier '(8 5 2 7 4 1 6 3 0)))


(defstruct (action
             (:constructor create-action (location symbol))
             (:print-object print-action))
  location symbol)
(defun action-apply-modifier (action modifier)
  (create-action (nth (action-location action) (modifier-map modifier))
                 (action-symbol action)))
(defun print-action (action stream)
  (format stream "(~A<-~A)" (action-location action) (action-symbol action)))


(defstruct (board
             (:constructor create-board (&key (slots (list *?* *?* *?* *?* *?* *?* *?* *?* *?*))))
             (:print-object print-board))
  slots)
(defun copy-board (board)
  (create-board :slots (copy-list (board-slots board))))
(defun print-board (board stream)
  (let ((slots (board-slots board)))
    (dotimes (i 9)
      (format stream "~A " (or (nth i slots) i))
      (cond ((= i 8) (format stream "~%"))
            ((or (= i 2) (= i 5)) (format stream "~%----------~%"))
            (t (format stream "| "))))))
(defun board-is-empty (board)
  (equal board (create-board)))
(defun board-is-full (board)
  (= 0 (length (remove-if-not #'null (board-slots board)))))
(defun board-is-slot-empty (board slot-index)
  (and (integerp slot-index)
       (<= 0 slot-index 8)
       (equal *?* (nth slot-index (board-slots board)))))
(defun board-apply-modifier (board modifier)
  (create-board :slots (modifier-apply modifier (board-slots board))))
(defun board-apply-action (board action)
  (let* ((old-slots (board-slots board))
         (new-board (copy-board board))
         (new-slots (board-slots new-board)))
    (setf (nth (action-location action) new-slots) (action-symbol action))
    new-board))


(defstruct (player
             (:constructor create-player (&optional symbol)))
  symbol)
(defun player-make-move (player game)
  (ttt-game-apply-action
    game
    (cond ((human-p player) (human-get-action player game))
          ((ai-p player) (ai-get-action player game)))))


(defstruct (tic-tac-toe-game
             (:conc-name ttt-game-)
             (:constructor create-ttt-game (player1 player2
                                            &key
                                            (board (create-board))
                                            (modified-board (copy-board board)))))
  board
  modified-board
  (modifier n-modifier)
  player1
  player2
  (end? nil)
  (winner nil))
(defun ttt-game-is-slot-empty (game slot-index)
  (board-is-slot-empty (ttt-game-board game) slot-index))
(defun ttt-game-is-modified-slot-empty (game modified-slot-index)
  (board-is-slot-empty (ttt-game-modified-board game) modified-slot-index))
(defun ttt-game-did-end (game)
  (let* ((player1 (ttt-game-player1 game))
         (player2 (ttt-game-player2 game))
         (board (ttt-game-board game))
         (slots (board-slots board))
         (winner
           (cond ((all-same (elements-at slots 0 1 2)) (nth 0 slots))
                 ((all-same (elements-at slots 3 4 5)) (nth 3 slots))
                 ((all-same (elements-at slots 6 7 8)) (nth 6 slots))

                 ((all-same (elements-at slots 0 3 6)) (nth 0 slots))
                 ((all-same (elements-at slots 1 4 7)) (nth 1 slots))
                 ((all-same (elements-at slots 2 5 8)) (nth 2 slots))

                 ((all-same (elements-at slots 0 4 8)) (nth 0 slots))
                 ((all-same (elements-at slots 2 4 6)) (nth 2 slots))

                 (t *?*)))
         (end? (or (not (equal winner *?*)) (board-is-full board))))
    (setf (ttt-game-end? game) end?)
    (setf (ttt-game-winner game)
          (cond ((equal winner *?*) nil)
                ((equal winner (player-symbol player1)) player1)
                ((equal winner (player-symbol player2)) player2)))
    end?))
(defun ttt-game-reverse-modifier (game)
  (modifier-inverse (ttt-game-modifier game)))
(defun ttt-game-add-modifier (game modifier)
  (setf (ttt-game-modifier game)
        (modifier-add (ttt-game-modifier game) modifier))
  (setf (ttt-game-modified-board game)
        (board-apply-modifier (ttt-game-board game) (ttt-game-modifier game)))
  game)
(defun ttt-game-apply-action (game action)
  (setf (ttt-game-board game)
        (board-apply-action (ttt-game-board game) action))
  (setf (ttt-game-modified-board game)
        (board-apply-modifier (ttt-game-board game) (ttt-game-modifier game)))
  game)
(defun ttt-game-start (game)
  (do* ((player1 (ttt-game-player1 game))
        (player2 (ttt-game-player2 game))
        (player player1 (if (equal player player1) player2 player1)))
       ((ttt-game-did-end game) (format t "~%~A" (ttt-game-board game)) (ttt-game-winner game))
    (format t "~%~A" (ttt-game-board game))
    (player-make-move player game)))





(defstruct (human-player
             (:include player)
             (:conc-name human-)
             (:predicate human-p)
             (:constructor create-human-player (&optional symbol))))
(defun human-get-action (player game)
  (create-action (human-get-move player game) (player-symbol player)))
(defun human-get-move (player game)
  (format t "~%Please enter your move (0-8): ")
  (do ((mov (read) (read)))
      ((ttt-game-is-slot-empty game mov) mov)))




(defstruct (ai-player
             (:include player)
             (:conc-name ai-)
             (:predicate ai-p)
             (:constructor create-ai-player (&optional symbol))))
(defun ai-get-action (player game)
  (action-apply-modifier (create-action (ai-get-move player game) (player-symbol player))
                         (ttt-game-reverse-modifier game)))
(defun ai-get-move (player game)
  (or (ai-get-move-try-to-win player game)
      (ai-get-move-try-to-block player game)
      (ai-get-best-move player game)
      (ai-get-random-move player game)))



(defun get-trifecta (slots symbol abc)
  (let* ((a (first abc)) (b (second abc)) (c (third abc))
         (l (elements-at slots a b c))
         (p nil))
    (if (and (= 2 (length (remove-if-not #'(lambda (x) (equal x symbol)) l)))
             (setf p (position *?* l)))
      (nth p abc)
      nil)))

(defun ai-get-move-try-to-win (player game)
  (do* ((lines '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)) (rest lines))
        (symbol (player-symbol player))
        (slots (board-slots (ttt-game-modified-board game)))
        (trifecta (get-trifecta slots symbol (first lines)) (get-trifecta slots symbol (first lines))))
       ((or (= 1 (length lines)) trifecta) trifecta)))

(defun ai-get-move-try-to-block (player game)
  (do* ((lines '((0 1 2) (3 4 5) (6 7 8) (0 3 6) (1 4 7) (2 5 8) (0 4 8) (2 4 6)) (rest lines))
        (symbol (if (equal (player-symbol player) *X*) *O* *X*))
        (slots (board-slots (ttt-game-modified-board game)))
        (trifecta (get-trifecta slots symbol (first lines)) (get-trifecta slots symbol (first lines))))
       ((or (= 1 (length lines)) trifecta) trifecta)))

(defun ai-get-random-move (player game)
  (random-element (remove-if-not #'(lambda (x) (ttt-game-is-modified-slot-empty game x)) '(0 1 2 3 4 5 6 7 8))))

(defun ai-get-best-move (player game)
  (let* ((my-symbol (player-symbol player))
         (his-symbol (if (equal my-symbol *X*) *O* *X*))
         (slots (board-slots (ttt-game-modified-board game)))
         (my-pos (all-positions my-symbol slots))
         (his-pos (all-positions his-symbol slots))

         (n n-modifier)
         (fh fh-modifier)
         (fv fv-modifier)
         (fd- fd--modifier)
         (fd+ fd+-modifier))
    (cond ((and (equal my-pos '()) (equal his-pos '())) 4)

          ((and (equal my-pos '(4)) (equal his-pos '(0))) 8)
          ((and (equal my-pos '(4)) (equal his-pos '(1))) (ttt-game-add-modifier game (random-element n fh)) 8)
          ((and (equal my-pos '(4)) (equal his-pos '(2))) (ttt-game-add-modifier game fh) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(3))) (ttt-game-add-modifier game fd-) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(5))) (ttt-game-add-modifier game fd+) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(6))) (ttt-game-add-modifier game fv) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(7))) (ttt-game-add-modifier game fv) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(8))) (ttt-game-add-modifier game fd+) (ai-get-move player game))

          ((and (equal my-pos '(4 8)) (equal his-pos '(0 5))) 6)
          ((and (equal my-pos '(4 8)) (equal his-pos '(0 7))) (ttt-game-add-modifier game fd-) (ai-get-move player game))


          ((and (equal my-pos '()) (equal his-pos '(0))) 4)
          ((and (equal my-pos '()) (equal his-pos '(1))) 4)
          ((and (equal my-pos '()) (equal his-pos '(2))) (ttt-game-add-modifier game fh) (ai-get-move player game))
          ((and (equal my-pos '()) (equal his-pos '(3))) (ttt-game-add-modifier game fd-) (ai-get-move player game))
          ((and (equal my-pos '()) (equal his-pos '(4))) (ttt-game-add-modifier game (random-element n fh fv fd+)) 0)
          ((and (equal my-pos '()) (equal his-pos '(5))) (ttt-game-add-modifier game fd+) (ai-get-move player game))
          ((and (equal my-pos '()) (equal his-pos '(6))) (ttt-game-add-modifier game fv) (ai-get-move player game))
          ((and (equal my-pos '()) (equal his-pos '(7))) (ttt-game-add-modifier game fv) (ai-get-move player game))
          ((and (equal my-pos '()) (equal his-pos '(8))) (ttt-game-add-modifier game fd+) (ai-get-move player game))

          ((and (equal my-pos '(0)) (equal his-pos '(4 8))) (ttt-game-add-modifier game (random-element n fd-)) 2)
          ((and (equal my-pos '(4)) (equal his-pos '(0 5))) (random-element 1 2 7 8))
          ((and (equal my-pos '(4)) (equal his-pos '(0 7))) (ttt-game-add-modifier game fd-) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(0 8))) (ttt-game-add-modifier game (random-element n fv fd- fd+)) 1)
          ((and (equal my-pos '(4)) (equal his-pos '(1 3))) (random-element 0 2 6))
          ((and (equal my-pos '(4)) (equal his-pos '(1 5))) (ttt-game-add-modifier game fh) (ai-get-move player game))
          ((and (equal my-pos '(4)) (equal his-pos '(1 6))) (random-element 0 2 3 5))
          ((and (equal my-pos '(4)) (equal his-pos '(1 8))) (ttt-game-add-modifier game fh) (ai-get-move player game))

          ((and (equal my-pos '(1 4)) (equal his-pos '(0 5 7))) (random-element 6 8))
          ((and (equal my-pos '(4 8)) (equal his-pos '(0 1 5))) 6)
          ((and (equal my-pos '(4 8)) (equal his-pos '(0 2 5))) 6)
          ((and (equal my-pos '(4 8)) (equal his-pos '(0 3 5))) 6)
          ((and (equal my-pos '(4 8)) (equal his-pos '(0 5 7))) 6)
          ((and (equal my-pos '(0 4)) (equal his-pos '(1 3 8))) 6)
          ((and (equal my-pos '(2 4)) (equal his-pos '(1 5 6))) (ttt-game-add-modifier game fh) (ai-get-move player game))
          ((and (equal my-pos '(3 4)) (equal his-pos '(1 5 6))) (random-element 0 2 8))
          ((and (equal my-pos '(4 5)) (equal his-pos '(1 3 8))) (ttt-game-add-modifier game fh) (ai-get-move player game))


          (t nil))))
