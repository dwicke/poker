(defun test-play-game ()
       (print "agent 1 continues to play")
;;        (play-game 
;;           (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(5 S) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) :agents(make-agents 2 4)) 4) 
;;        (print "--------------------------------------------------------------------------")
;;        (print "agent 1 stops playing")
;;        (play-game 
;;           (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(5 S) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) :agents(make-agents 2 5)) 4)
;;        (print "--------------------------------------------------------------------------")
       (print "?")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(A C) '(A H) '(A C) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) 
                       :agents (list 
                                  (make-agent :id 0 :chips 10 :bet #'safe)
                                  (make-agent :id 1 :chips 2 :bet #'safe)
                                  (make-agent :id 1 :chips 10 :bet #'safe)))
          4)
       )

(test-play-game)
