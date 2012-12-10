(defun test-play-game ()
       (print "agent 1 continues to play becuase he has no chips.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(5 S) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) :agents(make-agents 2 4)) 4) 
       (print "--------------------------------------------------------------------------")
       (print "agent 1 stops playing becuase he has chips.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(5 S) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) :agents(make-agents 2 5)) 4)
       (print "--------------------------------------------------------------------------")
       (print "two ties where the pot is split evenly between the two agents.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(A C) '(A H) '(A C) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) 
                       :agents (list 
                                  (make-agent :id 0 :chips 5 :bet #'safe)
                                  (make-agent :id 1 :chips 5 :bet #'safe)))
          2)
       (print "--------------------------------------------------------------------------")
       (print "2 ties and an odd pot where the left overs are carried over to the next pot.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(A C) '(A H) '(A C) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) 
                       :agents (list 
                                  (make-agent :id 'Joker :chips 20 :bet #'safe)
                                  (make-agent :id 'Catwomen :chips 5 :bet #'safe)
                                  (make-agent :id 'Batman :chips 5 :bet #'safe)))
          2)
       (print "--------------------------------------------------------------------------")
       (print "The Joker wins.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(A H) '(J C) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S))) 
                       :agents (list 
                                  (make-agent :id 'Joker :chips 20 :bet #'safe)
                                  (make-agent :id 'Batman :chips 2 :bet #'safe)))
          2)
       (print "--------------------------------------------------------------------------")
       (print "The Batman wins.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (RandomShuffle (make-deck)))
                       :agents (list 
                                  (make-agent :id 'Joker :chips 20 :bet #'safe)
                                  (make-agent :id 'Batman :chips 20 :bet #'bluff)))
          100)
       (print "--------------------------------------------------------------------------")
       (print "Test bluffing vs risky. Batman wins.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (RandomShuffle (make-deck)))
                       :agents (list 
                                  (make-agent :id 'Joker :chips 20 :bet #'risky)
                                  (make-agent :id 'Batman :chips 20 :bet #'bluff)))
          100)
       (print "Test win with no chips to make sure he does not get removed before he gets his winnings. Joker should win.")
       (play-game 
          (make-dealer :pot 0 :deckMaker #'(lambda () (list '(A D) '(A S) '(2 H) '(J C) '(Q H) '(Q D) '(K H) '(8 D) '(J C) '(4 D) '(J S)))
                       :agents (list 
                                  (make-agent :id 'Joker :chips 4 :bet #'bluff)
                                  (make-agent :id 'Batman :chips 6 :bet #'bluff)))
          2)
       )

(test-play-game)
