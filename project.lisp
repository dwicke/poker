;PokerMaster is the main function that takes three arguments:
;	    - N: # of PokerAgent's, 
;	    - M: # of playing chips per PokerAgent,
;	    - K: # of games to play/simulate.

(defun PokerMaster (N M K)
  (let ((winners));winners is the list of winners
    (dotimes (i K winners) ; play K games
      (setf winners (cons (play-game ;play the game
			   (make-dealer :pot 0 :deck (RandomShuffle (make-deck)) ; create the dealer
					:agents(make-agents N M))) ; create the players
			  winners))))); set the winner
    


; This macro is used in play-game
; so if game is done no one can bet
; then go to end.
(defmacro m-collect-bets (a-dealer)
  `(if (collect-bets ,a-dealer)
      (go end)))


(defun play-game (the-dealer) ; winner is the final agent left with chips
    (do () ((= (length (dealer-agents the-dealer)) 1) (dealer-agents the-dealer))
      ; so we do the following until there is a victor.
      (deal-hands the-dealer)   ; first deal all the agents their hand ie two cards to each agent
      (m-collect-bets the-dealer) ; get the bets from all the agents and if a winner go end
      (deal-com the-dealer 3)
      (m-collect-bets the-dealer)
      (deal-com the-dealer 1)
      (m-collect-bets the-dealer)
      (deal-com the-dealer 1)
      (m-collect-bets the-dealer)
      end
      (eval-winner the-dealer)
      ))

; This creates num-agents number of agents
; that are initialized with num-chips number of chips
; and the agents are returned as a list.
(defun make-agents (num-agents num-chips)
  (let ((agents))
    (dotimes (i num-agents agents)
      (setf agents (cons 
		    (make-agent :id i :chips num-chips :fold #'play-hand)
		    agents))
      )))


(defstruct agent
  id     ; the id of the agent
  chips  ; the remaining chips the agent has
  hand   ; the hand the agent was delt NOTE: *******is empty if folded
  fold  ; a predicate that will evaluate the current state and return t if should fold
)

(defstruct dealer
  deck      ; the remaining cards that the dealer can choose from
  agents    ; the remaining players in the game
  communal  ; the communal cards for the current game
  pot       ; the number of chips in the pot
)


(defun deal-com (the-dealer num) 
    (setf (dealer-communal the-dealer) 
	  (append (rand-remove the-dealer num) (dealer-communal the-dealer)))
)


; give two random cards from the deck to each of the
; players
(defun deal-hands (the-dealer)
  (dolist (cur-agent (dealer-agents the-dealer))
    (setf (agent-hand cur-agent) (rand-remove the-dealer 2)))
)

; returns num cards from the deck and 
; removes them from the deck
(defun rand-remove (the-dealer num)
     (loop :repeat num :for x :in (dealer-deck the-dealer) :collect x :do
       (setf  (dealer-deck the-dealer) (remove x  (dealer-deck the-dealer)))))



(defun test-rand-remove ()
  (let ((dealer (make-dealer :pot 0 :deck (RandomShuffle (make-deck)) ; create the dealer
					:agents(make-agents 2 5))))
    (print (dealer-deck dealer))
 (print (rand-remove dealer 2))
    (print (dealer-deck dealer))))






; Loop over all agents and collect bets from agents
; counting those that fold.  if (<= (- (length agents) n-folds) 1) 
; return T else nil
(defun collect-bets (the-dealer) 
  ; amount is the amount the current agent has bet
  ; agents is a lambda function that returns the list of agents that have not folded
  (let ( (amount 0) (agents #'(lambda () (remove-if #'(lambda (x) (if (= 0 (length (agent-hand x))) T)) (dealer-agents the-dealer)))))
    (dolist (ag (apply agents '()) (<= (length (apply agents '())) 1))
      (setf amount (apply (agent-fold ag) (list ag (dealer-communal the-dealer) (dealer-pot the-dealer))))
      (print amount)
      (if (= 0 amount) (setf (agent-hand ag) '())) ; discard the agent's cards if it didn't bet
      (print amount)
      (decf (agent-chips ag) amount)               ; remove the number of chips from the agent that it bet
      
      (incf (dealer-pot the-dealer) amount)        ; and add them to the pot
      (print (dealer-pot the-dealer))
      )))


; So Give the chips in the pot to the winner
; and remove any agents that have no chips left
(defun eval-winner (the-dealer)
  (let* ((agents #'(lambda () (remove-if #'(lambda (x) (if (= 0 (length (agent-hand x))) T)) (dealer-agents the-dealer))))
	 (sorted-agents (sort (apply agents '()) #'(lambda (x y) (if (not (null y)) (>= (CompareHands
							     (append (agent-hand x) (dealer-communal the-dealer))  
							     (append (agent-hand y) (dealer-communal the-dealer)))  0) T) ))))
    ; might want to do a remove if not equal to the first one when compare hands is done ie remove all that don't return 0 when compared to first in sorted-agents
    ; then I can split the pot.
    ; so the first agent in the sorted-agents is the winner! give him the pot
    (incf (agent-chips (first sorted-agents)) (dealer-pot the-dealer)))
 ; (print "past incf")
  ;set pot to 0
  (setf (dealer-pot the-dealer) 0)
  ; then remove the agents that have 3 or less chips left to play with.
  ; (this ensures that it is possible for them to play another round by betting one chip each time)
  (setf (dealer-agents the-dealer) (remove-if  #'(lambda (x) (if (<= 3 (agent-chips x)) T)) (dealer-agents the-dealer)))

)



(defun test-eval-winner ()
  (let ((dealer (make-dealer :pot 0 :deck (RandomShuffle (make-deck)) ; create the dealer
					:agents(make-agents 2 5))))
    (deal-hands dealer)
    (setf (agent-chips (first (dealer-agents dealer))) 2)
    
    (deal-com dealer 5)
  (eval-winner dealer)
  (print (dealer-agents dealer))))


; this is the interface to all fold functions
; this function will return 0 if folding
; otherwise it means to bet the returned number of chips
(defun play-hand (agent com-cards pot) 
  (if (= 0 (agent-chips agent)) 0 1)
 ); note that we could use this as a forwarding function based on id of agent
  















; Drew Wicke
; CS580 Fall 2012
; Dr. Duric
; Homework #3


;; (i) 


; Effects: returns the proportion of possible card pairs that are more 
;          desirable than the given pair using allpairs
(defun HandRank2 (allpairs yourhand)
  (let ((count 0) (yoursorted (sorthand (numericalhand yourhand))))
    (dolist (hand allpairs)
      (setf count (incf count))
      (if (equal (sorthand (numericalhand hand)) yoursorted)
	  (return)))
    (- 1 (/ count (length allpairs)))))


(defun testHandRank2 ()
  (print (HandRank2 (getpairs) '((A S) (A H)))); none are better so 0
  (print (HandRank2 (getpairs) '((2 S) (3 H))))); all are better except current 1325/1326


;-------------------------------------------------------------------------
; I wasn't sure how you test the code so I made the functions even though
; they call the same helper function.
; (ii)

; returns the number of opponent hands that are better 
; than the best hand that can be created by the arguments
; note this is used when there are 3 communal cards
(defun HandRank5 (mycards communal)
  (num-better mycards communal 1000))


(defun test-num-better ()
  (print (num-better '((A S) (A H)) '((A C) (A D) (2 H)) 1000)); should be zero
  (print (num-better '((A S) (2 H)) '((10 C) (J C) (Q C)) 1000))
  (print (num-better '((2 S) (3 H)) '((10 C) (J C) (4 H) (5 H)) 1000)); should be close to 1000
  (print (num-better '((5 S) (K H)) '((2 C) (10 S) (9 D) (A S) (J D)) 1000)))


; (iii)
; returns the number of opponent hands that are better 
; than the best hand that can be created by the argument
; note this is used when there are 4 communal cards
(defun HandRank6 (mycards communal)
  (num-better mycards communal 1000))


; (iv)
; returns the number of opponent hands that are better 
; than the best hand that can be created by the argument
; note this is used when there are 5 communal cards
(defun HandRank7 (mycards communal)
  (num-better mycards communal 1000))




;--------------------------------------------------------------------------
; helpers

; it is used with best to find the best hand.
; cards is the list of cards to choose from
; choose is the number of cards to choose from the list
; func is the function to call on each combination

(defun comb (choose cards func)
  (combo-solver cards nil choose func))

; recursive algorithm to call a functor on each possible combination
; ie every (length cur-list) C cur-combo-size.
(defun combo-solver (cur-list cur-combo cur-combo-size functor)
  (when (<= cur-combo-size (length cur-list))
    (if (zerop cur-combo-size) (return-from combo-solver (funcall functor cur-combo))) ; made a new combo so call functor
    (combo-solver (rest cur-list) cur-combo cur-combo-size functor ) ; decompose until the size of cur-combo-size then build the next combo
    (combo-solver (rest cur-list) (cons (first cur-list) cur-combo) (1- cur-combo-size) functor)))
    

; make sure i got it right
(defun comb-tester ()
  (print "first")
  (comb 5 '(1 2 3 4 5) #'print)
  (print "second")
  (length (comb 5 '(1 2 3 4 5 6) #'print))
  (print "third")
  (length (comb 5 '(1 2 3 4 5 6 7) #'print)))



; besthand is used to keep track of the best hand seen so far
(let ((besthand ))
  (defun best (hand)
    (if (or (equal nil besthand)
	    (= 1 (CompareHands hand besthand)))
    (setf besthand hand))
    besthand
    )
  (defun getbest()
    besthand)
  (defun resetbest()
    (setf besthand '())))

; creates a deck with removed cards removed from a deck of cards
(defun currentdeck (removed)
  (let ((deck (make-deck)))
    (dolist (card removed deck)
      (setf deck (remove card deck :test #'equal)))))

; returns the number of oponents hands that are better than
; the best hand that can be created with mycards and communal
; numoponents is the number of oponent hands to create
(defun num-better (mycards communal numoponents)
  (resetbest)
  (let* ((allmine (append mycards communal))
	 (deck (currentdeck allmine))
	 (score 0)
	 (mybest (comb 5 allmine #'best))
	 (opshand ; create a lambda function that will return the best random opponent hand
	  (lambda () 
	    (resetbest) ; must ensure best is reset
	    (let ((opscards); the opponents hand created by combining communal with 2 rand cards
		  (localdeck)); the current deck 
	      (setf opscards (elt deck (random (length deck)))); pick a random card
	      (setf localdeck (remove opscards deck :test #'equal)) ; remove that card from deck
	      ; then pick the second opponent card
	      (setf opscards (list opscards (elt localdeck (random (length localdeck)))))
	    (comb 5 (append opscards communal) #'best)))));finally get the best hand
    (dotimes (i numoponents score);loop numoponents times and return the number of times openents
      ; hand is better
      (resetbest); must reset the best
      (if (= 1 (CompareHands (funcall opshand) mybest));compare the hands
	  (incf score)))));if oponents hand is better increment score
	 
;;;;;; The rest is given code so don't need to test.

;; Evaluate hand from strongest to weakest
;; Input: 5 card hand
;; Return value:
;;   ((RoyalFlush Suit) ;; have royal flush give suit, otherwise nil
;;    (StraightFlush HighCard Suit)  ;; high card value & suit, otherwise nil
;;    (FourOfAKind CardVal)
;;    (FullHouse HighCardVal LowCardVal)   ;; HighCardVal - three of a kind value, LowCard - pair value
;;    (Flush HighCard Suit)
;;    (Straight HighCard)
;;    (ThreeOfAKind CardVal)
;;    (TwoPair HighCardVal LowCardVal)
;;    (Pair CardVal)
;;    HighCardVal)
(defun EvaluateHand (hand)
  (let ((cardvals (mapcar #'(lambda (x) (first x)) hand)) 
	(suits (mapcar #'(lambda (x) (second x)) hand))
	(suits-equal nil)
	(ret-val (list nil nil nil nil nil nil nil nil nil 
		       (if (eq (first (first hand)) 1) 14 (first (fifth hand))))))
    (setf suits-equal (every #'(lambda (x) (eq x (first suits))) (rest suits)))
    ;; check for Royal Flush
    (if (and suits-equal (eq (first cardvals) 10) (eq (fifth cardvals) 14))
	(setf (first ret-val) (list 'RoyalFlush (first suits))))
    (if (first ret-val) (return-from EvaluateHand ret-val))

    ;; check for Straight Flush
    (if (and suits-equal (eq (fifth cardvals) (+ (first cardvals) 4)))
	(setf (second ret-val) (list 'StraightFlush (fifth cardvals) (first suits))))
    (if (second ret-val) (return-from EvaluateHand ret-val))

    ;; check for Four of a Kind
    (cond ((eq (first cardvals) (fourth cardvals)) 
	   (setf (third ret-val) (list 'FourOfAKind (first cardvals))))
	  ((eq (second cardvals) (fifth cardvals)) 
	   (setf (third ret-val) (list 'FourOfAKind (second cardvals))))
	  (t nil))
    (if (third ret-val) (return-from EvaluateHand ret-val))

    ;; check for Full House
    (cond ((and (eq (first cardvals) (third cardvals))
		(eq (fourth cardvals) (fifth cardvals)))
	   (setf (fourth ret-val) (list 'FullHouse (first cardvals) (fourth cardvals))))
	  ((and (eq (first cardvals) (second cardvals))
		(eq (third cardvals) (fifth cardvals)))
	   (setf (fourth ret-val) (list 'FullHouse (third cardvals) (first cardvals))))
	  (t nil))
    (if (fourth ret-val) (return-from EvaluateHand ret-val))

    ;; check for Flush
    (if suits-equal (setf (fifth ret-val) (list 'Flush (fifth cardvals) (first suits))))
    (if (fifth ret-val) (return-from EvaluateHand ret-val))

    ;; check for Straight;;    CHECK THIS CAREFULLY
    (if (and (all-diff cardvals) (eq (fifth cardvals) (+ (first cardvals) 4)))
	(setf (sixth ret-val) (list 'Straight (fifth cardvals))))
    (if (sixth ret-val) (return-from EvaluateHand ret-val))

    ;; check for Three of a Kind
    (if (or (eq (first cardvals) (third cardvals))
	    (eq (second cardvals) (fourth cardvals))
	    (eq (third cardvals) (fifth cardvals)))
	(setf (seventh ret-val) (list 'ThreeOfAKind (third cardvals))))
    (if (seventh ret-val) (return-from EvaluateHand ret-val))

    ;; check for Two Pair
    (if (or (and (eq (first cardvals) (second cardvals))
		 (or (eq (third cardvals) (fourth cardvals))
		     (eq (fourth cardvals) (fifth cardvals))))
	    (and (eq (second cardvals) (third cardvals))
		 (eq (fourth cardvals) (fifth cardvals))))
	(setf (eighth ret-val) (list 'TwoPair (fourth cardvals) (second cardvals))))
    (if (eighth ret-val) (return-from EvaluateHand ret-val))

    ;; check for a Pair
    (cond ((eq (first cardvals) (second cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (first cardvals))))
	  ((eq (second cardvals) (third cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (second cardvals))))
	  ((eq (third cardvals) (fourth cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (third cardvals))))
	  ((eq (fourth cardvals) (fifth cardvals)) 
	   (setf (ninth ret-val) (list 'Pair (fourth cardvals))))
	  (t nil))
    (if (ninth ret-val) (return-from EvaluateHand ret-val))
    ret-val
    ))


;; function to break ties; compare two hands until one card is higher or done
;; return 1 if hand1 is better, -1 if hand2 is better and 0 otherwise
(defun BreakTies (cvals1 cvals2)
  ;; (format t "~% cvals1 = ~A    cvals2 = ~A ~%" cvals1 cvals2)  
  (cond ((or (null cvals1) (null cvals2)) 0)
	((> (first cvals1) (first cvals2)) 1)
	((< (first cvals1) (first cvals2)) -1)
	(t (BreakTies (rest cvals1) (rest cvals2))))
)

;; given two hands of five cards each, determine if hand1 is stronger than hand2 
;; there are three possible return values
;; 1: hand1 is better
;; -1: hand2 is better
;; 0: hands are equal
(defun CompareHands (hand1 hand2)
  (let ((nhand1 (NumericalHand hand1))
	(nhand2 (NumericalHand hand2)) (hs1 nil) (hs2 nil) (cvals1 nil) (cvals2))
    (setf hs1 (EvaluateHand nhand1))
    (setf hs2 (EvaluateHand nhand2))
    (setf cvals1 (reverse (mapcar #'(lambda(x) (first x)) nhand1)))
    (setf cvals2 (reverse (mapcar #'(lambda(x) (first x)) nhand2)))

    ;; (format t "~% nhand1 = ~A   cvals1 = ~A    hs1 = ~A" nhand1 cvals1 hs1)
    ;; (format t "~% nhand2 = ~A   cvals2 = ~A    hs2 = ~A" nhand2 cvals2 hs2)
    (cond ((or (first hs1) (first hs2))
	   ;; one player has a RoyalFlush
	   (cond ((null (first hs1)) -1)  ;; hand2 is better
		 ((null (first hs2))  1)  ;; hand1 is better
		 (t 0)))
	  ;; one player has a StraightFlush
	  ((or (second hs1) (second hs2))
	   (cond ((null (second hs1)) -1)   ;; player2 has SF
		 ((null (second hs2))  1)   ;; player1 has SF
		 ((> (second (second hs1)) (second (second hs2))) 1)  ;; higher card
		 ((< (second (second hs1)) (second (second hs2))) -1) ;; higher card
		 (t 0)))  ;; hands equal
	   ;; one player has Four of a Kind
	  ((or (third hs1) (third hs2))
	   (cond ((null (third hs1)) -1)   ;; player2 has FofaK
		 ((null (third hs2))  1)   ;; player1 has FofaK
		 ((> (second (third hs1)) (second (third hs2))) 1)  ;; higher card
		 ((< (second (third hs1)) (second (third hs2))) -1) ;; higher card
		 (t (BreakTies cvals1 cvals2)))) ;; break ties by finding the highest card that differs 
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 ;; (t 0)))   ;; hands equal
	  ;; one player has Full House
	  ((or (fourth hs1) (fourth hs2))
	   (cond ((null (fourth hs1)) -1)   ;; player2 has FH
		 ((null (fourth hs2))  1)   ;; player1 has FH
		 ((> (second (fourth hs1)) (second (fourth hs2))) 1)  ;; higher card
		 ((< (second (fourth hs1)) (second (fourth hs2))) -1) ;; higher card
		 (t (cond ((> (third (fourth hs1)) (third (fourth hs2))) 1)
			  ((< (third (fourth hs1)) (third (fourth hs2))) -1)
			  (t 0)))))   ;; hands equal
	  ;; one player has Flush
	  ((or (fifth hs1) (fifth hs2))
	   (cond ((null (fifth hs1)) -1)   ;; player2 has F
		 ((null (fifth hs2))  1)   ;; player1 has F
		 (t (BreakTies cvals1 cvals2))))   ;; break ties by finding the highest card that differs
		 ;; ((> (second (fifth hs1)) (second (fifth hs2))) 1)  ;; higher card
		 ;; ((< (second (fifth hs1)) (second (fifth hs2))) -1) ;; higher card
		 ;; (t 0)))   ;; hands equal
	  ;; one player has Straight
	  ((or (sixth hs1) (sixth hs2))
	   (cond ((null (sixth hs1)) -1)   ;; player2 has S
		 ((null (sixth hs2))  1)   ;; player1 has S
		 ((> (second (sixth hs1)) (second (sixth hs2))) 1)  ;; higher card
		 ((< (second (sixth hs1)) (second (sixth hs2))) -1) ;; higher card
		 (t 0)))   ;; hands equal
	  ;; one player has Three of a Kind
	  ((or (seventh hs1) (seventh hs2))
	   ;; (format t "~% Three of a Kind!!!! ~A ~A~%" (seventh hs1) (seventh hs2))
	   (cond ((null (seventh hs1)) -1)   ;; player2 has TofaK
		 ((null (seventh hs2))  1)   ;; player1 has TofaK
		 ((> (second (seventh hs1)) (second (seventh hs2))) 1)  ;; higher card
		 ((< (second (seventh hs1)) (second (seventh hs2))) -1) ;; higher card
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 (t (BreakTies cvals1 cvals2))))   ;; hands equal
	  ;; one player has TwoPair
	  ((or (eighth hs1) (eighth hs2))
	   (cond ((null (eighth hs1)) -1)   ;; player2 has TofaK
		 ((null (eighth hs2))  1)   ;; player1 has TofaK
		 ((> (second (eighth hs1)) (second (eighth hs2))) 1)  ;; higher hard card
		 ((< (second (eighth hs1)) (second (eighth hs2))) -1) ;; higher high card
		 ((> (third (eighth hs1)) (third (eighth hs2))) 1)  ;; higher low card
		 ((< (third (eighth hs1)) (third (eighth hs2))) -1) ;; higher low card
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 (t (BreakTies cvals1 cvals2))))  ;; hands equal
	  ;; one player has a Pair
	  ((or (ninth hs1) (ninth hs2))
	   (cond ((null (ninth hs1)) -1)   ;; player2 has Pair
		 ((null (ninth hs2))  1)   ;; player1 has Pair
		 ((> (second (ninth hs1)) (second (ninth hs2))) 1)  ;; higher card
		 ((< (second (ninth hs1)) (second (ninth hs2))) -1) ;; higher card
;;		 ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
;;		 ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		 (t (BreakTies cvals1 cvals2))))  ;; hands equal

	  ;; the strongest card wins
	  (t (cond ((< (tenth hs1) (tenth hs2)) -1) ;; player 2 has the high card
		   ((> (tenth hs1) (tenth hs2)) 1)  ;; player 1 has a high card
		   (t (BreakTies (reverse cvals1) (reverse cvals2))))))))  ;; hands equal
	   

 




;; Sort hand by card value
    (defun SortHand (hand)
     (sort hand #'(lambda (x y) (< (first x) (first y)))))

    ;; NumericalHand
    ;; replace A by 1, J by 11, Q by 12, K by 13
    (defun NumericalHand (hand)
    	   (let ((new-hand nil) (cardvals nil) (straight nil))
    	   (setf new-hand
	   	 (mapcar #'(lambda (x) 
		 	      (cond ((eq (first x) 'A) (list 1 (second x)))
			      	        ((eq (first x) 'J) (list 11 (second x)))
			    		((eq (first x) 'Q) (list 12 (second x)))
			    		((eq (first x) 'K) (list 13 (second x)))
			    		(t x)))
			       hand))
            (setf cardvals (sort (mapcar #'(lambda (x) (first x)) new-hand) #'<))
    	    (setf straight (and (eq (first cardvals) 1) (eq (second cardvals) 2) (eq (third cardvals) 3)
	    		   	         (eq (fourth cardvals) 4) (eq (fifth cardvals) 5)))
    	    (if straight (SortHand new-hand)   ;; keep A's as 1's - have straight
	    		 (SortHand (mapcar #'(lambda (x) (if (eq (first x) 1) (list  
	                                                         14 (second x)) x)) new-hand)))))



;; check if all cards are different
     (defun All-Diff (cardvals)
            (and (< (first cardvals) (second cardvals)) (< (second cardvals) (third cardvals))
       	    (< (third cardvals) (fourth cardvals)) (< (fourth cardvals) (fifth cardvals))))




;;REQUIRES: deck != nil and in the format
;;   ((Card1 Suit1) (Card2 Suit1) ... (Card1 Suit2) ... (Card1 Suit3) ... (Card13 Suit4))
;;   where Card1-Card13 is the sequence (A 2 3 4 5 6 7 8 9 10 J Q K)
;;   and Suit1-Suit4 is the sequence
;;   (S D H C) ie (Spades Diamonds Hearts Clubs)
;;EFFECTS: Returns a deck sorted in a random order
(defun RandomShuffle (deck)
  (let ((shuffled '()) (cur-deck-length (length deck)) (cur-elt))
    (dotimes (i cur-deck-length shuffled) ;iterate over the size of a deck
      (setf cur-elt (elt deck (random (length deck))));get a random card
      (setf shuffled (cons cur-elt shuffled));add the card to the shuffled deck
      (setf deck (remove cur-elt deck));remove the card from the unshuffled deck
      )))

;; EFFECTS: Returns a 52 card deck
;; eg. ((Card1 Suit1) (Card2 Suit1) ... (Card1 Suit2) ... (Card1 Suit3) ... (Card13 Suit4))
;;   where Card1-Card13 is the sequence (A 2 3 4 5 6 7 8 9 10 J Q K)
;;   and Suit1-Suit4 is the sequence
;;   (S D H C) ie (Spades Diamonds Hearts Clubs)
(defun make-deck()
  (let ((deck '()))
    (dolist (suit '(S D H C) deck)
      (dolist (val '(A 2 3 4 5 6 7 8 9 10 J Q K))
	(setf deck (cons (list val suit) deck))))))


(defun getpairs ()  '(((2 S) (3 H)) ((2 S) (3 C)) ((2 S) (3 D)) ((3 S) (2 H)) ((3 S) (2 C)) ((3 S) (2 D)) ((2 H) (3 C)) ((2 H) (3 D))
 ((3 H) (2 C)) ((3 H) (2 D)) ((2 C) (3 D)) ((3 C) (2 D)) ((2 D) (3 D)) ((2 C) (3 C)) ((2 H) (3 H)) ((2 S) (3 S))
 ((2 S) (4 H)) ((2 S) (4 C)) ((2 S) (4 D)) ((4 S) (2 H)) ((4 S) (2 C)) ((4 S) (2 D)) ((2 H) (4 C)) ((2 H) (4 D))
 ((4 H) (2 C)) ((4 H) (2 D)) ((2 C) (4 D)) ((4 C) (2 D)) ((2 D) (4 D)) ((2 C) (4 C)) ((2 H) (4 H)) ((2 S) (4 S))
 ((2 S) (5 H)) ((2 S) (5 C)) ((2 S) (5 D)) ((5 S) (2 H)) ((5 S) (2 C)) ((5 S) (2 D)) ((2 H) (5 C)) ((2 H) (5 D))
 ((5 H) (2 C)) ((5 H) (2 D)) ((2 C) (5 D)) ((5 C) (2 D)) ((2 D) (5 D)) ((2 C) (5 C)) ((2 H) (5 H)) ((2 S) (5 S))
 ((2 S) (6 H)) ((2 S) (6 C)) ((2 S) (6 D)) ((6 S) (2 H)) ((6 S) (2 C)) ((6 S) (2 D)) ((2 H) (6 C)) ((2 H) (6 D))
 ((6 H) (2 C)) ((6 H) (2 D)) ((2 C) (6 D)) ((6 C) (2 D)) ((2 D) (6 D)) ((2 C) (6 C)) ((2 H) (6 H)) ((2 S) (6 S))
 ((2 S) (7 H)) ((2 S) (7 C)) ((2 S) (7 D)) ((7 S) (2 H)) ((7 S) (2 C)) ((7 S) (2 D)) ((2 H) (7 C)) ((2 H) (7 D))
 ((7 H) (2 C)) ((7 H) (2 D)) ((2 C) (7 D)) ((7 C) (2 D)) ((2 D) (7 D)) ((2 C) (7 C)) ((2 H) (7 H)) ((2 S) (7 S))
 ((2 S) (8 H)) ((2 S) (8 C)) ((2 S) (8 D)) ((8 S) (2 H)) ((8 S) (2 C)) ((8 S) (2 D)) ((2 H) (8 C)) ((2 H) (8 D))
 ((8 H) (2 C)) ((8 H) (2 D)) ((2 C) (8 D)) ((8 C) (2 D)) ((2 D) (8 D)) ((2 C) (8 C)) ((2 H) (8 H)) ((2 S) (8 S))
 ((2 S) (9 H)) ((2 S) (9 C)) ((2 S) (9 D)) ((9 S) (2 H)) ((9 S) (2 C)) ((9 S) (2 D)) ((2 H) (9 C)) ((2 H) (9 D))
 ((9 H) (2 C)) ((9 H) (2 D)) ((2 C) (9 D)) ((9 C) (2 D)) ((2 D) (9 D)) ((2 C) (9 C)) ((2 H) (9 H)) ((2 S) (9 S))
 ((2 S) (10 H)) ((2 S) (10 C)) ((2 S) (10 D)) ((10 S) (2 H)) ((10 S) (2 C)) ((10 S) (2 D)) ((2 H) (10 C)) ((2 H) (10 D))
 ((10 H) (2 C)) ((10 H) (2 D)) ((2 C) (10 D)) ((10 C) (2 D)) ((2 D) (10 D)) ((2 C) (10 C)) ((2 H) (10 H)) ((2 S) (10 S))
 ((2 S) (J H)) ((2 H) (J C)) ((J H) (2 C)) ((2 C) (J D)) ((J C) (2 D)) ((2 S) (J C)) ((2 S) (J D)) ((2 H) (J D))
 ((J S) (2 D)) ((J H) (2 D)) ((J S) (2 H)) ((J S) (2 C)) ((2 D) (J D)) ((2 C) (J C)) ((2 H) (J H)) ((2 S) (J S))
 ((4 S) (3 C)) ((3 S) (4 H)) ((3 S) (4 C)) ((3 S) (4 D)) ((4 S) (3 H)) ((4 S) (3 D)) ((3 H) (4 C)) ((3 H) (4 D))
 ((4 H) (3 C)) ((4 H) (3 D)) ((3 C) (4 D)) ((4 C) (3 D)) ((3 H) (4 H)) ((3 D) (4 D)) ((3 C) (4 C)) ((3 S) (4 S))
 ((5 S) (3 H)) ((5 S) (3 C)) ((5 S) (3 D)) ((3 C) (5 D)) ((5 C) (3 D)) ((3 S) (5 H)) ((3 H) (5 C)) ((3 H) (5 D))
 ((5 H) (3 C)) ((5 H) (3 D)) ((3 S) (5 C)) ((3 S) (5 D)) ((3 H) (5 H)) ((3 D) (5 D)) ((3 S) (5 S)) ((3 C) (5 C))
 ((3 H) (6 C)) ((3 H) (6 D)) ((6 H) (3 C)) ((6 H) (3 D)) ((6 S) (3 H)) ((3 S) (6 D)) ((6 S) (3 C)) ((6 S) (3 D))
 ((3 C) (6 D)) ((6 C) (3 D)) ((3 S) (6 C)) ((3 S) (6 H)) ((3 H) (6 H)) ((3 D) (6 D)) ((3 C) (6 C)) ((3 S) (6 S))
 ((3 H) (7 C)) ((3 H) (7 D)) ((7 H) (3 C)) ((7 H) (3 D)) ((3 S) (7 C)) ((7 S) (3 H)) ((7 S) (3 C)) ((3 C) (7 D))
 ((7 C) (3 D)) ((3 S) (7 H)) ((3 S) (7 D)) ((7 S) (3 D)) ((7 H) (3 C)) ((7 H) (3 D)) ((3 S) (7 C)) ((7 S) (3 H))
 ((7 S) (3 C)) ((3 C) (7 D)) ((7 C) (3 D)) ((3 S) (7 H)) ((3 S) (7 D)) ((7 S) (3 D)) ((3 D) (7 D)) ((3 C) (7 C))
 ((3 S) (7 S)) ((3 H) (7 H)) ((3 C) (7 C)) ((3 S) (7 S)) ((3 H) (7 H)) ((8 S) (3 H)) ((8 S) (3 C)) ((8 S) (3 D))
 ((3 H) (8 C)) ((3 H) (8 D)) ((8 H) (3 C)) ((8 H) (3 D)) ((8 S) (3 H)) ((8 S) (3 C)) ((8 S) (3 D)) ((3 H) (8 C))
 ((3 H) (8 D)) ((8 H) (3 C)) ((8 H) (3 D)) ((8 C) (3 D)) ((3 C) (8 D)) ((3 S) (8 H)) ((3 S) (8 C)) ((3 S) (8 D))
 ((3 S) (8 S)) ((3 C) (8 C)) ((3 D) (8 D)) ((3 H) (8 H)) ((3 C) (9 D)) ((3 S) (9 C)) ((3 S) (9 D)) ((3 S) (9 H))
 ((9 C) (3 D)) ((9 H) (3 D)) ((9 H) (3 C)) ((3 H) (9 D)) ((3 H) (9 C)) ((9 S) (3 D)) ((9 S) (3 C)) ((9 S) (3 H))
 ((3 S) (9 S)) ((3 C) (9 C)) ((3 D) (9 D)) ((3 H) (9 H)) ((3 S) (10 H)) ((3 S) (10 D)) ((3 S) (10 C)) ((3 C) (10 D))
 ((10 C) (3 D)) ((10 H) (3 D)) ((10 H) (3 C)) ((3 H) (10 D)) ((3 H) (10 C)) ((10 S) (3 D)) ((10 S) (3 C)) ((10 S) (3 H))
 ((3 S) (10 S)) ((3 D) (10 D)) ((3 C) (10 C)) ((3 H) (10 H)) ((3 S) (J H)) ((3 S) (J D)) ((3 S) (J C)) ((J C) (3 D))
 ((3 C) (J D)) ((J H) (3 D)) ((3 H) (J D)) ((J S) (3 D)) ((J S) (3 C)) ((J S) (3 H)) ((3 H) (J C)) ((3 S) (J S))
 ((3 C) (J C)) ((3 H) (J H)) ((5 C) (4 D)) ((4 C) (5 D)) ((5 S) (4 D)) ((5 S) (4 C)) ((5 S) (4 H)) ((4 S) (5 D))
 ((4 S) (5 C)) ((4 S) (5 H)) ((5 H) (4 D)) ((5 H) (4 C)) ((4 H) (5 D)) ((4 H) (5 C)) ((4 S) (5 S)) ((4 C) (5 C))
 ((4 H) (5 H)) ((4 C) (6 D)) ((6 S) (4 D)) ((6 S) (4 C)) ((6 S) (4 H)) ((4 S) (6 D)) ((4 S) (6 C)) ((4 S) (6 H))
 ((6 H) (4 C)) ((4 H) (6 D)) ((4 H) (6 C)) ((4 S) (6 S)) ((4 H) (7 C)) ((7 S) (4 D)) ((7 S) (4 C)) ((7 S) (4 H))
 ((4 S) (7 D)) ((4 S) (7 C)) ((4 S) (7 H)) ((4 S) (7 S)) ((4 C) (8 D)) ((4 H) (8 D)) ((4 H) (8 C)) ((4 S) (8 D))
 ((4 S) (8 C)) ((4 S) (8 H)) ((8 C) (4 D)) ((8 H) (4 D)) ((8 H) (4 C)) ((8 S) (4 D)) ((8 S) (4 C)) ((8 S) (4 H))
 ((4 D) (8 D)) ((4 C) (8 C)) ((4 H) (8 H)) ((4 S) (8 S)) ((4 C) (9 D)) ((4 H) (9 D)) ((4 H) (9 C)) ((9 C) (4 D))
 ((9 H) (4 D)) ((4 S) (9 D)) ((4 S) (9 C)) ((4 S) (9 H)) ((9 H) (4 C)) ((9 S) (4 D)) ((9 S) (4 C)) ((9 S) (4 H))
 ((4 C) (9 C)) ((4 H) (9 H)) ((4 D) (9 D)) ((4 S) (9 S)) ((4 C) (10 D)) ((4 H) (10 D)) ((4 H) (10 C)) ((10 C) (4 D))
 ((10 H) (4 D)) ((10 H) (4 C)) ((4 S) (10 D)) ((4 S) (10 C)) ((4 S) (10 H)) ((10 S) (4 D)) ((10 S) (4 C)) ((10 S) (4 H))
 ((4 D) (10 D)) ((4 C) (10 C)) ((4 H) (10 H)) ((4 S) (10 S)) ((J H) (4 C)) ((J C) (4 D)) ((4 C) (J D)) ((4 H) (J D))
 ((4 H) (J C)) ((J H) (4 D)) ((4 S) (J D)) ((4 S) (J C)) ((4 S) (J H)) ((J S) (4 D)) ((J S) (4 H)) ((J S) (4 C))
 ((4 D) (J D)) ((4 C) (J C)) ((4 S) (J S)) ((4 H) (J H)) ((5 H) (6 C)) ((6 C) (5 D)) ((5 C) (6 D)) ((6 H) (5 D))
 ((6 H) (5 C)) ((6 S) (5 D)) ((6 S) (5 C)) ((5 H) (6 D)) ((6 S) (5 H)) ((5 S) (6 D)) ((5 S) (6 C)) ((5 S) (6 H))
 ((5 C) (6 C)) ((5 D) (6 D)) ((5 S) (6 S)) ((5 H) (6 H)) ((5 H) (7 C)) ((7 C) (5 D)) ((5 C) (7 D)) ((7 S) (5 D))
 ((7 S) (5 C)) ((7 H) (5 D)) ((7 H) (5 C)) ((5 H) (7 D)) ((5 S) (7 H)) ((7 S) (5 H)) ((5 S) (7 D)) ((5 S) (7 C))
 ((5 C) (7 C)) ((5 D) (7 D)) ((5 S) (7 S)) ((5 H) (7 H)) ((5 C) (8 D)) ((8 C) (5 D)) ((5 S) (8 D)) ((5 H) (8 D))
 ((5 H) (8 C)) ((8 H) (5 D)) ((8 H) (5 C)) ((8 S) (5 D)) ((8 S) (5 C)) ((8 S) (5 H)) ((5 S) (8 C)) ((5 S) (8 H))
 ((5 C) (8 C)) ((5 H) (8 H)) ((5 S) (8 S)) ((5 D) (8 D)) ((9 C) (5 D)) ((5 C) (9 D)) ((5 H) (9 D)) ((5 H) (9 C))
 ((5 S) (9 D)) ((5 S) (9 C)) ((9 H) (5 D)) ((9 H) (5 C)) ((9 S) (5 D)) ((9 S) (5 C)) ((9 S) (5 H)) ((5 S) (9 H))
 ((5 C) (9 C)) ((5 H) (9 H)) ((5 S) (9 S)) ((5 D) (9 D)) ((5 S) (10 D)) ((5 S) (10 C)) ((5 S) (10 H)) ((10 C) (5 D))
 ((5 C) (10 D)) ((10 H) (5 D)) ((10 H) (5 C)) ((10 S) (5 D)) ((10 S) (5 C)) ((10 S) (5 H)) ((5 H) (10 D)) ((5 H) (10 C))
 ((5 S) (10 S)) ((5 D) (10 D)) ((5 C) (10 C)) ((5 H) (10 H)) ((J H) (5 C)) ((5 H) (J D)) ((5 H) (J C)) ((J H) (5 D))
 ((J S) (5 D)) ((J S) (5 C)) ((J S) (5 H)) ((5 S) (J D)) ((5 S) (J C)) ((5 S) (J H)) ((J C) (5 D)) ((5 C) (J D))
 ((5 S) (J S)) ((5 D) (J D)) ((5 C) (J C)) ((5 H) (J H)) ((7 H) (6 D)) ((6 H) (7 D)) ((6 H) (7 C)) ((7 S) (6 D))
 ((7 S) (6 C)) ((7 S) (6 H)) ((6 S) (7 D)) ((6 S) (7 C)) ((6 S) (7 H)) ((7 C) (6 D)) ((6 C) (7 D)) ((7 H) (6 C))
 ((6 S) (7 S)) ((6 C) (7 C)) ((6 D) (7 D)) ((6 S) (8 C)) ((6 S) (8 H)) ((8 S) (6 C)) ((8 S) (6 H)) ((6 H) (8 D))
 ((6 H) (8 C)) ((6 S) (8 D)) ((8 C) (6 D)) ((6 C) (8 D)) ((8 H) (6 D)) ((8 S) (6 D)) ((8 H) (6 C)) ((6 C) (8 C))
 ((6 H) (8 H)) ((6 S) (8 S)) ((6 H) (9 D)) ((6 H) (9 C)) ((6 S) (9 D)) ((6 S) (9 C)) ((6 S) (9 H)) ((9 C) (6 D))
 ((6 C) (9 D)) ((9 H) (6 D)) ((9 S) (6 D)) ((9 H) (6 C)) ((9 S) (6 C)) ((9 S) (6 H)) ((6 D) (9 D)) ((6 H) (9 H))
 ((6 S) (9 S)) ((6 C) (9 C)) ((6 H) (10 D)) ((6 H) (10 C)) ((6 S) (10 D)) ((6 S) (10 C)) ((6 S) (10 H)) ((10 S) (6 D))
 ((10 S) (6 C)) ((10 C) (6 D)) ((6 C) (10 D)) ((10 H) (6 D)) ((10 H) (6 C)) ((10 S) (6 H)) ((6 C) (10 C)) ((6 D) (10 D))
 ((6 H) (10 H)) ((6 S) (10 S)) ((6 H) (J C)) ((J H) (6 D)) ((6 H) (J D)) ((J H) (6 C)) ((6 S) (J D)) ((6 S) (J C))
 ((6 S) (J H)) ((J S) (6 D)) ((J S) (6 C)) ((J S) (6 H)) ((J C) (6 D)) ((6 C) (J D)) ((6 S) (J S)) ((6 C) (J C))
 ((6 H) (J H)) ((6 D) (J D)) ((8 C) (7 D)) ((7 C) (8 D)) ((8 S) (7 H)) ((8 H) (7 C)) ((7 S) (8 C)) ((7 S) (8 H))
 ((7 H) (8 C)) ((7 D) (8 D)) ((7 S) (8 S)) ((7 C) (8 C)) ((7 H) (8 H)) ((7 S) (9 D)) ((7 S) (9 C)) ((7 S) (9 H))
 ((7 H) (9 H)) ((7 S) (9 S)) ((7 C) (10 D)) ((10 C) (7 D)) ((10 S) (7 D)) ((10 S) (7 C)) ((10 S) (7 H)) ((10 H) (7 D))
 ((10 H) (7 C)) ((7 S) (10 D)) ((7 H) (10 D)) ((7 H) (10 C)) ((7 S) (10 C)) ((7 S) (10 H)) ((7 H) (10 H)) ((7 S) (10 S))
 ((7 C) (10 C)) ((7 D) (10 D)) ((J S) (7 C)) ((7 H) (J D)) ((7 H) (J C)) ((J H) (7 D)) ((J C) (7 D)) ((J S) (7 D))
 ((J S) (7 H)) ((7 S) (J D)) ((7 C) (J D)) ((J H) (7 C)) ((7 S) (J H)) ((7 S) (J C)) ((7 C) (J C)) ((7 H) (J H))
 ((7 S) (J S)) ((8 C) (9 D)) ((8 S) (9 H)) ((9 H) (8 D)) ((8 H) (9 D)) ((9 S) (8 D)) ((9 C) (8 D)) ((9 S) (8 H))
 ((8 S) (9 D)) ((8 S) (9 C)) ((9 S) (8 C)) ((8 H) (9 C)) ((8 D) (9 D)) ((8 C) (9 C)) ((8 S) (9 S)) ((8 S) (10 C))
 ((10 H) (8 D)) ((10 H) (8 C)) ((8 H) (10 D)) ((8 S) (10 H)) ((10 S) (8 D)) ((8 S) (10 D)) ((10 S) (8 C)) ((10 S) (8 H))
 ((8 S) (10 S)) ((8 D) (10 D)) ((8 H) (10 H)) ((8 H) (J D)) ((J C) (8 D)) ((8 S) (J D)) ((J H) (8 D)) ((J H) (8 C))
 ((8 S) (J H)) ((J S) (8 C)) ((J S) (8 H)) ((8 S) (J C)) ((8 D) (J D)) ((8 H) (J H)) ((9 H) (10 C)) ((9 S) (10 C))
 ((9 S) (10 H)) ((9 S) (10 D)) ((9 C) (10 D)) ((10 C) (9 D)) ((10 H) (9 D)) ((10 H) (9 C)) ((9 H) (10 D)) ((J H) (9 D))
 ((J H) (9 C)) ((10 S) (J D)) ((J H) (10 C)) ((J S) (10 H)) ((2 S) (Q D)) ((9 C) (7 D)) ((9 S) (7 D)) ((7 C) (9 D))
 ((9 S) (7 C)) ((9 S) (7 H)) ((9 H) (7 D)) ((9 H) (7 C)) ((7 D) (9 D)) ((7 C) (9 C)) ((9 H) (8 C)) ((2 S) (Q H))
 ((9 D) (J D)) ((J S) (10 C)) ((10 S) (J C)) ((2 C) (Q D)) ((Q H) (2 D)) ((8 S) (7 C)) ((3 S) (Q H)) ((7 D) (J D))
 ((4 S) (Q D)) ((6 H) (Q C)) ((8 C) (10 C)) ((2 S) (Q C)) ((2 S) (Q S)) ((10 D) (J D)) ((2 C) (Q C)) ((3 H) (Q D))
 ((3 H) (Q C)) ((3 S) (Q D)) ((3 S) (Q C)) ((6 H) (7 H)) ((7 S) (8 D)) ((Q H) (7 D)) ((10 C) (8 D)) ((8 C) (10 D))
 ((8 H) (10 C)) ((J S) (8 D)) ((8 C) (J D)) ((8 H) (J C)) ((8 C) (J C)) ((8 S) (J S)) ((3 C) (Q D)) ((10 S) (9 D))
 ((10 S) (9 C)) ((10 S) (9 H)) ((9 S) (10 S)) ((9 D) (10 D)) ((8 D) (Q D)) ((10 C) (J C)) ((Q S) (2 D)) ((10 H) (J C))
 ((J S) (10 D)) ((Q H) (2 C)) ((J H) (10 D)) ((2 H) (Q D)) ((2 H) (Q H)) ((2 D) (Q D)) ((Q H) (3 D)) ((Q S) (3 D))
 ((Q S) (3 C)) ((Q S) (3 H)) ((Q H) (3 C)) ((Q C) (3 D)) ((3 H) (Q H)) ((9 C) (10 C)) ((9 H) (J D)) ((9 H) (J C))
 ((J S) (9 D)) ((J S) (9 C)) ((J S) (9 H)) ((9 S) (J D)) ((9 S) (J C)) ((9 S) (J H)) ((J C) (9 D)) ((9 C) (J D))
 ((9 S) (J S)) ((9 C) (J C)) ((10 H) (J D)) ((J C) (10 D)) ((10 C) (J D)) ((3 D) (Q D)) ((10 S) (J H)) ((10 S) (J S))
 ((2 H) (Q C)) ((Q S) (2 C)) ((Q C) (2 D)) ((3 S) (Q S)) ((4 C) (Q D)) ((Q H) (4 D)) ((4 H) (Q C)) ((4 H) (Q D))
 ((Q H) (4 C)) ((Q C) (4 D)) ((4 S) (Q C)) ((4 S) (Q H)) ((Q S) (4 D)) ((4 D) (Q D)) ((4 C) (Q C)) ((4 H) (Q H))
 ((5 C) (Q D)) ((5 H) (Q D)) ((5 H) (Q C)) ((5 S) (Q D)) ((5 S) (Q C)) ((5 S) (Q H)) ((Q H) (5 D)) ((Q C) (5 D))
 ((5 D) (Q D)) ((9 H) (10 H)) ((Q S) (4 C)) ((Q S) (4 H)) ((6 D) (8 D)) ((8 H) (7 D)) ((8 S) (7 D)) ((3 C) (Q C))
 ((4 S) (Q S)) ((Q H) (5 C)) ((Q S) (5 D)) ((Q S) (5 C)) ((Q S) (5 H)) ((5 H) (Q H)) ((5 S) (Q S)) ((5 C) (Q C))
 ((Q H) (6 D)) ((10 H) (J H)) ((Q H) (6 C)) ((6 S) (Q C)) ((6 S) (Q D)) ((Q S) (6 D)) ((Q S) (6 C)) ((Q S) (6 H))
 ((Q C) (6 D)) ((6 C) (Q D)) ((6 H) (Q D)) ((6 S) (Q H)) ((6 S) (Q S)) ((6 D) (Q D)) ((6 C) (Q C)) ((6 H) (Q H))
 ((Q H) (7 C)) ((Q S) (7 D)) ((Q S) (7 C)) ((Q S) (7 H)) ((Q C) (7 D)) ((7 C) (Q D)) ((7 H) (Q D)) ((7 H) (Q C))
 ((7 S) (Q D)) ((7 S) (Q C)) ((7 S) (Q H)) ((7 D) (Q D)) ((8 H) (9 H)) ((7 C) (Q C)) ((7 H) (Q H)) ((7 S) (Q S))
 ((Q H) (8 C)) ((9 H) (J H)) ((Q C) (8 D)) ((Q H) (8 D)) ((8 H) (Q D)) ((8 H) (Q C)) ((8 C) (Q D)) ((Q S) (8 C))
 ((Q S) (8 H)) ((Q S) (8 D)) ((8 S) (Q D)) ((8 S) (Q C)) ((8 S) (Q H)) ((8 H) (Q H)) ((8 C) (Q C)) ((8 S) (Q S))
 ((Q C) (9 D)) ((9 C) (Q D)) ((Q H) (9 D)) ((Q H) (9 C)) ((9 H) (Q D)) ((9 H) (Q C)) ((Q S) (9 D)) ((Q S) (9 C))
 ((Q S) (9 H)) ((9 S) (Q D)) ((9 S) (Q C)) ((9 S) (Q H)) ((9 D) (Q D)) ((9 C) (Q C)) ((9 H) (Q H)) ((9 S) (Q S))
 ((Q C) (10 D)) ((10 C) (Q D)) ((Q H) (10 D)) ((Q H) (10 C)) ((10 H) (Q D)) ((10 H) (Q C)) ((Q S) (10 D)) ((Q S) (10 C))
 ((Q S) (10 H)) ((10 S) (Q D)) ((10 S) (Q C)) ((10 S) (Q H)) ((10 D) (Q D)) ((10 C) (Q C)) ((10 H) (Q H)) ((10 S) (Q S))
 ((Q C) (J D)) ((J C) (Q D)) ((Q H) (J D)) ((Q H) (J C)) ((J H) (Q D)) ((J H) (Q C)) ((Q S) (J D)) ((Q S) (J C))
 ((Q S) (J H)) ((J S) (Q D)) ((J S) (Q C)) ((J S) (Q H)) ((J D) (Q D)) ((J C) (Q C)) ((J H) (Q H)) ((J S) (Q S))
 ((K C) (2 D)) ((2 C) (K D)) ((K H) (2 D)) ((K H) (2 C)) ((2 H) (K D)) ((2 H) (K C)) ((K S) (2 D)) ((K S) (2 C))
 ((K S) (2 H)) ((2 S) (K D)) ((2 S) (K C)) ((2 S) (K H)) ((2 D) (K D)) ((2 C) (K C)) ((2 H) (K H)) ((2 S) (K S))
 ((K C) (3 D)) ((3 C) (K D)) ((K H) (3 D)) ((K H) (3 C)) ((3 H) (K D)) ((3 H) (K C)) ((K S) (3 D)) ((K S) (3 C))
 ((K S) (3 H)) ((3 S) (K D)) ((3 S) (K C)) ((3 S) (K H)) ((3 D) (K D)) ((3 C) (K C)) ((3 H) (K H)) ((3 S) (K S))
 ((K C) (4 D)) ((4 C) (K D)) ((K H) (4 D)) ((K H) (4 C)) ((4 H) (K D)) ((4 H) (K C)) ((K S) (4 D)) ((K S) (4 C))
 ((K S) (4 H)) ((4 S) (K D)) ((4 S) (K C)) ((4 S) (K H)) ((4 D) (K D)) ((4 C) (K C)) ((4 H) (K H)) ((4 S) (K S))
 ((K C) (5 D)) ((5 C) (K D)) ((K H) (5 D)) ((K H) (5 C)) ((5 H) (K D)) ((5 H) (K C)) ((K S) (5 D)) ((K S) (5 C))
 ((K S) (5 H)) ((5 S) (K D)) ((5 S) (K C)) ((5 S) (K H)) ((5 D) (K D)) ((5 C) (K C)) ((5 H) (K H)) ((5 S) (K S))
 ((K C) (6 D)) ((6 C) (K D)) ((K H) (6 D)) ((K H) (6 C)) ((6 H) (K D)) ((6 H) (K C)) ((K S) (6 D)) ((K S) (6 C))
 ((K S) (6 H)) ((6 S) (K D)) ((6 S) (K C)) ((6 S) (K H)) ((6 D) (K D)) ((6 C) (K C)) ((6 H) (K H)) ((6 S) (K S))
 ((K C) (7 D)) ((7 C) (K D)) ((K H) (7 D)) ((K H) (7 C)) ((7 H) (K D)) ((7 H) (K C)) ((K S) (7 D)) ((K S) (7 C))
 ((K S) (7 H)) ((7 S) (K D)) ((7 S) (K C)) ((7 S) (K H)) ((7 D) (K D)) ((7 C) (K C)) ((7 H) (K H)) ((7 S) (K S))
 ((K C) (8 D)) ((8 C) (K D)) ((K H) (8 D)) ((K H) (8 C)) ((8 H) (K D)) ((8 H) (K C)) ((K S) (8 D)) ((K S) (8 C))
 ((K S) (8 H)) ((8 S) (K D)) ((8 S) (K C)) ((8 S) (K H)) ((8 D) (K D)) ((8 C) (K C)) ((8 H) (K H)) ((8 S) (K S))
 ((K C) (9 D)) ((9 C) (K D)) ((K H) (9 D)) ((K H) (9 C)) ((9 H) (K D)) ((9 H) (K C)) ((K S) (9 D)) ((K S) (9 C))
 ((K S) (9 H)) ((9 S) (K D)) ((9 S) (K C)) ((9 S) (K H)) ((9 D) (K D)) ((9 C) (K C)) ((9 H) (K H)) ((9 S) (K S))
 ((K C) (10 D)) ((10 C) (K D)) ((K H) (10 D)) ((K H) (10 C)) ((10 H) (K D)) ((10 H) (K C)) ((K S) (10 D)) ((K S) (10 C))
 ((K S) (10 H)) ((10 S) (K D)) ((10 S) (K C)) ((10 S) (K H)) ((10 D) (K D)) ((10 C) (K C)) ((10 H) (K H)) ((10 S) (K S))
 ((K C) (J D)) ((J C) (K D)) ((K H) (J D)) ((K H) (J C)) ((J H) (K D)) ((J H) (K C)) ((K S) (J D)) ((K S) (J C))
 ((K S) (J H)) ((J S) (K D)) ((J S) (K C)) ((J S) (K H)) ((J D) (K D)) ((J C) (K C)) ((J H) (K H)) ((J S) (K S))
 ((K C) (Q D)) ((Q C) (K D)) ((K H) (Q D)) ((K H) (Q C)) ((Q H) (K D)) ((Q H) (K C)) ((K S) (Q D)) ((K S) (Q C))
 ((K S) (Q H)) ((Q S) (K D)) ((Q S) (K C)) ((Q S) (K H)) ((Q D) (K D)) ((Q C) (K C)) ((Q H) (K H)) ((Q S) (K S))
 ((2 C) (A D)) ((A C) (2 D)) ((2 H) (A D)) ((2 H) (A C)) ((A H) (2 D)) ((A H) (2 C)) ((2 S) (A D)) ((2 S) (A C))
 ((2 S) (A H)) ((A S) (2 D)) ((A S) (2 C)) ((A S) (2 H)) ((A D) (2 D)) ((A C) (2 C)) ((A H) (2 H)) ((A S) (2 S))
 ((3 C) (A D)) ((A C) (3 D)) ((3 H) (A D)) ((3 H) (A C)) ((A H) (3 D)) ((A H) (3 C)) ((3 S) (A D)) ((3 S) (A C))
 ((3 S) (A H)) ((A S) (3 D)) ((A S) (3 C)) ((A S) (3 H)) ((A D) (3 D)) ((A C) (3 C)) ((A H) (3 H)) ((A S) (3 S))
 ((4 C) (A D)) ((A C) (4 D)) ((4 H) (A D)) ((4 H) (A C)) ((A H) (4 D)) ((A H) (4 C)) ((4 S) (A D)) ((4 S) (A C))
 ((4 S) (A H)) ((A S) (4 D)) ((A S) (4 C)) ((A S) (4 H)) ((A D) (4 D)) ((A C) (4 C)) ((A H) (4 H)) ((A S) (4 S))
 ((5 C) (A D)) ((A C) (5 D)) ((5 H) (A D)) ((5 H) (A C)) ((A H) (5 D)) ((A H) (5 C)) ((5 S) (A D)) ((5 S) (A C))
 ((5 S) (A H)) ((A S) (5 D)) ((A S) (5 C)) ((A S) (5 H)) ((A D) (5 D)) ((A C) (5 C)) ((A H) (5 H)) ((A S) (5 S))
 ((6 C) (A D)) ((A C) (6 D)) ((6 H) (A D)) ((6 H) (A C)) ((A H) (6 D)) ((A H) (6 C)) ((6 S) (A D)) ((6 S) (A C))
 ((6 S) (A H)) ((A S) (6 D)) ((A S) (6 C)) ((A S) (6 H)) ((A D) (6 D)) ((A C) (6 C)) ((A H) (6 H)) ((A S) (6 S))
 ((7 C) (A D)) ((A C) (7 D)) ((7 H) (A D)) ((7 H) (A C)) ((A H) (7 D)) ((A H) (7 C)) ((7 S) (A D)) ((7 S) (A C))
 ((7 S) (A H)) ((A S) (7 D)) ((A S) (7 C)) ((A S) (7 H)) ((A D) (7 D)) ((A C) (7 C)) ((A H) (7 H)) ((A S) (7 S))
 ((8 C) (A D)) ((A C) (8 D)) ((8 H) (A D)) ((8 H) (A C)) ((A H) (8 D)) ((A H) (8 C)) ((8 S) (A D)) ((8 S) (A C))
 ((8 S) (A H)) ((A S) (8 D)) ((A S) (8 C)) ((A S) (8 H)) ((A D) (8 D)) ((A C) (8 C)) ((A H) (8 H)) ((A S) (8 S))
 ((9 C) (A D)) ((A C) (9 D)) ((9 H) (A D)) ((9 H) (A C)) ((A H) (9 D)) ((A H) (9 C)) ((9 S) (A D)) ((9 S) (A C))
 ((9 S) (A H)) ((A S) (9 D)) ((A S) (9 C)) ((A S) (9 H)) ((A D) (9 D)) ((A C) (9 C)) ((A H) (9 H)) ((A S) (9 S))
 ((10 C) (A D)) ((A C) (10 D)) ((10 H) (A D)) ((10 H) (A C)) ((A H) (10 D)) ((A H) (10 C)) ((10 S) (A D)) ((10 S) (A C))
 ((10 S) (A H)) ((A S) (10 D)) ((A S) (10 C)) ((A S) (10 H)) ((A D) (10 D)) ((A C) (10 C)) ((A H) (10 H)) ((A S) (10 S))
 ((J C) (A D)) ((A C) (J D)) ((J H) (A D)) ((J H) (A C)) ((A H) (J D)) ((A H) (J C)) ((J S) (A D)) ((J S) (A C))
 ((J S) (A H)) ((A S) (J D)) ((A S) (J C)) ((A S) (J H)) ((A D) (J D)) ((A C) (J C)) ((A H) (J H)) ((A S) (J S))
 ((Q C) (A D)) ((A C) (Q D)) ((Q H) (A D)) ((Q H) (A C)) ((A H) (Q D)) ((A H) (Q C)) ((Q S) (A D)) ((Q S) (A C))
 ((Q S) (A H)) ((A S) (Q D)) ((A S) (Q C)) ((A S) (Q H)) ((A D) (Q D)) ((A C) (Q C)) ((A H) (Q H)) ((A S) (Q S))
 ((K C) (A D)) ((A C) (K D)) ((K H) (A D)) ((K H) (A C)) ((A H) (K D)) ((A H) (K C)) ((K S) (A D)) ((K S) (A C))
 ((K S) (A H)) ((A S) (K D)) ((A S) (K C)) ((A S) (K H)) ((A D) (K D)) ((A C) (K C)) ((A H) (K H)) ((A S) (K S))
 ((2 C) (2 D)) ((2 H) (2 D)) ((2 H) (2 C)) ((2 S) (2 D)) ((2 S) (2 C)) ((2 S) (2 H)) ((3 C) (3 D)) ((3 H) (3 D))
 ((3 H) (3 C)) ((3 S) (3 D)) ((3 S) (3 C)) ((3 S) (3 H)) ((4 C) (4 D)) ((4 H) (4 D)) ((4 H) (4 C)) ((4 S) (4 D))
 ((4 S) (4 C)) ((4 S) (4 H)) ((5 C) (5 D)) ((5 H) (5 D)) ((5 H) (5 C)) ((5 S) (5 D)) ((5 S) (5 C)) ((5 S) (5 H))
 ((6 C) (6 D)) ((6 H) (6 D)) ((6 H) (6 C)) ((6 S) (6 D)) ((6 S) (6 C)) ((6 S) (6 H)) ((7 C) (7 D)) ((7 H) (7 D))
 ((7 H) (7 C)) ((7 S) (7 D)) ((7 S) (7 C)) ((7 S) (7 H)) ((8 C) (8 D)) ((8 H) (8 D)) ((8 H) (8 C)) ((8 S) (8 D))
 ((8 S) (8 C)) ((8 S) (8 H)) ((9 C) (9 D)) ((9 H) (9 D)) ((9 H) (9 C)) ((9 S) (9 D)) ((9 S) (9 C)) ((9 S) (9 H))
 ((10 C) (10 D)) ((10 H) (10 D)) ((10 H) (10 C)) ((10 S) (10 D)) ((10 S) (10 C)) ((10 S) (10 H)) ((J C) (J D)) ((J H) (J D))
 ((J H) (J C)) ((J S) (J D)) ((J S) (J C)) ((J S) (J H)) ((Q C) (Q D)) ((Q H) (Q D)) ((Q H) (Q C)) ((Q S) (Q D))
 ((Q S) (Q C)) ((Q S) (Q H)) ((K C) (K D)) ((K H) (K D)) ((K H) (K C)) ((K S) (K D)) ((K S) (K C)) ((K S) (K H))
 ((A C) (A D)) ((A H) (A D)) ((A H) (A C)) ((A S) (A D)) ((A S) (A C)) ((A S) (A H))))
