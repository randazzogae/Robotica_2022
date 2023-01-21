/* declare the primitive actions */
primitive_action(goto(_)).
primitive_action(buy_paper).
primitive_action(buy_coffee).
primitive_action(board_plane).
primitive_action(sense(_)).

/* all actions are always possible except board_plane */
/* which requires being at the right gate */
poss(goto(_),_).
poss(buy_paper,_).
poss(buy_coffee,_).
poss(sense(_),_).
/* boarding requires being at the right gate */
poss(board_plane,S) :-  holds(it_is_gate_A,S) ->
		           holds(am_at(gate_A),S) ; holds(am_at(gate_B),S).
/* successor state axioms */
/* I am at X if I just went there or */
/* if I was there and did not go anywhere else */
/* (assumes that boarding the plane leaves you at the gate) */
holds(am_at(X),do(A,S)) :- A = goto(X); (holds(am_at(X),S), not (A = goto(_))).
holds(it_is_gate_A,do(A,S)):- holds(it_is_gate_A,S) ; A = assm(it_is_gate_A,1).

/* facts about the initial situation s() */
/* none necessary here */

proc(catch_plane,
		(sense(it_is_gate_A): 
		buy_paper: 
		branch_on(it_is_gate_A): 
		if(it_is_gate_A,
			goto(gate_A): buy_coffee,
			buy_coffee: goto(gate_B)): 
			board_plane)
).
