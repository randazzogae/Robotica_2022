/* declare the primitive actions */
primitive_action(goto(_)).
primitive_action(buy_paper).
primitive_action(buy_coffee).
primitive_action(board_plane).
primitive_action(sense(_)).
primitive_action(smoke).

/*aggiungiamo azioni primitiva*/
primitive_action(buy_flower).
primitive_action(drop_flower).
primitive_action(get_info).
primitive_action(take_bottle).
primitive_action(recycle).

/* all actions are always possible except board_plane, */
/* which requires being at the right gate */
poss(goto(_),_).
poss(buy_paper,_).
poss(buy_coffee,_).
poss(sense(_),_).
poss(smoke,_).
poss(get_info,_).
poss(buy_flower,_).
poss(drop_flower,_).
poss(take_bottle,_).
poss(recycle,_).

/* boarding requires being at the right gate */
poss(board_plane,S) :- holds(it_is_gate_A,S) -> 
		       holds(am_at(gate_A),S) ;holds(it_is_gate_C,S) -> holds(am_at(imbarco_C),S) ;
                       holds(am_at(gate_B),S).

/* successor state axioms */

/* I am at X if I just went there or */
/* if I was there and did not go anywhere else */
/* (assumes that boarding the plane leaves you at the gate) */
holds(am_at(X),do(A,S)) :- A = goto(X) ; 
			  (holds(am_at(X),S), not (A = goto(_))).

holds(it_is_gate_A,do(A,S)) :- holds(it_is_gate_A,S) ; 
			       A = assm(it_is_gate_A,1).

/* aggiungiamo gate C */
holds(it_is_gate_C,do(A,S)) :- holds(it_is_gate_C,S) ; 
			       A = assm(it_is_gate_C,1).

holds(it_is_late,do(A,S)) :- holds(it_is_late,S) ; 
			       A = assm(it_is_late,1).

holds(bottle,do(A,S)) :- holds(bottle,S) ; 
			       A = assm(bottle,1).

/* facts about the initial situation s0 */
/* none necessary here */

proc(catch_plane,   
	   buy_paper:	
	   sense(it_is_gate_A):   
	   branch_on(it_is_gate_A):
	   if(it_is_gate_A,
		 goto(gate_A):
		 have_a_break,
               sense(it_is_gate_C):   
	       branch_on(it_is_gate_C):
               if(it_is_gate_C,
		    goto(gate_C):
		    preparativi,
		    goto(gate_B):
           have_a_break)):
	   board_plane
    ).

proc(have_a_break,
	sense(it_is_late):	
	branch_on(it_is_late):
	if(it_is_late,
		buy_coffee,
		buy_coffee:smoke)
    ).

proc(info,
          goto(piazza):
          get_info
).

proc(fiori,
	  goto(piazza):
	  goto(fioraio):
	  buy_flower
).

proc(girl,
	  goto(piazza):
	  goto(ragazza):
	  drop_flower:
	  goto(piazza)
).

                 
proc(preparativi,
		info:
		fiori:
		girl:
		goto(imbarco_C):
                sense(bottle):
                branch_on(bottle):
                if(bottle,
                         take_bottle:
                         goto(cestino):
                         recycle:
                         goto(imbarco_C),
                         goto(imbarco_C))
).
