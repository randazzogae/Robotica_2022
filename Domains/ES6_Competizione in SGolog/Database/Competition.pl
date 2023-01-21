/* declare the primitive actions */
primitive_action(goto(_)).
primitive_action(wander).
primitive_action(go_flag).
primitive_action(sense(_)).
primitive_action(open_gripper).
primitive_action(send_message(_)).
primitive_action(wait).


poss(goto(_),_).
poss(wander,_).
poss(turn_off_sonar,_).
poss(sense(_),_).
poss(open_gripper,_).
poss(go_flag,_).
poss(send_message(_),_).
poss(wait,_).

/* successor state axioms */
holds(something_visible,do(A,S)):- holds(something_visible,S) ; A = assm(something_visible,1).
holds(red_flag,do(A,S))  :- holds(something_visible,S), A = go_flag; A = assm(red_flag,1).
holds(green_flag,do(A,S)):- holds(something_visible,S), A = go_flag; A = assm(green_flag,1).
holds(blue_flag,do(A,S)):- holds(something_visible,S), A = go_flag; A = assm(blue_flag,1).
holds(can_go,do(A,S)):-  (holds(red_flag,S),   A = goto(red_attractor)  ); 
			 (holds(blue_flag,S),  A = goto(blue_attractor) ); 
                         (holds(green_flag,S), A = goto(green_attractor));
			 A=assm(can_go,1).


/* facts about the initial situation s0 */
/* none necessary here */


proc(execute,
		(sense(something_visible): 
		branch_on(something_visible): 
		if(something_visible,
			go_flag,
			wander: go_flag): 
			deliver_red_flag)
).


proc(deliver_red_flag,
		(sense(red_flag):
		branch_on(red_flag):
		if(red_flag,
			go_bin(red_attractor,red_bin),
			deliver_green_flag))
).

proc(deliver_green_flag,
		(sense(green_flag):
		branch_on(green_flag):
		if(green_flag,
			go_bin(green_attractor, green_bin),
			deliver_blue_flag))
).

proc(deliver_blue_flag,
		(sense(blue_flag):
		branch_on(blue_flag):
		if(blue_flag,
			go_bin(blue_attractor, blue_bin),
			wander))
).

proc(go_bin(X,Y),
	goto(X) : send_message(am_at_attractor) : 
	(sense(can_go):
	 branch_on(can_go):
	 if(can_go,
            	goto(Y) : open_gripper : goto(X),
		wait: goto(Y) : open_gripper : goto(X)))
).
	


