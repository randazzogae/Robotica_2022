/*sinistro*/
/* declare the primitive actions */
primitive_action(goto(_)).
primitive_action(goto_lab(_)).
primitive_action(wander).
primitive_action(explore(_)).
primitive_action(go_flag).
primitive_action(exit).
primitive_action(sense(_)).
primitive_action(open_gripper).
primitive_action(reset_timer).
primitive_action(calcola_percorso_consegna).
primitive_action(prima_cella_consegna).
primitive_action(seconda_cella_consegna).
primitive_action(terza_cella_consegna).
primitive_action(send_message(_)).
primitive_action(wait).
primitive_action(freecell(_,_)).
primitive_action(flag(_,_)).

poss(goto(_),_).
poss(goto_lab(_),_).
poss(wander,_).
poss(explore(_),_).
poss(turn_off_sonar,_).
poss(sense(_),_).
poss(open_gripper,_).
poss(reset_timer,_).
poss(calcola_percorso_consegna,_).
poss(prima_cella_consegna,_).
poss(seconda_cella_consegna,_).
poss(terza_cella_consegna,_).
poss(go_flag,_).
poss(exit,_).
poss(send_message(_),_).
poss(wait,_).
poss(freecell(_,_),_).

/* successor state axioms */
holds(something_visible,do(A,S)):- holds(something_visible,S) ; A = assm(something_visible,1).
holds(something_visible_lab,do(A,S)):- holds(something_visible_lab,S) ; A = assm(something_visible_lab,1).
holds(in_low_lab,do(A,S)):- holds(in_low_lab,S) ; A = assm(in_low_lab,1).
holds(in_high_lab,do(A,S)):- holds(in_high_lab,S) ; A = assm(in_high_lab,1).
holds(in_centre_lab,do(A,S)):- holds(in_centre_lab,S) ; A = assm(in_centre_lab,1).
holds(in_lab,do(A,S)):- holds(in_lab,S) ; A = assm(in_lab,1).
holds(use_lab,do(A,S)):- holds(use_lab,S) ; A = assm(use_lab,1).
holds(use_lab_red_blue,do(A,S)):- holds(use_lab_red_blue,S) ; A = assm(use_lab_red_blue,1).
holds(come_from_low,do(A,S)):- holds(come_from_low,S) ; A = assm(come_from_low,1).
holds(have_flag,do(A,S)):- holds(have_flag,S) ; A = assm(have_flag,1).
holds(room,do(A,S)):- holds(room,S) ; A = assm(room,1).
holds(centre,do(A,S)):- holds(centre,S) ; A = assm(centre,1).
holds(red_flag,do(A,S))  :- holds(something_visible,S), A = go_flag; A = assm(red_flag,1).
holds(green_flag,do(A,S)):- holds(something_visible,S), A = go_flag; A = assm(green_flag,1).
holds(blue_flag,do(A,S)):- holds(something_visible,S), A = go_flag; A = assm(blue_flag,1).
holds(can_go,do(A,S)):-  (holds(red_flag,S),   A = goto(red_attractor)  ); 
			 (holds(blue_flag,S),  A = goto(blue_attractor) ); 
                         (holds(green_flag,S), A = goto(green_attractor));
			 A=assm(can_go,1).


/* facts about the initial situation s0 */
/* none necessary here */

proc(visita_cella_libera,
		goto(prima_cella):
		goto(seconda_cella)
).

proc(execute,
		(sense(something_visible): 
		branch_on(something_visible): 
		if(something_visible,
			go_flag,
			wander: go_flag): 
			deliver_red_flag)
).

proc(bring_flag,
			(sense(room):
			branch_on(room):
	 		if(room,
				exit:deliver_red_flag,
				deliver_red_flag)
			)
).

proc(explore_lab,
		
		explore(entry):
		(sense(something_visible_lab):
		branch_on(something_visible_lab):
			if(something_visible_lab,
				go_flag:exit_lab,
				goto_lab(centre):explore_centre												
				)		
		)				
).	
		


proc(explore_centre,
		explore(attractor1):
		(sense(something_visible_lab):
		branch_on(something_visible_lab):
			if(something_visible_lab,
				go_flag:exit_lab,
				explore(attractor2):
				(sense(something_visible_lab):
				branch_on(something_visible_lab):
					if(something_visible_lab,
						go_flag:exit_lab,
							explore(door2):
							(sense(something_visible_lab):
							branch_on(something_visible_lab):
								if(something_visible_lab,
									go_flag:exit_lab,
									go_other_room
								)
							)
					)
				)
			)		
		)		
).		
proc(exit_lab,
		(sense(in_high_lab):
		branch_on(in_high_lab):
			if(in_high_lab,
				goto_lab(door1_high),
				(sense(in_centre_lab):
				branch_on(in_centre_lab):
					if(in_centre_lab,
						goto_lab(door2_high):goto_lab(door2):
						goto_lab(door1_high),
						(sense(have_flag):
						branch_on(have_flag):
							if(have_flag,
								goto_lab(door2_low):goto_lab(door2_high):
								goto_lab(door2):goto_lab(door1_high),
								goto_lab(door1_low)
								)
						)					
					)		
				)
			)
	)
).

proc(go_other_room,	
	goto_lab(door2):
	explore(exit):
	(sense(something_visible_lab):
	branch_on(something_visible_lab):
		if(something_visible_lab,
			go_flag:exit_lab,
			exit_lab
			)
		)
	).



			
			


proc(deliver_red_flag,
		(sense(red_flag):
		branch_on(red_flag):
		if(red_flag,
		   (sense(use_lab_red_blue):
			branch_on(use_lab_red_blue):
			if(use_lab_red_blue,
				goto(entry_lab),goto(here):calcola_percorso_consegna:prima_cella_consegna:seconda_cella_consegna:terza_cella_consegna   
			)
			):
			go_bin(red_attractor1,red_attractor2,red_bin),
			deliver_green_flag))
).

proc(deliver_green_flag,		
		(sense(green_flag):
		branch_on(green_flag):
		if(green_flag,
			(sense(use_lab):
			branch_on(use_lab):
			if(use_lab,
				goto(entry_lab),goto(here):calcola_percorso_consegna:prima_cella_consegna:seconda_cella_consegna:terza_cella_consegna      
			)
			):
			go_bin(green_attractor1,green_attractor2, green_bin),
			deliver_blue_flag))
).

proc(deliver_blue_flag,
		(sense(blue_flag):
		branch_on(blue_flag):
		if(blue_flag,
		   (sense(use_lab_red_blue):
			branch_on(use_lab_red_blue):
			if(use_lab_red_blue,
				goto(entry_lab),goto(here):calcola_percorso_consegna:prima_cella_consegna:seconda_cella_consegna:terza_cella_consegna      
			)
			):
			go_bin(blue_attractor1,blue_attractor2, blue_bin),
			wander))
).

proc(go_bin(Z,X,Y),
        goto(Z) :
        (sense(in_lab):
         branch_on(in_lab):
         	if(in_lab,
            	exit_lab:goto(Z),
            	goto(Z))
        ):    
	       goto(X) : send_message(am_at_attractor) : 
	       (sense(can_go):
	 branch_on(can_go):
	 if(can_go,
            	goto(Y) : open_gripper : goto(X) : goto(Z):reset_timer,
		wait: goto(Y) : open_gripper : goto(X) : goto(Z):reset_timer))
).