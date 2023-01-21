:-dynamic(holds/2).
:-op(970,xfy, [:]). /*Sequence.*/
:-op(950, xfy, [#]). /*Nondeterministic action choice.*/

/*do4(P,S,C,C1) recursively descends the CAT C (first two clauses).*/
/*Once a leaf of the CAT is reached (third clause), "do" is called,*/
/*which then extends this branch according to P*/
do4(E,S,[A|C],C1):-primitive_action(A),C1=[A|C2],do4(E,do(A,S),C,C2).
do4(E,S,[[P,C1,C2]],C):-do4(E,do(assm(P,1),S),C1,C3),
				do4(E,do(assm(P,0),S),C2,C4),
				C = [[P,C3,C4]].
do4(E,S,[],C):-do(E,S,C).

do(E1 : E2, S, C) :-do(E1,S,C1), do4(E2,S,C1,C). /* sequence */
do(?(P),S,C) :-C=[],holds(P,S). /* test */

do(E1 # E2, S, C) :- do(E1,S,C); do(E2,S,C). /* nond. act. choice */
do(if(P,E1,E2),S,C) :- do((?(P) : E1) # (?(neg(P)) : E2),S,C).

do(star(_),_,[]). /* nondet. iteration .*/
do(star(E),S,C) :- do(E : star(E),S,C).
do(while(P,E),S,S1) :- do(star(?(P) : E) : ?(neg(P)),S,S1).

do(pi(V,E),S,S1) :- sub(V,_,E,E1), do(E1,S,S1). /*nond. arg. choice */

do(E,S,C) :- proc(E,E1), do(E1,S,C). /* procedure */

/* the base cases: primitive actions and branch_on(P) */
do(E,S,[E]) :- primitive_action(E), poss(E,S).
do(branch_on(P),S,[[P,[],[]]]) :- holds(sensed(P),S).

/* sub and sub_list are auxiliary predicates */
/* sub(Name,New,Term1,Term2): */
/* Term1 is Term2 with Name replaced by New. */
sub(_,_,T1,T2) :- var(T1), T2 = T1.
sub(X1,X2,T1,T2) :- not var(T1), T1 = X1, T2 = X2.
sub(X1,X2,T1,T2) :- not T1 = X1, T1 =..[F|L1], sub_list(X1,X2,L1,L2),T2 =..[F|L2].
sub_list(_,_,[],[]).
sub_list(X1,X2,[T1|L1],[T2|L2]) :- sub(X1,X2,T1,T2), sub_list(X1,X2,L1,L2).

/* Definition of holds for arbitrary nonatomic formulas */
holds(and(P1,P2),S) :- holds(P1,S), holds(P2,S).
holds(or(P1,P2),S) :- holds(P1,S); holds(P2,S).
holds(neg(P),S) :- not holds(P,S). /* Negation by failure */
holds(some(V,P),S) :- sub(V,_,P,P1), holds(P1,S). 
/* the successor state axiom for sensed */
holds(sensed(P),do(A,S)) :- A = sense(P) ; holds(sensed(P),S).
