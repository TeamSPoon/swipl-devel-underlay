
:-module(leq,[leq/0]).
:-use_module(library(chr/chr_runtime)).
:-use_module(library(chr)).

verify_attributes(A, D, B) :-
	(   get_attr(A, leq, C)
	->  B=[(sort(C, F),  (var(D)-> (get_attr(D, leq, E)->sort(E, G), 'chr merge_attributes'(F, G, H), put_attr(D, leq, H), '$run_suspensions_leq___2'(F);put_attr(D, leq, F), '$run_suspensions_leq___2'(F)); (compound(D)->term_variables(D, I), attach_increment(I, F);true), '$run_suspensions_leq___2'(F)))]
	;   B=[]
	).

leq___2__3(_, _, A) :-
	setarg(2, A, active).

leq___2__1__0__6([], A, B, C) :-
	leq___2__2(A, B, C).
leq___2__1__0__6([A|H], F, C, D) :-
	(   A=suspension(_, active, _, _, _, _, B, G),
	    B==C,
	    E=t(4, D, A),
	    '$novel_production'(D, E),
	    '$novel_production'(A, E),
	    'chr debug_event'(try([],
				  [D, A],
				  true,
				  leq(F, G)))
	->  'chr debug_event'(apply([],
				    [D, A],
				    true,
				    leq(F, G))),
	    '$extend_history'(D, E),
	    setarg(2, D, active),
	    leq(F, G),
	    (   D=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, D, inactive),
		leq___2__1__0__6(H, F, C, D)
	    ;   true
	    )
	;   leq___2__1__0__6(H, F, C, D)
	).

leq(A, B) :-
	C=suspension(E, active, t, 0, leq:leq___2__0(A, B, C), leq, A, B),
	term_variables(term(A, B), D),
	'chr none_locked'(D),
	'chr gen_id'(E),
	nb_getval('$chr_store_global_list_leq____leq___2', F),
	b_setval('$chr_store_global_list_leq____leq___2', [C|F]),
	attach_leq___2(D, C),
	setarg(2, C, inactive),
	'chr debug_event'(insert(#(leq(A, B), C))),
	(   'chr debugging'
	->  (   'chr debug_event'(call(C)),
		leq___2__0(A, B, C)
	    ;   'chr debug_event'(fail(C)), !,
		fail
	    ),
	    (   'chr debug_event'(exit(C))
	    ;   'chr debug_event'(redo(C)),
		fail
	    )
	;   leq___2__0(A, B, C)
	).

leq___2__0(B, A, C) :-
	A==B,
	'chr debug_event'(try([C], [], true, true)), !,
	'chr debug_event'(apply([C], [], true, true)),
	'chr debug_event'(remove(C)),
	C=suspension(_, _, _, _, _, leq, D, E),
	setarg(2, C, removed),
	term_variables(term(D, E), H),
	nb_getval('$chr_store_global_list_leq____leq___2', F),
	'chr sbag_del_element'(F, C, G),
	b_setval('$chr_store_global_list_leq____leq___2', G),
	detach_leq___2(H, C).
leq___2__0(B, A, H) :-
	(   'chr newvia_2'(A, B, C)
	->  get_attr(C, leq, D)
	;   nb_getval('$chr_store_global_list_leq____leq___2', D)
	),
	member(E, D),
	E=suspension(_, active, _, _, _, _, F, G),
	F==A,
	G==B,
	(   'chr debug_event'(try([H, E], [], true, B=A)), !,
	    'chr debug_event'(apply([H, E], [], true, B=A)),
	    'chr debug_event'(remove(E)),
	    E=suspension(_, _, _, _, _, leq, I, J),
	    setarg(2, E, removed),
	    term_variables(term(I, J), M),
	    nb_getval('$chr_store_global_list_leq____leq___2', K),
	    'chr sbag_del_element'(K, E, L),
	    b_setval('$chr_store_global_list_leq____leq___2', L),
	    detach_leq___2(M, E),
	    'chr debug_event'(remove(H)),
	    H=suspension(_, _, _, _, _, leq, N, O),
	    setarg(2, H, removed),
	    term_variables(term(N, O), R),
	    nb_getval('$chr_store_global_list_leq____leq___2', P),
	    'chr sbag_del_element'(P, H, Q),
	    b_setval('$chr_store_global_list_leq____leq___2', Q),
	    detach_leq___2(R, H),
	    B=A
	;   'chr debug_event'(try([E, H], [], true, A=B)), !,
	    'chr debug_event'(apply([E, H], [], true, A=B)),
	    'chr debug_event'(remove(E)),
	    E=suspension(_, _, _, _, _, leq, S, T),
	    setarg(2, E, removed),
	    term_variables(term(S, T), W),
	    nb_getval('$chr_store_global_list_leq____leq___2', U),
	    'chr sbag_del_element'(U, E, V),
	    b_setval('$chr_store_global_list_leq____leq___2', V),
	    detach_leq___2(W, E),
	    'chr debug_event'(remove(H)),
	    H=suspension(_, _, _, _, _, leq, X, Y),
	    setarg(2, H, removed),
	    term_variables(term(X, Y), B1),
	    nb_getval('$chr_store_global_list_leq____leq___2', Z),
	    'chr sbag_del_element'(Z, H, A1),
	    b_setval('$chr_store_global_list_leq____leq___2', A1),
	    detach_leq___2(B1, H),
	    A=B
	).
leq___2__0(A, B, H) :-
	(   'chr newvia_2'(A, B, C)
	->  get_attr(C, leq, D)
	;   nb_getval('$chr_store_global_list_leq____leq___2', D)
	),
	(   member(E, D),
	    E=suspension(_, active, _, _, _, _, F, G),
	    F==A,
	    G==B,
	    'chr debug_event'(try([H], [E], true, true)), !,
	    'chr debug_event'(apply([H], [E], true, true)),
	    'chr debug_event'(remove(H)),
	    H=suspension(_, _, _, _, _, leq, I, J),
	    setarg(2, H, removed),
	    term_variables(term(I, J), M),
	    nb_getval('$chr_store_global_list_leq____leq___2', K),
	    'chr sbag_del_element'(K, H, L),
	    b_setval('$chr_store_global_list_leq____leq___2', L),
	    detach_leq___2(M, H)
	;    !,
	    leq___2__0__0__5(D, A, B, H)
	).
leq___2__0(A, B, C) :-
	leq___2__1(A, B, C).

leq___2__0__0__5([], A, B, C) :-
	leq___2__1(A, B, C).
leq___2__0__0__5([A|L], C, E, F) :-
	(   A=suspension(_, active, _, _, _, _, B, D),
	    B==C,
	    D==E,
	    'chr debug_event'(try([A], [F], true, true))
	->  'chr debug_event'(apply([A], [F], true, true)),
	    'chr debug_event'(remove(A)),
	    A=suspension(_, _, _, _, _, leq, G, H),
	    setarg(2, A, removed),
	    term_variables(term(G, H), K),
	    nb_getval('$chr_store_global_list_leq____leq___2', I),
	    'chr sbag_del_element'(I, A, J),
	    b_setval('$chr_store_global_list_leq____leq___2', J),
	    detach_leq___2(K, A),
	    setarg(2, F, active),
	    (   F=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, F, inactive),
		leq___2__0__0__5(L, C, E, F)
	    ;   true
	    )
	;   leq___2__0__0__5(L, C, E, F)
	).

circle(A, B, C) :-
	leq(A, B),
	leq(B, C),
	leq(C, A).

leq___2__2(A, D, E) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, leq, C)
	;   nb_getval('$chr_store_global_list_leq____leq___2', C)
	), !,
	leq___2__2__0__7(C, A, D, E).
leq___2__2(A, B, C) :-
	leq___2__3(A, B, C).

detach_leq___2([], _).
detach_leq___2([A|E], C) :-
	(   get_attr(A, leq, B)
	->  'chr sbag_del_element'(B, C, D),
	    (   D==[]
	    ->  del_attr(A, leq)
	    ;   put_attr(A, leq, D)
	    )
	;   true
	),
	detach_leq___2(E, C).

attach_increment([], _).
attach_increment([A|F], C) :-
	'chr not_locked'(A),
	(   get_attr(A, leq, B)
	->  sort(B, D),
	    'chr merge_attributes'(C, D, E),
	    put_attr(A, leq, E)
	;   put_attr(A, leq, C)
	),
	attach_increment(F, C).

attach_leq___2([], _).
attach_leq___2([A|D], B) :-
	(   get_attr(A, leq, C)
	->  put_attr(A, leq, [B|C])
	;   put_attr(A, leq, [B])
	),
	attach_leq___2(D, B).

leq :-
	circle(A, B, C),
	\+ attvar(A),
	A==B,
	B==C.

leq___2__2__0__7([], A, B, C) :-
	leq___2__3(A, B, C).
leq___2__2__0__7([A|H], C, G, D) :-
	(   A=suspension(_, active, _, _, _, _, F, B),
	    B==C,
	    E=t(4, A, D),
	    '$novel_production'(A, E),
	    '$novel_production'(D, E),
	    'chr debug_event'(try([],
				  [A, D],
				  true,
				  leq(F, G)))
	->  'chr debug_event'(apply([],
				    [A, D],
				    true,
				    leq(F, G))),
	    '$extend_history'(D, E),
	    setarg(2, D, active),
	    leq(F, G),
	    (   D=suspension(_, active, _, _, _, _, _, _)
	    ->  setarg(2, D, inactive),
		leq___2__2__0__7(H, C, G, D)
	    ;   true
	    )
	;   leq___2__2__0__7(H, C, G, D)
	).

leq___2__1(D, A, E) :-
	(   'chr newvia_1'(A, B)
	->  get_attr(B, leq, C)
	;   nb_getval('$chr_store_global_list_leq____leq___2', C)
	), !,
	leq___2__1__0__6(C, D, A, E).
leq___2__1(A, B, C) :-
	leq___2__2(A, B, C).

attribute_goals(_, A, A).
