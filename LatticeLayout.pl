%%%%%%%
%%%%% NODES
%%%%% layering((bot, 1), L).
%%%%%%%
layering(L1, L2, N1, N2) :-
	layering1(L1, L2, N1, 0, N2).

layering1([], [], _, _, 0).
layering1([(X, Layer)|L1], L2, NextLayer, MaxNodes, N3):-
	NextLayer is Layer + 1,
	(	setof((Y, NextLayer), (lat_graph:arc(X,Y), \+ member((Y, NextLayer), L1)), L3)
		-> 	length(L3, NNodes),
		(	NNodes > MaxNodes, N1 is NNodes; N1 is MaxNodes	),
			append(L1, L3, L5),
			layering1(L5, L4, _, N1, N2)
		;	N1 is MaxNodes,
			layering1(L1, L4, _, N1, N2)
	),
	my_append((X, Layer), L4, L2),
	(	N2 > N1, N3 is N2; N3 is N1	).


my_append((X, Layer), [], NewL):-
	append([(X, Layer)], [], NewL).
my_append((X, Layer), L, NewL):-
	member((X, WhatLayer), L)
	->	(	Layer > WhatLayer
		->	select((X, WhatLayer), L, L1),
			append([(X, Layer)], L1, NewL)
		;	NewL = L
		)
	;	append([(X, Layer)], L, NewL).
	
draw_list(_, []).
draw_list(F, L):-
	get_layer(L, L1),
	length(L1, NNodes),
	I = 1,
	layout_layer(F, L1, NNodes, I),
	append(L1, L2, L),
	draw_list(F, L2).
	
get_layer([], []).
get_layer([(X, Layer)|L1], L2):-
	get_this_layer(L1, Layer, L3),
	append([(X, Layer)], L3, L2).
	
get_this_layer([], _, []).
get_this_layer([(Y, Layer)|L1], Layer, L2):-
	get_this_layer(L1, Layer, L3),
	append([(Y, Layer)], L3, L2).
get_this_layer([(_, _)|L1], Layer, L2):-
	get_this_layer(L1, Layer, L2).
	
layout_layer(_, [], _, _).
layout_layer(F, [(X, Layer)|L], N, I) :-
	I > N
	-> 	true
	;	colour(X, C),
		get(F, node, X, Layer, C, N, I, _),
		I1 is I + 1,
		layout_layer(F, L, N, I1).

get_node_layer([], _, 0).
get_node_layer([(Node, Ly)|_], Node, Ly).
get_node_layer([_|L1], Node, Ly):-
	get_node_layer(L1, Node, Ly).

get_no_connected(L):-
	lat_graph:members(ListNodes), 
	setof(Node, (member(Node, ListNodes), detached_bottop(Node)), L); L = [].
get_no_connected_top(L):-
	lat_graph:members(ListNodes), 
	setof(Node, (member(Node, ListNodes), detached_top(Node)), L); L = [].
get_no_connected_bot(L):-
	lat_graph:members(ListNodes), 
	setof(Node, (member(Node, ListNodes), detached_bot(Node)), L); L = [].

add_semi_connected(L, Layer, L1):-
	lat_graph:members(ListNodes), 
	setof((Node, Layer), (member(Node, ListNodes), attached_top(Node), detached_bot(Node)), L2)
	-> 	append(L, L2, L1)
	;	L1 = L.

layout_no_connected(_, []).
layout_no_connected(F, List) :-
	length(List, N),
	draw_no_connected(F, List, N, 1).

draw_no_connected(_, [], _, _).
draw_no_connected(F, [H|T], N, I):-
	colour(H, C),
	get(F, node, H, I, C, N, 0, _),
	J is I + 1,
	draw_no_connected(F, T, N, J).

colour(X, yellow):- lat_graph:bot(X).
colour(X, blue):- lat_graph:top(X).
colour(X, red):- detached_top(X), detached_bot(X).
colour(X, orange):- detached_top(X).
colour(X, purple):- detached_bot(X).
colour(X, brown) :- not(supr_inf_one(X,lat_graph)).
colour(_, green).

detached_bottop(X):- detached_bot(X), detached_top(X).
detached_bot(X):- lat_graph:bot(B), not(lat_graph:leq(B, X)).
detached_top(X):- lat_graph:top(T), not(lat_graph:leq(X, T)).
attached_top(X):- lat_graph:top(T), lat_graph:leq(X, T).


%%%%%%%
%%%%% ARCS
%%%%%%%

prepare_for_drawing :-
	no_cycles,
	add_arc,
	retract_leq,
	lat_graph:assert(lat_graph:leq(X, X)),
	lat_graph:assert(':-'(lat_graph:leq(X, Y), (lat_graph:arc(X, Z), lat_graph:leq(Z, Y)))),
	del_arcs.

no_cycles :-
	forall(lat_graph:arc(X, Y), (	\+ path(Y, X); retract(lat_graph:arc(X, Y))	)).
	
retract_leq :- retractall(lat_graph:leq(_,_)).

add_arc :-
	lat_graph:members(L),
	ext2(L, X, Y, _),
	\+ path(X, Y),
	lat_graph:leq(X, Y),
	asserta(lat_graph:arc(X, Y)),
	fail.
add_arc.

path(X, X).
path(X, Y) :-
	lat_graph:arc(X, Z), path(Z, Y).

del_arcs :-
	retract(lat_graph:arc(X, Y)), delarcaux(X, Y), fail.
del_arcs.

delarcaux(X, Y) :- lat_graph:leq(X, Y), !.
delarcaux(X, Y) :- asserta(lat_graph:arc(X, Y)).

darcs(F) :-
	lat_graph:members(L),
	ext2(L, X, Y, _),
	lat_graph:arc(X, Y),
	get(F, node, X, XN),
	get(F, node, Y, YN),
	make_connect(XN, YN, program),
	fail.
darcs(_).

ext2(L, X, Y, R) :-
	ext1(L, X, S),
	ext1(S, Y, R).

ext1(L, X, R) :-
	append(A, [X|B], L),
	append(A, B, R).
