call_connective(Name, [X], L):-
	setof([X], call(lat:Name, X), L).
call_connective(Name, [X,Y], L):-
	setof([X,Y], call(lat:Name, X,Y), L).
call_connective(Name, [X,Y,Z], L):-
	setof([X,Y,Z], call(lat:Name, X,Y,Z), L).
call_connective(Name, [W,X,Y,Z], L):-
	setof([W,X,Y,Z], call(lat:Name, W,X,Y,Z), L).
call_connective(Name, [V,W,X,Y,Z], L):-
	setof([V,W,X,Y,Z], call(lat:Name, V,W,X,Y,Z), L).
call_connective(Name, [U,V,W,X,Y,Z], L):-
	setof([U,V,W,X,Y,Z], call(lat:Name, U,V,W,X,Y,Z), L).

show_result(L1, L2) :-
	L1 == L2,
	not_vars(L1)
	->	write(true)
	;   show_each_result(1, L1, L2).
show_each_result(_, [], []).
show_each_result(I, [_|Vs], [X1|Xs]) :-
	(   var(X1)
	->  get_var_name(I, VarName),
		upcase_atom(VarName, VarUpper),
		writeln(VarUpper)
	;   writeln(X1)
	),
	J is I + 1,
	show_each_result(J, Vs, Xs).

not_vars([]).
not_vars([X1|Xs]):-
	\+ var(X1),
	not_vars(Xs).
