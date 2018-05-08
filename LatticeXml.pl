:- [library(sgml)].

document_XML(F, element('LATTICE', [], Lattice)) :- 
	lat_graph:members(L),
	setof(element('MEMBER', [], [Node]), member(Node, L), L0),
	L1 = [element('MEMBERS', [], L0)],

	lat_graph:bot(B),
	L2 = [element('BOT',[],[B])],
	lat_graph:top(T),
	L3 = [element('TOP',[],[T])],

	(	 lat_graph:arc(_, _)
	-> 	setof(element('ARC',[],[element('FROM',[],[X]), 
								element('TO',[],[Y])]), 
								lat_graph:arc(X, Y), L4)
	;	L4 = []
	),
	
	L5 = [element('AGGREGATOR',[],[element('NAME',[],['leq']), 
								element('ARITY',[],[2]), 
								element('CLAUSE',[],
									[element('HEAD',[],['leq(A, A)']),
									 element('BODY',[],['true'])])])],
	L6 = [element('AGGREGATOR',[],[element('NAME',[],['leq']), 
								element('ARITY',[],[2]), 
								element('CLAUSE',[],
									[element('HEAD',[],['leq(A, B)']),
									 element('BODY',[],['arc(A, C),leq(C, B)'])])])],

	retractall(lattice_predicate(_, _, _, _)),
	get(F, member, view, V),
	pce_open(V, read, File),
		repeat,
	(   read_term(File, Term, [variable_names(LVars)]),
		Term \== end_of_file
	->  with_output_to(atom(Atom), 
				write_term(Term, [variable_names(LVars), quoted(true)])),
		analize_predicate(Atom, (Name, Arity, Head, Body)),
		(	filtered(Name, Arity)
		->  fail
		;   assertz(lattice_predicate(Name, Arity, Head, Body))
		),
		fail
	;	!
	),
	close(File),

	findall(element('AGGREGATOR',[],[element('NAME',[],[PP]), 
								element('ARITY',[],[AA]), 
								element('CLAUSE',[],
									[element('HEAD',[],[HH]),
									 element('BODY',[],[BB])])]),
			lattice_predicate(PP, AA, HH, BB), L7),

	append([L1, L2, L3, L4, L5, L6, L7], Lattice). 

generate_XML(F, Out) :-
	document_XML(F, XML),
	pce_open(Out, write, Stream),
    xml_write(Stream, XML, [net(false)]),
    close(Stream).