:- pce_begin_class(lattice_node(name), device).

initialise(Node, Name, Color:name) :->
	"Create node name"::
	% Name as string
	with_output_to(codes(Codes), write(Name)),
	string_codes(String, Codes),
	send(Node, send_super, initialise),
	send(Node, display, new(C, circle(30)), point(-2, -2)),
	send(C, fill_pattern, colour(Color)),
	send(Node, display, new(T, text(String, center))),
	send(T, center, C?center),
	send(T, font, font(courier, bold, 12)),
	send(Node, send_super, name, Name),
	send(Node, recogniser, @lattice_node_recogniser).
	
:- pce_global(@lattice_node_recogniser, make_lattice_node_recogniser).

make_lattice_node_recogniser(G) :-
	new(ThisNode, @event?receiver),
	new(C, click_gesture(left, '', single, message(ThisNode, setFromTo))),
	new(DD, drag_and_drop_gesture(left, modifier(@default, @default, up), get_source := @arg1?name)),
	new(M, move_gesture(left, '')),
	new(P, popup_gesture(new(Pop, popup))),
	send_list(Pop, append,
			  [ menu_item(delete_node, message(@arg1, free_node))
			  , menu_item(rename, message(@arg1, rename))
	  ]),
	new(G, handler_group(C, M, DD, P)).

event(Node, Ev:event) :->
	"Make it work"::
	(   send(@lattice_node_recogniser, event, Ev)
	->  true
	;   send(Node, send_super, event, Ev)
	).

rename(Node) :->
	"Get new name for a node"::
	get(Node, frame, F),
	
	new(ND, dialog('Enter Node Name')), 
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(TI, text_item(name))), 
	send(ND, append, button(ok, and(message(Node, check_return, F, TI?selection),
									message(ND, return, TI?selection)))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),
	
	get(ND, confirm_centered,
			    F?area?center, Return),
	send(ND, destroy),
	Return \== @nil,
	get(Return, strip, Name),
	
	get(Node, name, OldName),
	get(Node, member, text, Text),
	send(Text, string, Name),
	send(Node, send_super, name, Name),

	assert_node(Return),
	(get_node_member(OldName, NProper)
	->	replace_in_connections(NProper, Return),
		retract_node(NProper),
		replace_in_lattice(F, OldName, Return)
	;	send(F, lattice)
	),
	send(F, set_modified_state),
	send(F, report, status, 'Truth degree name replaced in lattice code').


check_return(_, F, Node) :->
	get_node_member(Node, _)
	->	send(F, report, error, 'Name already in used'),
		fail
	; 	Node = ''
		->	send(F, report, error, 'Name can\'t be empty'),
			fail
		;	true.
	
free_node(Node) :->
	get(Node, frame, F),
	get(Node, name, Name),
	send(Node, free),
	get_node_member(Name, NName),
	retract_node(NName),
	send(F, set_modified_state),
	
	send(F, lattice),
	replace_in_lattice(F, NName, '????'),
	send(F, report, status, 'Truth degree name replaced with -????- in lattice code').
								
assert_node(Name) :-
	assertz(lat_graph:member(Name)),
	lat_graph:members(L)
	->	append(L, [Name], L1),
		retract(lat_graph:members(L)),
		asserta(lat_graph:members(L1))
	;	assert(lat_graph:members([Name])).
	
retract_node(NName) :-
	(	lat_graph:members(L)
	->	select(NName, L, L1),
		retract(lat_graph:members(L)),
		asserta(lat_graph:members(L1))
	;	true
	),
	(	lat_graph:member(NName)
	->	retract(lat_graph:member(NName))
	;	true
	),
	delete_connections(NName).
	
replace_in_connections(Old, New):-
	forall(lat_graph:arc(N1, Old), assertz(lat_graph:arc(N1, New))),
	forall(lat_graph:arc(Old, N2), assertz(lat_graph:arc(New, N2))),
	forall(lat_graph:top(Old), assertz(lat_graph:top(New))),
	forall(lat_graph:bot(Old), assertz(lat_graph:bot(New))).

delete_connections(Node):-
	forall(lat_graph:top(Node), retract(lat_graph:top(Node))),
	forall(lat_graph:bot(Node), retract(lat_graph:bot(Node))),
	forall(lat_graph:arc(_, Node), retract(lat_graph:arc(_, Node))),
	forall(lat_graph:arc(Node, _), retract(lat_graph:arc(Node, _))).

setFromTo(Node) :->
	"Select Nodes From and To"::
	(	lat_graph:fromNode(FNode),
		FNode \== @nil, FNode \== Node
	)
	->	retractall(lat_graph:fromNode(_)),
		(	lat_graph:bot(B)
		->	get(Node, name, Name),
			Name \= B
		;	true
		),
		make_connect(FNode, Node, user)
	; 	(	lat_graph:top(T)
		->	get(Node, name, Name),
			Name \= T
		;	true
		),
		assertz(lat_graph:fromNode(Node)).

:- pce_end_class.
