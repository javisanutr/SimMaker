:- pce_begin_class(sim_node(name), device).

initialise(Node, Name, Color:name) :->
	"Create node name"::
	% Name as string
	get(Name, split, '/', Chain),
	get(Chain, tail, ET),
	get(ET, value, ETVal),
	if(ETVal=='0',(
		get(Chain, head, EH),
		get(EH, value, EHVal),
		send(Name, value, EHVal)
	),true),
	get(Name, value, Name2),
	with_output_to(codes(Codes), write(Name2)),
	string_codes(String, Codes),
	send(Node, send_super, initialise),
	send(Node, display, new(C, circle(30)), point(-2, -2)),
	send(C, fill_pattern, colour(Color)),
	send(Node, display, new(T, text(Name, center))),
	send(T, center, C?center),
	send(T, font, font(courier, bold, 12)),
	send(Node, send_super, name, Name),
	send(Node, recogniser, @sim_node_recogniser),
	
	get_nodesim_member(NName, String),
	assert_sim_node(NName).
	
:- pce_global(@sim_node_recogniser, make_sim_node_recogniser).

make_sim_node_recogniser(G) :-
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
	(   send(@sim_node_recogniser, event, Ev)
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
	(	Return == @nil
	->	send(Node, free_node)
	;	get(Return, strip, Return2),
		
		get(Return2, split, '/', Chain),
		get(Chain, tail, ET),
		get(ET, value, ETVal),
		if(ETVal=='0',(
			get(Chain, head, EH),
			get(EH, value, EHVal),
			New = EHVal
		),New = Return2),
		
		get(Node, name, Old),
		get(Node, member, text, Text),
		get_nodesim_member(NOld, Old),
		get_nodesim_member(NNew, New),
		send(Text, string, New),
		% send(Node, send_super, name, New),
		send(Node, name, New),
		
		rename_sim_node(NOld, NNew),
		(sim:lista(L),
		member(NNew, L)
		->	replace_sim_in_connections(NOld, NNew),
			rename_nodesim_editor(F, Old, New),
			% rename_nodesim_matrix(F, NNew)
			send(F, update_matrix)
		;	true
			% send(F, lattice)
		),
		send(F, set_modified_state),
		send(F, report, status, 'Truth degree name replaced in lattice code')
	).


check_return(_, F, Name2) :->
	get(Name2, strip, Name),
	if(Name == '',(
		send(F, report, error, 'Name can\'t be empty'),
		fail
	),true),
	if((string_codes(Name, Codes),member(46, Codes)),(
		send(F, report, error, 'Ilegal characters'),
		fail
	),true),
	get_nodesim_member(NName, Name),
	sim:lista(L),
	if(member(NName, L),(
		send(F, report, error, 'Name already in used'),
		fail
	),true).

get_connection(NodeA, NodeB:sim_node, Connection:sim_connection) :<-
	try(get(NodeA, connections, L), false),
	get(L, size, N),
	0 < N,
	between(1, N, Index),
	get(L, nth1, Index, Connection),
	(	get(Connection, to, NodeB)
	;	get(Connection, from, NodeB)
	).
	
	
	
free_connections(Node) :->
	try((get(Node, connections, L),
		send(L, for_all, message(@arg1, free_connect))),
	true).

free_node(Node) :->
	get(Node, frame, F),
	get(Node, name, Name),
	send(Node, free_connections),
	send(Node, free),
	get_nodesim_member(NName, Name),
	retract_sim_node(NName),
	
	%% Actualizar matrix
	send(F, update_matrix),
	
	send(F, set_modified_state),
	send(F, report, status, 'Truth degree name replaced with -????- in lattice code').
		
assert_sim_node(NName) :-
	sim:lista(L),
	member(NName, L)
	->	true
	;(	append(L, [NName], L1),
		retract(sim:lista(L)),
		asserta(sim:lista(L1))
	).

rename_sim_node(NOld, NNew) :-
	sim:lista(L),
	if((member(NOld, L), not(member(NNew, L))),(
		select(NOld, L, NNew, L2),
		retract(sim:lista(L)),
		asserta(sim:lista(L2))
	),
		true
	).
	
retract_sim_node(NName) :-
	select(NName, L, L1),
	retract(sim:lista(L)),
	asserta(sim:lista(L1)),
	delete_sim_connections(NName).
	
replace_sim_in_connections(NOld, NNew):-
	forall(sim:r(N1, NOld, Val), assertz(sim:r(N1, NNew, Val))),
	forall(sim:r(NOld, N2, Val), assertz(sim:r(NNew, N2, Val))),
	delete_sim_connections(NOld).

delete_sim_connections(Node):-
	forall(sim:r(_, Node, _), retract(sim:r(_, Node, _))),
	forall(sim:r(Node, _, _), retract(sim:r(Node, _, _))),
	lat:top(Top),
	retractall(sim:r(A, A, Top)),
	asserta(sim:r(B, B, Top)).

setFromTo(Node) :->
	"Select Nodes From and To"::
	(	try(sim:fromNode(FNode),fail),
		FNode \== Node
	)
	->	retractall(sim:fromNode(_)),
		make_simconnect(FNode, Node, user, _)
	; 	assertz(sim:fromNode(Node)).

:- pce_end_class.
