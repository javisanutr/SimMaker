:- pce_global(@sim_from_handle,
              new(handle(w/2, h/2, out, out))).
:- pce_global(@sim_to_handle,
              new(handle(w/2, h/2, in, in))).
:- pce_global(@sim_link,
              new(link(out, in,
                       line(arrows := none)))).

:- pce_begin_class(sim_connection(from, to), connection).

initialise(Connect, From:sim_node, To:sim_node) :->
	"Create From-To Connection"::
	send(From, handle, @sim_from_handle),
	send(To, handle, @sim_to_handle),
	send(Connect, send_super, initialise, From, To, @sim_link),
	send(Connect, recogniser, @link_recogniser),
	send(From?device, display, new(Rect, box(25,20))),
	send(Rect, fill_pattern, colour(white)),
	new(_, constraint(Connect, Rect, identity(center))),
	
	get(From, name, FName),
	get_nodesim_member(FItem, FName),
	get(To, name, TName),
	get_nodesim_member(TItem, TName),
	try((sim:r(FItem, TItem, Rel),
	% sim:r(TItem, FItem, Z)), lat_graph:bot(Rel)),
	sim:r(TItem, FItem, Rel)), lat:bot(Rel)),
	
	with_output_to(codes(Codes), write(Rel)),
	string_codes(String, Codes),
	send(From?device, display, new(T, text(String, center))),
	send(T, center, Rect?center),
	send(T, font, font(courier, bold, 12)),
	new(_, constraint(Rect, T, identity(center))).
	
darcsim(F) :-
	sim:lista(L),
	ext2(L, A, B, _),
	sim:r(A, B, Z),
	lat:bot(Bot),
	Z \== Bot,
	get_nodesim_member(A, NameA),
	get_nodesim_member(B, NameB),
	get(F, node, NameA, ANode),
	get(F, node, NameB, BNode),
	make_simconnect(ANode, BNode, program, _),
	fail.
darcsim(_).
	
make_simconnect(From, To, WhoCalls, Connection) :-
	(	WhoCalls == program
		-> 	true
		;	(	not(get(From, get_connection, To, Con))
			->	get(From, frame, F),
				ok_arrow_mode(F),
				
				%% Actualizar editor
				get(From, name, FName),
				% get_nodesim_member(FItem, FName),
				get(To, name, TName),
				% get_nodesim_member(TItem, TName),
				(repeat,
				not(remove_valsim_editor(F, FName, TName))),!,
				lat:bot(Rel),
				with_output_to(codes(Codes), write(Rel)),
				string_codes(String, Codes),
				add_valsim_editor(F, FName, TName, String),
				
				send(F, set_modified_state)
			;	false
			)
	),
	new(Connection, sim_connection(From, To)),
	(WhoCalls \== program -> send(Connection, edit_connect)).

ext2(L, X, Y, R) :-
	ext1(L, X, S),
	ext1(S, Y, R).

ext1(L, X, B) :-
	append(A, [X|B], L).

	
free_box(Connect) :->
	get(Connect, all_constraints, Constraints),
	get(Constraints, head, C),
	get(C, to, Box),
	get(Box, all_constraints, Constraints2),
	get(Constraints2, head, C2),
	get(C2, to, Text),
	send(Text, free),
	send(Box, free).
	
free_connect(Link) :->
	get(Link, frame, F),
	get(Link, from, From),
	get(Link, to, To),
	get(From, name, N1),
	get_nodesim_member(Fname, N1),
	get(To, name, N2),
	get_nodesim_member(Tname, N2),
	send(Link, free_box),
	send(Link, free),
	retractall(sim:r(Fname, Tname, _)),
	retractall(sim:r(Tname, Fname, _)),
	
	%% Actualizar matrix
	get(F, member, dialog_matrix, DM),
	get(DM, member, matrix, Tab),
	get(Tab, get_cell_node, N1, N2, Cell1),
	get(Tab, get_cell_node, N2, N1, Cell2),
	lat:bot(Bot),
	with_output_to(codes(Codes), write(Bot)),
	string_codes(String, Codes),
	send(Cell1, value, String),
	send(Cell2, value, String),
	
	%% Actualizar editor
	(repeat,
	not(remove_valsim_editor(F, N1,N2))),!,
	send(F, set_modified_state).

free_connect_graph(Link) :->
	get(Link, frame, F),
	get(Link, from, From),
	get(Link, to, To),
	get(From, name, N1),
	get_nodesim_member(Fname, N1),
	get(To, name, N2),
	get_nodesim_member(Tname, N2),
	send(Link, free_box),
	send(Link, free).
	
edit_connect(Link) :->
	get(Link, frame, F),
	new(ND, dialog('Enter new value')), 
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	lat:bot(Bot),
	send(ND, append, new(TI, text_item(value))), 
	send(ND, append, button(ok, and(message(Link, check_return, F, TI?selection),
									message(ND, return, TI?selection)))),
	send(ND, append, button(cancel, message(ND, return, Bot))),
	send(ND, default_button, ok),
	
	get(ND, confirm_centered,
			    F?area?center, Return),
	send(ND, destroy),
	% Return \== @nil,
	
	% Obteniendo Text del Link
	get(Link, all_constraints, Constraints),
	get(Constraints, head, C),
	get(C, to, Box),
	get(Box, all_constraints, Constraints2),
	get(Constraints2, head, C2),
	get(C2, to, Text),
	send(Link, edit_sim_r, Return),
	get(Text, value, PrevVal),
	send(Text, value, Return),
	
	%% Actualizar editor
	get(Link, from, From),
	get(Link, to, To),
	get(From, name, N1),
	get_nodesim_member(FName, N1),
	get(To, name, N2),
	get_nodesim_member(TName, N2),
	(	get_linesim_editor(F, N1,N2, Pos)
	->	(repeat,
		not(remove_valsim_editor(F, N1,N2))),!,	
		add_valsim_editor(F, N1, N2, Return, Pos)
	;	add_valsim_editor(F, N1, N2, Return)
	),
	
	%% Actualizar matrix
	get(F, member, dialog_matrix, DM),
	get(DM, member, matrix, Tab),
	get(Tab, get_cell_node, N1, N2, Cell1),
	get(Tab, get_cell_node, N2, N1, Cell2),
	send(Cell1, value, Return),
	send(Cell2, value, Return),
	
	%% eliminar si es bottom del editor y graph
	sim:r(FName, TName, Value),
	(	Value == Bot
	->	retractall(sim:r(FName, TName, _)),
		retractall(sim:r(TName, FName, _)),
		send(Link, free_connect_graph),
		(repeat,
		not(remove_valsim_editor(F, N1, N2))),!
	;	true
	),
	
	send(F, set_modified_state).


check_return(_, F, Node) :->
	if(get_node_member(Node, _),(
		true
	),
		(send(F, report, error, 'Value is not in lattice'),
		fail)
	),
	if(Node = '',(
		send(F, report, error, 'Value can\'t be empty'),
		fail
	),
		true
	).
	
edit_sim_r(Link, Value) :->
	get_node_member(Value, NValue),
	get(Link, from, From),
	get(Link, to, To),
	get(From, name, N1),
	get_nodesim_member(Fname, N1),
	get(To, name, N2),
	get_nodesim_member(Tname, N2),
	retractall(sim:r(Fname, Tname, _)),
	retractall(sim:r(Tname, Fname, _)),
	assertz(sim:r(Fname, Tname, NValue)),
	assertz(sim:r(Tname, Fname, NValue)).
	
:-  pce_global(@linksim_recogniser, make_linksim_recogniser).
 
make_linksim_recogniser(G) :-
	new(M, move_gesture(left, '')),
 	new(P, popup_gesture(new(Pop, popup))),
 	send_list(Pop, append,
                   [menu_item(delete_link, message(@arg1, free_connect)),
					menu_item(change_value, message(@arg1, edit_connect))
                   ]),
   	new(G, handler_group(M, P)).

event(Link, Ev:event) :->
	"Make it work"::
	(   send(@linksim_recogniser, event, Ev)
	->  true
	;   send(Link, send_super, event, Ev)
	).
	
:- pce_end_class.