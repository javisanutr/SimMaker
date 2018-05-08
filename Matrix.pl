% :- module(matrix,[]).

:- use_module(library(tabular)).
:- use_module(library(autowin)).
:- use_module(library(pce)).

% matrix(List, Lat) :-
	% new(T, matrix(List, Lat)).

:- pce_begin_class(matrix, tabular).

variable(row_length, int, both, "Row size").

initialise(T) :->
	send(T, send_super, initialise),
	send(T, border, 1),
	send(T, cell_spacing, -1),
	send(T, rules, all),
	send(T, set_row_length, 0).

:- pce_global(@matrix_cell_recogniser, make_matrix_cell_recogniser).

make_matrix_cell_recogniser(G) :-
	new(Cell, @event?receiver),
	new(C, click_gesture(left, '', single, message(Cell, dialog_value))),
	new(G, handler_group(C)).

set_column_names(T, List) :-
	send(T, append, ''),
	length(List, N),
	send(T, set_row_length, N+1),
	fill_column_table_names(T, List),!,
	send(T, next_row).

set_row_length(Tab, RowS) :->
	send(Tab, slot, row_length, RowS).
	
get_row_length(Tab, RowS) :<-
	get(Tab, slot, row_length, RowS).
	
% Obtener texto(objeto celda) X Y de Tabular
get_cell(Tabular, X, Y, Text) :<-
	get(Tabular, graphicals, Cells),
	get(Tabular, get_row_length, RowS),
	get(Cells, nth1, RowS * Y + X + 1, Text).

% Obtener celda de la relación de NodoA y NodoB
get_cell_node(Tab, NodeAS, NodeBS, Cell) :<-
	sim:lista(L),
	get_nodesim_member(NodeA, NodeAS),
	get_nodesim_member(NodeB, NodeBS),
	nth1(X,L,NodeA),
	nth1(Y,L,NodeB),
	get(Tab, get_cell, X, Y, Cell).
	

fill_column_table_names(_, []).
fill_column_table_names(T, [Item|List]) :-
	get_nodesim_member(Item, Column),
	send(T, append, Column, bold, center),
	fill_column_table_names(T, List).

set_row_length(T,N) :->
	send(T, row_length, N).

:- pce_end_class.

:- pce_begin_class(matrix_cell(value), text).

initialise(Cell, Value) :->
	send(Cell, send_super, initialise),
	send(Cell, value, Value),
	send(Cell, recogniser, @matrix_cell_recogniser).

dialog_value(Cell) :->
	get(Cell, frame, F),
	get(Cell, get_cell_posX, X),
	get(Cell, get_cell_posY, Y),
	if(X == Y, (send(F, report, error, 'Change not allowed'),fail), true),
	new(ND, dialog('Enter new value')), 
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(TI, text_item(value))), 
	send(ND, append, button(ok, and(message(Cell, check_return, F, TI?selection),
									message(ND, return, TI?selection)))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),
	
	get(ND, confirm_centered,
			    F?area?center, Return),
	send(ND, destroy),
	Return \== @nil,
	get(Return, strip, Return2),
	get_node_member(Return2, Value),
	
	sim:lista(L),
	nth1(X,L,NodeA),
	nth1(Y,L,NodeB),
	get(Cell, device, Tabular),
	get(Tabular, get_cell, Y, X, Cell2),
	
	retractall(sim:r(NodeA, NodeB, _)),
	retractall(sim:r(NodeB, NodeA, _)),
	lattice_maker:lat:bot(Bot),
	(	Value = Bot
	;	assertz(sim:r(NodeA, NodeB, Value)),
		assertz(sim:r(NodeB, NodeA, Value))
	),
	
	send(Cell, value, Return2),
	send(Cell2, value, Return2),
	
	%% Actualizar grafo
	get_nodesim_member(NodeA, StringA),
	get_nodesim_member(NodeB, StringB),
	get(F, node, StringA, NodeAObj),
	get(F, node, StringB, NodeBObj),
	(	get(NodeAObj, get_connection, NodeBObj, Link)
	;	make_simconnect(NodeAObj, NodeBObj, program, Link)
	),
	%% Obteniendo Text del Link
	get(Link, all_constraints, Constraints),
	get(Constraints, head, C),
	get(C, to, Box),
	get(Box, all_constraints, Constraints2),
	get(Constraints2, head, C2),
	get(C2, to, Text),
	send(Text, value, Return2),
	
	%% Actualizar editor
	(	get_linesim_editor(F, StringA, StringB, Pos)
	->	(repeat,
		not(remove_valsim_editor(F, StringA, StringB))),!,
		add_valsim_editor(F, StringA, StringB, Return2, Pos)
	;	(repeat,
		not(remove_valsim_editor(F, StringA, StringB))),!,
		add_valsim_editor(F, StringA, StringB, Return2)
	),
	
	%% Mixed editor and graph
	lat:bot(Bot),
	(	Value == Bot
	->	send(Link, free_connect_graph),
		(repeat,
		not(remove_valsim_editor(F, StringA, StringB))),!
	;	true
	),
	send(F, set_modified_state).


% Obtener fila de Text
get_cell_posX(Text, X) :<-
	get(Text, layout_interface, TC),
	get(TC, column, X2),
	X is X2-1.

% Obtener columna de Text
get_cell_posY(Text, Y) :<-
	get(Text, layout_interface, TC),
	get(TC, row, Y2),
	Y is Y2-1.
	
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

event(Cell, Ev:event) :->
	"Make it work"::
	(   send(@matrix_cell_recogniser, event, Ev)
	->  true
	;   send(Cell, send_super, event, Ev)
	).
	
:- pce_end_class.
