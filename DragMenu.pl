:- pce_begin_class(dragmenu(name), menu).

:- pce_global(@dragmenu_recogniser, make_dragmenu_recogniser).

initialise(Menu, Name:name) :->
	"Create a cycle menu"::
	send(Menu, send_super, initialise),
	send(Menu, name, Name),
	send(Menu, kind, cycle).

make_dragmenu_recogniser(G) :-
	new(G, handler_group(resize_gesture(left),
					drag_and_drop_gesture(left))).
event(B, Ev:event) :->
	( send(B, send_super, event, Ev)
	; send(@dragmenu_recogniser, event, Ev)
	).

drop(Obj, Name, _) :->
	(	get(Obj, member, Name, _), NName = Name
	;	atom_codes(Name, Codes),
		select(44, Codes, 46, NCodes),
		atom_codes(NName, NCodes),
		get(Obj, member, NName, _)
	),
	send(Obj, selection, NName).

add_member(Menu, Name) :->
	new(Item, menu_item(Name, @default, Name?print_name)),
	send(Menu, append, Item).

get_selection(Obj, Value, Xi) :-
	get(Obj, selection, Xi)
	->	true
	;   get(Obj, selection, V),
		get(V, value, VV),
		(	atom_number(VV, Value);	Value = VV	).
	
:- pce_end_class.

:- pce_extend_class(menu_item).
	
print_name(Name:name, NName):-
	string_lower(Name, NName).

:- pce_end_class.
