:- pce_global(@from_handle,
              new(handle(w/2, 0, out, out))).
:- pce_global(@to_handle,
              new(handle(w/2, h, in, in))).
:- pce_global(@fromto_link,
              new(link(out, in,
                       line(arrows := second)))).

:- pce_begin_class(graph_connection(from, to), connection).

initialise(Connect, From:lattice_node, To:lattice_node) :->
	"Create From-To Connection"::
	send(From, handle, @from_handle),
	send(To, handle, @to_handle),
	send(Connect, send_super, initialise, From, To, @fromto_link),
	send(Connect, recogniser, @link_recogniser).
	
make_connect(From, To, WhoCalls) :-
	get(From, name, N1),
	get_node_member(N1, Fname),
	get(To, name, N2),
	get_node_member(N2, Tname),
	(	lat_graph:top(T) ->	Fname \= T	; true	), 
	(	lat_graph:bot(B) -> Tname \= B	; true	),
	(	\+ lat_graph:leq(Tname, Fname)	),
	(	WhoCalls == program
		-> 	true
		;	get(From, frame, F),
			ok_arrow_mode(F),
			send(F, set_modified_state)
	),
	new(_, graph_connection(From, To)),
	(	lat_graph:arc(Fname, Tname)
		-> true
		; assertz(lat_graph:arc(Fname, Tname))
	).

free_connect(Link) :->
	get(Link, frame, F),
	get(Link, from, From),
	get(Link, to, To),
	get(From, name, N1),
	get_node_member(N1, Fname),
	get(To, name, N2),
	get_node_member(N2, Tname),
	send(Link, free),
	retract(lat_graph:arc(Fname, Tname)),
	send(F, set_modified_state).
	 
:-  pce_global(@link_recogniser, make_link_recogniser).
 
make_link_recogniser(G) :-
	new(M, move_gesture(left, '')),
 	new(P, popup_gesture(new(Pop, popup))),
 	send_list(Pop, append,
                   [menu_item(delete_link, message(@arg1, free_connect))  
                   ]),
   	new(G, handler_group(M, P)).

event(Link, Ev:event) :->
	"Make it work"::
	(   send(@link_recogniser, event, Ev)
	->  true
	;   send(Link, send_super, event, Ev)
	).
	
:- pce_end_class.