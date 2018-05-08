:- module(emacs_find, []).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).
:- use_module(library(hyper)).
:- use_module(library(pce_report)).

:- pce_global(@editor_find_history, new(chain)).
:- pce_global(@editor_replace_history, new(chain)).

:- pce_begin_class(editor_find_dialog, dialog, "Find and replace in text").

variable(search_origin,	int*, get, "Start of the search").
variable(search_point,	int*, get, "Current search location").

initialise(D, E:editor) :->
	"Create for editor"::
	send_super(D, initialise, 'Find and Replace'),
	send(D, pen, 0),
	send(D, append, new(TI1, text_item(find))),
	send(TI1, value_set, @editor_find_history),
	send(D, append, new(TI2, text_item(replace))),
	send(TI2, value_set, @editor_replace_history),
	send(D, append, new(M, menu(options, marked, @nil))),
	send(M, multiple_selection, @on),
	send(M, show_label, @off),
	send_list(M, append, [match_case, whole_word]),
	get(E, exact_case, MatchCase),
	send(M, selected, match_case, MatchCase),
	send(D, append, button(next)),
	send(D, append, button(replace)),
	send(D, append, button(replace_all)),
	send(D, append, button(close)),
	send(D, default_button, next),
	new(_, partof_hyper(E, D, search, editor)),
	send(new(R, report_dialog), below, D),
	send(R, pen, 0),
	send(D, background, gainsboro),
	send(R, background, gainsboro),

	new(_, partof_hyper(E, D, search, editor)),

	new(G, key_binding(editor_find_dialog)),
	send(G, function, 'ESC', close),
	
    get(E, caret, Caret),
    send(D, slot, search_origin, Caret),
    send(D, slot, search_point, Caret).

editor(D, E:editor) :<-
	"Get associated editor"::
	get(D, hypered, editor, E).

open(D) :->
	"Open for attached editor"::
	get(D, editor, E),
	get(E, frame, F),
	send(D, transient_for, F),
	send(D, modal, transient),
	get(D, confirm_centered,
						F?area?center, _).

close(D) :->
	"Cancel search dialog"::
	(   get(D, search_origin, Origin),
	    Origin \== @nil
	->  get(D, editor, E),
	    send(E, caret, Origin)
	;   true
	),
	send(D, destroy).

append_history(_, Find:name, Replace:name) :->
	"Add search to history"::
	send(@editor_find_history, delete_all, Find),
	send(@editor_find_history, prepend, Find),
	send(@editor_replace_history, delete_all, Replace),
	send(@editor_replace_history, prepend, Replace).

point(D, Here:int) :->
    send(D, slot, search_point, Here).

next(D) :->
	%% hypered
	get(D, editor, E),
	get(E, text_buffer, B),
	get(B, length, Length),
	
	get(D, member, options, Opt),
	get(Opt, selected, match_case, Case),
	get(Opt, selected, whole_word, Word),
	get(D, member, find, TI1),
	get(TI1, selection, For),

	get(D, slot, search_point, Point),
	(	Point > Length
	->	Next_point = 1
	;	Next_point = Point + 1
	),
	(	get(B, find, Next_point, For, 1, start, Case, Word, Index)
	->	true
	;	Index = 0
	),
	send(D, slot, search_point, Index),
	send(E, caret, Index).

replace(D) :->
	%% hypered
	get(D, editor, E),
	get(E, frame, F),
	
	ok_replace(F),

	get(E, text_buffer, B),
	get(B, length, Length),
	
	get(D, member, options, Opt),
	get(Opt, selected, match_case, Case),
	get(Opt, selected, whole_word, Word),
	get(D, member, find, TI1),
	get(D, member, replace, TI2),
	get(TI1, selection, For),
	get(TI2, selection, New),
	atom_length(For, Chars),

	get(D, slot, search_point, Point),
	(	Point > Length
	->	Next_point = 0
	;	Next_point = Point
	),
	(	get(B, find, Next_point, For, 1, start, Case, Word, Index)
	->	send(B, delete, Index, Chars),
		send(B, insert, Index, New)
	;	Index = 0
	),
	send(D, slot, search_point, Index),
	send(E, caret, Index).

replace_all(D) :->
	%% hypered
	get(D, editor, E),
	get(E, frame, F),
	ok_replace(F),

	get(E, text_buffer, B),
	get(B, length, Length),
	
	get(D, member, options, Opt),
	get(Opt, selected, match_case, Case),
	get(Opt, selected, whole_word, Word),
	get(D, member, find, TI1),
	get(D, member, replace, TI2),
	get(TI1, selection, For),
	get(TI2, selection, New),
	atom_length(For, Chars),
	Int = 0,
	repeat,
	(	get(B, find, Int, For, 1, start, Case, Word, Index),
		Index < Length
	->	send(B, delete, Index, Chars),
		send(B, insert, Index, New),
		Int = Index + 1,
		fail
	;	!
	).
	
ok_replace(F) :-
	send(F, get_edit_mode)
	->  true
	;	get(F, msgbox, 'Editor must be in write mode to rename truth degree references. Activate now?', RT),
		RT \= @nil,
		send(F, set_edit_mode_on).
	
:- pce_end_class(editor_find_dialog).
