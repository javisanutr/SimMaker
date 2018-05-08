/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

        This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(lattice_maker,
          [ lattice_maker/0
          ]).

:- use_module(library(pce)).
:- use_module(library(dragdrop)).
:- use_module(library(keybinding)).
:- use_module(library(help_message)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).

:- consult('DragMenu.pl').
:- consult('LatticeConnect.pl').
:- consult('LatticeEval.pl').
:- consult('LatticeFind.pl').
:- consult('LatticeLayout.pl').
:- consult('LatticeNode.pl').
:- consult('LatticePrint.pl').
:- consult('LatticeTest.pl').
:- consult('LatticeXml.pl').
:- consult('SimNode.pl').
:- consult('SimConnect.pl').
:- consult('Matrix.pl').



:- pce_autoload(tool_bar, library(toolbar)).
:- pce_autoload(finder, library(find_file)).
:- pce_autoload(helper, library(pce_helper)).
:- pce_autoload(prolog_predicate, library(prolog_predicate)).

:- pce_global(@finder, new(finder)).
:- pce_global(@helper, new(helper)).
:- pce_global(@gvl, new(lattice_maker)).
:- pce_global(@gvs, new(sim_maker)).
:- pce_help_file(latticehelp, 'index.xml').

:- dynamic lat_graph:member/1.
:- dynamic lat_graph:members/1.
:- dynamic lat_graph:top/1.
:- dynamic lat_graph:bot/1.
:- dynamic lat_graph:arc/2.
:- dynamic lat_graph:leq/2.
:- dynamic lat_graph:level/2.
:- dynamic lat_graph:distance/3.
:- dynamic lat_graph:fromNode/1.
:- dynamic sim:r/3.
:- dynamic sim:lista/1.
:- dynamic floper.

:- pce_image_directory('.\\bitmaps').

resource(imgnew, image, image('new.xpm')).
resource(imgopen, image, image('open.xpm')).
resource(imgsave, image, image('save.xpm')).
resource(imgquit, image, image('quit.xpm')).
resource(imgimage, image, image('image.xpm')).
resource(imgxml, image, image('xml.xpm')).
resource(imgprint, image, image('print.xpm')).
resource(imgclear, image, image('clear.xpm')).
resource(imgedit, image, image('edit.xpm')).
resource(imgadd, image, image('add.xpm')).
resource(imgclean, image, image('clean.xpm')).
resource(imgfind, image, image('find.xpm')).
resource(imggraph, image, image('complete.xpm')).
resource(imgnode, image, image('node.xpm')).
resource(imgarrow, image, image('arrow.xpm')).
resource(imgnormalize, image, image('normalize.xpm')).
resource(imgsendup, image, image('sendup.xpm')).
resource(imgundo, image, image('undo.xpm')).
resource(imgleq, image, image('restore_leq.xpm')).
resource(imgeditfind, image, image('find_replace.xpm')).
resource(imghelp, image, image('help.xpm')).
resource(imgswitch, image, image('switch.xpm')).


lattice_maker :-
	% guitracer,
	free(@gvl),
	free(@gvs),
	new(@gvl, lattice_maker),
	new(@gvs, sim_maker),
    send(@gvl, open_centered).

sim_gui :-
	get_new_state(@gvl)
	->	ok_nonew(@gvl)
	;(
		% ok_normalize(GV),
		ok_changes(@gvl)
	),
	% lat:top(Top),
	% retractall(sim:lista(_)),
	% assertz(sim:lista([])),
	% retractall(sim:r(_,_,_)),
	% assertz(sim:r(X,X,Top)),
	send(@gvl, show, @off),
	send(@gvs, open_centered).
	% (	get_modified_state(@gvs)
	% ->	send(@gvs, sim_to_graph)
	% ;	true
	% ).

lattice_gui :-
	% send(@gvl, show, @on),
	send(@gvs, show, @off),
	send(@gvl, open_centered).


:- pce_begin_class(lattice_maker, frame).

variable(drawnode, bool, both, "Node selected").
variable(drawarrow,     bool, both, "Arrow selected").
variable(new, bool, both, "New Graph").
variable(modified, bool, both, "Graph modified").
variable(editable, bool, both, "Editor editable").
variable(filename, name, both, "File loaded").
variable(redrawn, bool, both, "Graph redrawn").
variable(max_nodes_layer, int, both, "Nodes in widest layer").
variable(loaded_lattice,bool,both,"The lattice is being loaded").
variable(property, char_array, both, "Property to test").

% Local configuration
locale_create(_, default, [alias(lattice), decimal_point('.'), thousand_sep(''), grouping([repeat(3)])]).
set_locale(lattice).

initialise(GV) :->
	"Create lattice_maker"::
	send(GV, send_super, initialise, 'Lattice Maker'),
	send(GV, background, gainsboro),

	send(GV, append, new(P, picture)),
	send(P, size, size(80, 350)),
	send(P, label, 'Graphical representation of lattice'),

	send(new(DD, dialog(edit_toolbar)), below, P),
	send(DD, size, size(80, 25)),
	send(DD, background, gainsboro),
	send(DD, pen, 0),
	send(DD, append, new(TB2, tool_bar)),
	send(TB2, colour, gainsboro),
	send(TB2, gap, size(3, 3)),
	fill_edit_toolbar(TB2),
	
	send(new(V, view), below, P),
	send(V, font, font(arial, normal, 12)),
	send(V, size, size(80, 15)),
    send(V, editable, @off),
	send(V, label, 'Connective definitions in lattice'),

	new(Pm, message(GV, newnode, @event?position)),
	send(P, recogniser, click_gesture(left, '', single,
			  message(Pm, execute))),
	send(V, recogniser, popup_gesture(new(Pop, popup))),
	send_list(Pop, append,
			  [ menu_item('undo       Ctrl+Z', message(@arg1?editor, undo),
												condition := message(GV, get_edit_mode)),
				menu_item('copy       Ctrl+C', message(@arg1?editor, copy),
												condition := message(GV, get_edit_mode)),
				menu_item('paste      Ctrl+V', message(@arg1?editor, paste),
												condition := message(GV, get_edit_mode)),
				menu_item('cut         Ctrl+X', message(@arg1?editor, cut),
												condition := message(GV, get_edit_mode)),
				menu_item('find        Ctrl+F', message(GV, find_in),
												condition := message(GV, get_edit_mode)),
				menu_item('select_all Ctrl+A', message(@arg1?editor, mark_whole_buffer), end_group := @on,
												condition := message(GV, get_edit_mode)),
				menu_item(toggle_edit_mode, message(@arg1, toggle_edit_mode), end_group := @on),
				menu_item(show_key_bindings, message(@prolog, show_key_bindings, @arg1))
				]),

	send(V, key_binding, '\\C-f', message(GV, find_in)),
	send(V, key_binding, '\\C-a', message(V?editor, mark_whole_buffer)),
	send(new(Oper, dialog), right, V),
	send(Oper, name, dialog_eval),
	send(Oper, background, gainsboro),
	fill_operators_dialog(Oper),

	send(new(R, dialog(report)), below, V),
	send(R, background, gainsboro),
	fill_dialog(R),

	send(new(Menu, dialog(dialog_menu)), above, P),
	% #D8D8D8 = gainsboro
	send(Menu, background, gainsboro),
	send(Menu, pen, 0),
	send(Menu, gap, size(3, 3)),
	send(Menu, append, new(MB, menu_bar)),
	fill_menu(MB),
	send(Menu, append, new(TB, tool_bar)),
	send(TB, colour, gainsboro),
	send(TB, gap, size(3, 3)),
	fill_toolbar(TB),
	send(TB, left, new(SimB, button(simMode, message(@prolog, sim_gui), sim_mode))),
	
	send(GV, icon, image('image.xpm')),
	send(GV, fit),
	send(GV, set_filename, ''),
	send(GV, set_initial_state).

frame_clear(F) :->
	get(F, members, W),
	send(W, for_all, (message(F, delete, @arg1))).
	
fill_menu(MB) :-
	get(MB, frame, FrameMB),
	get(FrameMB, member, picture, P),

	send(MB, append, new(F, popup(file))),
	send(MB, append, new(E, popup(edit))),
	send(MB, append, new(G, popup(graphic))),
	send(MB, append, new(A, popup(connective))),
	send(MB, append, new(D, popup(distance))),
	send(MB, append, new(H, popup(help))),

	send(F, append, menu_item(new_lattice, message(FrameMB, new))),
	send(F, append, menu_item(load_lattice, message(FrameMB, load))),
	send(F, append, menu_item(save_to_file, message(FrameMB, save),
									condition := message(FrameMB, lattice_noempty))),
	send(F, append, menu_item(save_as, message(FrameMB, saveAs),
									condition := message(FrameMB, lattice_noempty))),
	send(F, append, menu_item(print, message(FrameMB, print),
									condition := message(FrameMB, graph_noempty, P),
									end_group := @on)),
	send(F, append, menu_item(export_image, message(FrameMB, image),
									condition := message(FrameMB, graph_noempty, P))),
	send(F, append, menu_item(export_lattice_to_XML, message(FrameMB, export_xml),
									condition := message(FrameMB, lattice_noempty),
									end_group := @on)),
	send(F, append, menu_item(exit_program, message(FrameMB, bye))),

	send(E, append, menu_item(clear_text_editor, message(FrameMB, clear_editor),
									condition := message(FrameMB, get_edit_mode))),
	send(E, append, menu_item(toggle_edit_mode, message(FrameMB, toggle_edit_mode))),
	send(E, append, menu_item(add_connective, message(FrameMB, add_connective),
									condition := message(FrameMB, get_edit_mode))),
	send(E, append, menu_item(find_and_replace, message(FrameMB, find_in),
									condition := message(FrameMB, get_edit_mode),
									end_group := @on)),
	send(E, append, menu_item(redraw_graph_from_lattice, message(FrameMB, lattice_to_graph),
									condition := message(FrameMB, lattice_noempty))),
	send(E, append, menu_item(undo_redraw_graph, message(FrameMB, undo_send_graph),
									condition := message(FrameMB, lattice_noempty), 
                                    end_group := @on)),

    send(E, append, menu_item(restore_leq, message(FrameMB, restore_view_leq),
									condition := message(FrameMB, lattice_noempty))),
    
	send(G, append, menu_item(clear_graph_editor, message(FrameMB, clear_graph),
									condition := message(FrameMB, graph_noempty, P),
									end_group := @on)),
	send(G, append, menu_item(set_truth_degree_mode, message(FrameMB, set_selected_node))),
	send(G, append, menu_item(set_leq_arrow_mode, message(FrameMB, set_selected_arrow),
									end_group := @on)),
	send(G, append, menu_item(find_truth_degree, message(FrameMB, find),
									condition := message(FrameMB, graph_noempty, P))),
	send(G, append, menu_item(complete_graph, message(FrameMB, complete),
									condition := message(FrameMB, graph_noempty, P))),
	send(G, append, menu_item(normalize_graph, message(FrameMB, normalize),
									condition := message(FrameMB, graph_noempty, P))),

	send(A, append, menu_item(evaluate, message(FrameMB, eval_selected_connective),
									condition := message(FrameMB, lattice_noempty))),
	send(A, append, menu_item(test_connective, message(FrameMB, test_selected_connective),
									condition := message(FrameMB, lattice_noempty))),
									
	send(D, append, menu_item(evaluate, message(FrameMB, eval_distance),
									condition := message(FrameMB, lattice_noempty))),
    send(D, append, menu_item(test, message(FrameMB, check_distance),
									condition := message(FrameMB, lattice_noempty))),
	send(D, append, menu_item(generate, message(FrameMB, restore_view_distance),
									condition := message(FrameMB, lattice_noempty))),
    
	send_list(H, append, [ menu_item(user_manual, message(@helper, give_help, latticehelp, 'latticehelp')),
						   menu_item(about, message(FrameMB, about_dialog))
						   ]).

about_dialog(F) :->
	new(ND, dialog('About: LatticeMaker')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),
	add_label(ND, about1, 'LatticeMaker v1.0', bold, blue, 14),
	add_label(ND, about2, 'University of Castilla-La Mancha (Spain)', normal, black, 12),
	add_label(ND, about3, 'SWI-Prolog and XPCE Graphical Interface for Lattice Edition',
							normal, black, 12),
	add_label(ND, about3, 'by Maria del Señor Martinez Ruiz', normal, black, 12),
	add_label(ND, about3, 'mds.martinezruiz@gmail.com', normal, black, 12),
	send(ND, append, button(close, message(ND, destroy))),
	send(ND, open).

fill_toolbar(TB) :-
	get(TB, frame, FrameTB),

	send(TB, append, tool_button(message(FrameTB, new), resource(imgnew), new_lattice)),
	send(TB, append, tool_button(message(FrameTB, load), resource(imgopen), load_lattice)),
	send(TB, append, tool_button(message(FrameTB, save), resource(imgsave), save_to_file)),
	send(TB, append, tool_button(message(FrameTB, print), resource(imgprint), print)),
	send(TB, append, tool_button(message(FrameTB, image), resource(imgimage), export_image)),
	send(TB, append, tool_button(message(FrameTB, export_xml), resource(imgxml), export_to_xml)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, clear_graph), resource(imgclear), clear_graph_editor)),
	send(TB, append, tool_button(message(FrameTB, set_selected_node), resource(imgnode), set_truth_degree_mode)),
	send(TB, append, tool_button(message(FrameTB, set_selected_arrow), resource(imgarrow), set_leq_arrow_mode)),
	send(TB, append, tool_button(message(FrameTB, find), resource(imgfind), find_truth_degree)),
	send(TB, append, tool_button(message(FrameTB, complete), resource(imggraph), complete)),
	send(TB, append, tool_button(message(FrameTB, normalize), resource(imgnormalize), redraw_normalize_graph)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(@helper, give_help, latticehelp, 'latticehelp'), resource(imghelp), help)),
	% send(TB, append, tool_button(message(@prolog, sim_gui), resource(imgswitch), sim_mode)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, bye), resource(imgquit), exit_program)),
	send(TB, append, gap),
	send(TB, append, gap),
	send(TB, append, gap),
	send(TB, append, gap),
	send(TB, append, gap).

fill_edit_toolbar(TB) :-
	get(TB, frame, FrameTB),
	send(TB, append, tool_button(message(FrameTB, clear_editor), resource(imgclean), clear_text_editor)),
	send(TB, append, tool_button(message(FrameTB, toggle_edit_mode), resource(imgedit), toggle_edit_mode)),
	send(TB, append, tool_button(message(FrameTB, add_connective), resource(imgadd), add_connective)),
	send(TB, append, tool_button(message(FrameTB, find_in), resource(imgeditfind), find_and_replace)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, lattice_to_graph), resource(imgsendup), redraw_graph_from_lattice)),
	send(TB, append, tool_button(message(FrameTB, undo_send_graph), resource(imgundo), undo_redraw_graph)),
    send(TB, append, gap),
    send(TB, append, tool_button(message(FrameTB, restore_view_leq), resource(imgleq), restore_leq)).

fill_operators_dialog(D) :-
	add_label(D, aggrhelp, 'Select a connective to EVAL or TEST', normal, blue, 12),
    % Connective Control
	send(D, append, new(B, menu(connectives, cycle, message(D, fill_terms)))),
	send_list(B, append, [empty]),
	send(B, alignment, left),
    send(B, label, 'Connective 1 ($1)'),
    % Second connective Control
	send(D, append, new(S, menu(second, cycle, message(D, fill_terms))),right),
	send_list(S, append, [empty]),
	send(S, alignment, center),
    send(S, label, 'Connective 2 ($2)'),
	send(S, active, @off),
    
    % Properties definition box
    send(D,append,new(PD1,text_item(definition))),
    send(PD1,width,70),
    send(PD1,editable,@off),
    send(D,append,new(PD2,text_item(param2))),
    send(PD2,width,70),
    send(PD2,editable,@off),
    send(PD2,displayed,@off),

	send(D, append, new(W1, dialog_group(group1, group))),
	send(W1, alignment, center),
	send(W1, append, new(W2, dialog_group(group11, group))),
	send(new(W3, dialog_group(group12, group)), left, W2),
    send(new(W4, dialog_group(connectives, box)), below, W3),
    send(new(W5, dialog_group(distances, box)), below, W2),
    
	add_label(W2, infor1, 'Select the Terms to EVAL', normal, blue, 12),
	add_label(W2, infor2, '(Shift + Drag and Drop can be used)', normal, blue, 12),
	add_dragmenu(W2, 1, 6),

	add_label(W3, infor1, 'Select the property to TEST', normal, blue, 12),
    add_label(W3,infor2,'Category',bold,black,12),
	
    send(W3,append,new(CC,menu(category,choice,message(D,fill_properties,@arg1)))),
    send_list(CC,append,['Basic','Combined','Multiple']),
    send(CC,show_label,@off),
    send(W3,append,new(PLB,list_browser)),
    send(PLB,size,size(20,10)),
    send(PLB,below,CC),
    send(PLB,label,'Property'),
    send(PLB,alignment,center),
    send(PLB,select_message,message(D,options,@arg1?key)),
    add_prop_list('Basic',PLB),
    
	send(D, gap, size(40, 10)),
	new(BEval, button(eval, message(D, eval_selected_connective))),
	send(W4, append, BEval),
	send(BEval, font, font(arial, bold, 12)),
	send(BEval, colour, blue),
    send(BEval, help_message, tag, 'Evaluate the connective and terms selected'),

	new(Output, view),
	
	new(Box1,box(40,40)),
	send(Box1, right, Output),
	send(Box1, radius, 360),
	send(Box1,fill_pattern,@nil),

	send(W4, append, new(BTest, button(test, message(D, test_selected_connective))), right),
	send(BTest, font, font(arial, bold, 12)),
	send(BTest, colour, blue),
    send(BTest, help_message, tag, 'Test the property selected'),
    
    send(W5, append, new(BDistEval, button(eval, message(D, eval_distance))), right),
	send(BDistEval, font, font(arial, bold, 12)),
	send(BDistEval, colour, blue),
    send(BDistEval, help_message, tag, 'Eval distance'),
    send(BDistEval, active, @off),

	send(W5, append, new(BDistCheck, button(test, message(D, check_distance))), right),
	send(BDistCheck, font, font(arial, bold, 12)),
	send(BDistCheck, colour, blue),
    send(BDistCheck, help_message, tag, 'Check distance'),
	
    send(W5, append, new(BDistGen, button(generate, message(D, restore_view_distance))), right),
	send(BDistGen, font, font(arial, bold, 12)),
	send(BDistGen, colour, blue),
    send(BDistGen, help_message, tag, 'Generate the distances'),
    
    
	send(D, append, Output, next_row),
	send(Output, size, size(50, 8)),
	send(Output, alignment, center),
	send(Output, editable, @off),

	send(D, resize_message, message(D, layout, @arg2)).

    
fill_properties(F,Arg) :->
    get(F, member(dialog_eval), D),
    get_container_optgroup(D,Opt),
    get(Opt,member,list_browser,LB),
    add_prop_list(Arg,LB).
    
add_prop_list('Basic',LB) :- 
        send(LB,clear),
        send_list(LB,append,[frontier_top, frontier_bot, increasing, non_increasing, decreasing,non_decreasing,monotony,idempotency,commutativity,associativity]).
add_prop_list('Multiple',LB) :- 
        send(LB,clear),
        send_list(LB,append,[switchness, adjointness, distributivity]).
add_prop_list('Combined',LB) :- 
        send(LB,clear),
        send_list(LB,append,[t_norm, t_conorm, implication, aggregator]).
    
options(F, Opt) :->
	get(F, member(dialog_eval), D),
    % Unlock the Second connective Combobox
	get(D, member, second, SC),
    (       two_connectives(Opt)
    ->      send(SC, active, @on)
    ;       send(SC, active, @off)
    ),
    
    get(D,member,definition,PD1),
    get(D,member,param2,PD2),
    send(PD1,clear),
    send(PD2,clear),
    get_connective(F,D,connectives,Name,_),
    get_connective(F,D,second,Name2,_),
    
    send(F, slot, property, Opt),
    
    (
        % The connective is applied to two params, write each param in a different text_item
        two_params(Opt),send(PD1,label,'Param1:'),send(PD2,displayed,@on),send(PD2,geometry,40,115),send(PD1,geometry,40,85)
        
        % The property needs two connectives, write them in the text_item depending on wich one is selected
        -> (   two_connectives(Opt)
           ->  ( (Name == '', Name2 == '') 
              ->  get_math_def(Opt,'$1','$2',Exp1,param1), get_math_def(Opt,'$1','$2',Exp2,param2)
              ; ( (Name == '', Name2 \= '')
                 -> get_math_def(Opt,'$1',Name2,Exp1,param1), get_math_def(Opt,'$1',Name2,Exp2,param2)
                 ; ( (Name \= '', Name2 == '')
                    -> get_math_def(Opt,Name,'$2',Exp1,param1), get_math_def(Opt,Name,'$2',Exp2,param2)
                    ;  get_math_def(Opt,Name, Name2,Exp1,param1),get_math_def(Opt,Name, Name2,Exp2,param2)
                   ) 
                )
             )
           
          % Only needs one connective
          ;   (Name == '' -> get_math_def(Opt,'$',Exp1,param1),get_math_def(Opt,'$',Exp2,param2) ; get_math_def(Opt,Name,Exp1,param1), get_math_def(Opt,Name,Exp2,param2))
          ), send(PD1,append,Exp1),send(PD2,append,Exp2)
        
        % The connective only uses one parameter
        % The property uses two connectives
        ; send(PD1,label,'Definition:'),send(PD2,displayed,@off),send(PD1,geometry,40,100), 
        (   two_connectives(Opt)
           ->  ( (Name == '', Name2 == '') 
              ->  get_math_def(Opt,'$1','$2',Exp1) 
              ; ( (Name == '', Name2 \= '')
                 -> get_math_def(Opt,'$1',Name2,Exp1)
                 ; ( (Name \= '', Name2 == '')
                    -> get_math_def(Opt,Name,'$2',Exp1)
                    ;  get_math_def(Opt,Name, Name2,Exp1)
                   ) 
                )
             )
           % Only uses one
          ;   (Name == '' -> get_math_def(Opt,'$',Exp1) ; get_math_def(Opt,Name,Exp1))
          ), send(PD1,append,Exp1)
    ).
    
two_connectives(switchness).
two_connectives(distributivity).
two_connectives(adjointness).

two_params(increasing).
two_params(non_decreasing).
two_params(decreasing).
two_params(non_increasing).
two_params(distributivity).
two_params(monotony).
two_params(implication).

% Return the atom with the mathematical expression of the property

get_math_def(frontier_top,Ag1,S) :- format(atom(S),"~w(T, T) = T",[Ag1]).
get_math_def(frontier_bot,Ag1,S) :- format(atom(S),"~w(B, B) = B",[Ag1]).
get_math_def(idempotency,Ag1,S) :- format(atom(S),"~w(X, X) = X",[Ag1]).
get_math_def(commutativity,Ag1,S) :- format(atom(S),"~w(X, Y) == ~w(Y, X)",[Ag1,Ag1]).
get_math_def(aggregator,Ag1,S) :- format(atom(S),"Frontier top, frontier bot, monotony",[]).
get_math_def(associativity,Ag1,S) :- format(atom(S),"~w(~w(X, Y), Z) == ~w(X, ~w(Y, Z))",[Ag1,Ag1,Ag1,Ag1]).
get_math_def(t_norm,Ag1,S) :- format(atom(S), "~w is AND, commutative, associative, monotone and ~w(X,T)==X",[Ag1,Ag1]).
get_math_def(t_conorm,Ag1,S) :- format(atom(S), "~w is OR, commutative, associative, monotone and ~w(X,B)==X",[Ag1,Ag1]).
get_math_def(implication,Ag1,S,param1) :- format(atom(S), "If X < Y => ~w(X, Z) =< ~w(Y, Z)", [Ag1,Ag1]).
get_math_def(implication,Ag1,S,param2) :- format(atom(S), "If X < Y => ~w(Z, X) >= ~w(Z, Y)", [Ag1,Ag1]).
get_math_def(monotony,Ag1,S,param1) :- format(atom(S),"If X < Y => ~w(X, Z) =< ~w(Y, Z)",[Ag1,Ag1]).
get_math_def(monotony,Ag1,S,param2) :- format(atom(S),"If X < Y => ~w(Z, X) =< ~w(Z, Y)",[Ag1,Ag1]).
get_math_def(adjointness,Ag1,Ag2,S) :- format(atom(S), "X <= ~w(Y,Z) <==> ~w(X,Z) <= Y", [Ag2,Ag1]).
get_math_def(switchness,Ag1,Ag2,S) :- format(atom(S),"~w(~w(X, Y), Z) == ~w(X, ~w(Y, Z))",[Ag1,Ag2,Ag2,Ag1]).
get_math_def(increasing,Ag1,S,param1) :- format(atom(S),"If X < Y => ~w(X, Z) < ~w(Y, Z)",[Ag1,Ag1]).
get_math_def(increasing,Ag1,S,param2) :- format(atom(S),"If X < Y => ~w(Z, X) < ~w(Z, Y)",[Ag1,Ag1]).
get_math_def(non_decreasing,Ag1,S,param1) :- format(atom(S),"If X < Y => ~w(X, Z) =< ~w(Y, Z)",[Ag1,Ag1]).
get_math_def(non_decreasing,Ag1,S,param2) :- format(atom(S),"If X < Y => ~w(Z, X) =< ~w(Z, Y)",[Ag1,Ag1]).
get_math_def(decreasing,Ag1,S,param1) :- format(atom(S),"If X < Y => ~w(X, Z) > ~w(Y, Z)",[Ag1,Ag1]).
get_math_def(decreasing,Ag1,S,param2) :- format(atom(S),"If X < Y => ~w(Z, X) > ~w(Z, Y)",[Ag1,Ag1]).
get_math_def(non_increasing,Ag1,S,param1) :- format(atom(S),"If X < Y => ~w(X, Z) >= ~w(Y, Z)",[Ag1,Ag1]).
get_math_def(non_increasing,Ag1,S,param2) :- format(atom(S),"If X < Y => ~w(Z, X) >= ~w(Z, Y)",[Ag1,Ag1]).
get_math_def(distributivity,Ag1,Ag2,S,param1) :- format(atom(S), "~w(~w(X, Y), Z) == ~w(~w(X, Z), ~w(Y, Z))",[Ag2,Ag1,Ag1,Ag2,Ag2]).
get_math_def(distributivity,Ag1,Ag2,S,param2) :- format(atom(S), "~w(X,~w(Y, Z)) == ~w(~w(X, Y), ~w(X, Z))",[Ag2,Ag1,Ag1,Ag2,Ag2]).


fill_terms(F) :->
	get(F, member(dialog_eval), D),
	get_container_combo(D, C),
	get(D, member, connectives, Aggr),
	get(Aggr, selection, AggrSelection),
	(   AggrSelection == empty
	->  send(F, report, error, 'EMPTY list of connectives. Load a lattice and retry.')
	;   (	atomic_list_concat([_, Arity], '/', AggrSelection),
			atom_number(Arity, NumA),
			activate_dragmenu(C, 1, 6, @off),
			activate_dragmenu(C, 1, NumA, @on)
		)
	),
    get_connective(F,D,connectives,Name,_),
	% Get Eval distance button
	get_container_dist_buttons(D,Dist_Buttons),
	get(Dist_Buttons,member,eval,Dist_Eval),
	% Get Test button
	get(D,member,group1,G1),
    get(G1,member,connectives,CN),
    get(CN,member,test,T),
	
	( Name == distance
		% Unlock the Eval distance button
		-> send(Dist_Eval,active,@on)
        ;  ( (Name == infimum ; Name == supremum)
				% Lock the Test button
				-> send(T,active,@off)
				  % Unlock Test button and lock Eval button
				; send(T,active,@on),
				  send(Dist_Eval,active,@off)
			)
    ).

add_label(D, Name, Text, Style, Colour, Size) :-
	send(D, append, new(L, label(Name, Text))),
	send(L, alignment, center),
	send(L, length, 0),
	send(L, colour, Colour),
	send(L, font, font(arial, Style, Size)).

add_dragmenu(G, DefArity, Arity) :-
	forall(between(DefArity, Arity, I),
			(       get_term_name(I, StrText),
					get_var_name(I, Var),
					send(G, append, new(M, dragmenu(StrText))),
					send(M, alignment, center),
					send(M, displayed, @on),
					send(M, append, Var)
			)).
fill_dragmenu(G, DefArity, Arity, ListAdd) :-
	forall(between(DefArity, Arity, I),
			(       get_term_name(I, StrText),
					get_var_name(I, Var),
					get(G, member(StrText), DM),
					send(DM, clear),
					send(DM, append, Var),
					(   ListAdd == []
					->  true
					;   send_list(DM, add_member, ListAdd)
					)
			)).
			
get_term_name(I, Text):-
    atom_concat(term_, I, Text).
get_var_name(I, Text):-
    atom_concat(x, I, Text).
	
activate_dragmenu(G, DefArity, Arity, OnOff) :-
	forall(between(DefArity, Arity, I),
			(       get_term_name(I, StrText),
					get(G, member(StrText), DM),
					send(DM, active, OnOff)
			)).

fill_dialog(D) :-
	add_label(D, reporter, '', bold, red, 12).

new(F) :->
	ok_changes(F),
	send(F, clearspace),
	lat_graph:assert(lat_graph:leq(X, X)),
	lat_graph:assert(':-'(lat_graph:leq(X, Y), (lat_graph:arc(X, Z), lat_graph:leq(Z, Y)))),
    lat_graph:assert(':-'(lat_graph:distance(X, Y, Z), (lat_graph:level(X, L1), lat_graph:level(Y, L2), Z is abs(L1-L2)))),
	send(F, set_new_state),
	send(F, set_filename, ''),
	send(F, set_selected_node).

load(F) :->
	"Ask for a file name"::
	ok_changes(F),
	get(F, member, view, V),
	get(@finder, file, @on, '.lat', FileName),
    
    % The lattice is being loaded for the first time
    send(F,set_load_state),
    
	send(F, clearspace),
	(	wipe_module(lat_graph),
		lat_graph:consult(FileName)
	->	(	validate_lattice(lat_graph)
		->	send(V, load, FileName),
			send(F, report, status, 'File %s read', FileName),
			send(F, label, string('LatticeMaker - %s', FileName)),		
			% "Show file content"
			send(F, set_filename, FileName),
			send(F, set_nonew_state),
			send(F, normalize),
          
            % The lattice is already loaded
            send(F,set_noload_state)
		;	send(F, report, error, 'File %s hasn\'t a lattice format', FileName)
		)
	;	send(F, report, error, 'File %s has errors', FileName)
	).

save(F) :->
	"Write to file"::
	get_new_state(F)
	->	send(F, saveAs)
	;	send(F, lattice_noempty),
		ok_normalize(F),
		(	get_filename(F, FileName)
		->	true
		;	get(@finder, file, save, '.lat', FileName)
		),
		add_generated_to_file(F, FileName, Path),

		get(F, member, view, V),
		send(V, save, Path),

		send(F, report, status, 'Saved in %s', Path),
		send(F, label, string('LatticeMaker - %s', Path)),		
		send(F, set_nomodified_state),
		send(F, set_filename, Path).

add_generated_to_file(F, FileName, Path) :-
	file_base_name(FileName, JustName),
	file_directory_name(FileName, DirName),
	(	atom_concat('generated_', _, JustName)
	->  Path = FileName
	;   atom_concat('generated_', JustName, NewJustName),
		directory_file_path(DirName, NewJustName, Path)
	),
	(   exists_file(Path)
	->  get(F, msgbox, '"generated_" file already exists. Rewrite it?', RT),
		RT \= @nil
	;   true
	).

saveAs(F) :->
	"Write to file"::
	writeln('entro en saveas'),
	send(F, lattice_noempty),
	ok_normalize(F),
	get(@finder, file, save, '.lat', FileName),
	add_generated_to_file(F, FileName, Path),
	
	get(F, member, view, V),
	send(V, save, Path),

	send(F, report, status, 'Saved in %s', Path),
	send(F, label, string('LatticeMaker - %s', Path)),		
	send(F, set_nomodified_state),
	send(F, set_filename, Path).

image(F):->
	get(F, member, picture, P),
	send(F, graph_noempty, P),

	new(ND, dialog('Image file format')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(L, label(infor, 'Select one of this image formats'))),
	send(L, alignment, center),
	send(L, length, 0),
	send(L, colour, blue),
	send(L, font, font(arial, normal, 12)),
	new(CB, menu(image, cycle)),
	send(ND, append, CB),
	send_list(CB, append, [xpm,jpeg,gif]),
	send(CB, alignment, center),
	send(ND, append, button(ok, message(ND, return, CB?selection))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),
	get(ND, confirm_centered,
						F?area?center, ImgFormat),
	send(ND, destroy),
	ImgFormat \== @nil,

	get_name_for_file(F, ImgFormat, Path),
 	new(File, file(Path)),
	send(File, open, write),

	picture_to_image(P, Image),

	send(Image, save, File, ImgFormat),
	send(File, close),
	send(File, done),
	send(F, report, status, 'Saved Image in %s', Path),
	send(F, layout).

picture_to_image(P, I) :-
	get(P, bounding_box, area(X, Y, W, H)),
	new(I, image(@nil, W, H, pixmap)),
	new(M1, point(-X, -Y)),
	send(P?graphicals, for_all, and(message(@arg1, relative_move, M1),
									message(I, draw_in, @arg1))).

export_xml(F) :->
	"Export to XML file"::
	send(F, lattice_noempty),
	ok_normalize(F),
	get_name_for_file(F, xml, Path),

	new(ND, dialog('Lattice XML document')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	add_label(ND, label1, 'This is the XML document for current lattice', bold, dark_blue, 12),
	add_label(ND, label2, 'Do you want to save it?', bold, dark_blue, 12),
	send(ND, append, new(NV, view(text))),

	send(ND, append, button(ok, and(message(NV, save, Path),
									message(ND, return, ok)))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),

	new(Buffer, text_buffer),
	generate_XML(F, Buffer),

	send(NV, text_buffer, Buffer),
	send(NV, editable, @off),

	get(ND, confirm_centered, F?area?center, Return),
	send(ND, destroy),
	Return \== @nil.

print(F) :->
	send(F, lattice_noempty),
	ok_normalize(F),
	new(D, dialog('Print options')),
	send(D, transient_for, F),
	send(D, modal, transient),
	send(D, pen, 0),

	send(D, append, new(T, menu(options, marked))),
	send_list(T, append, [windows_printer, postscript]),
	send(T, layout, horizontal),
	send(D, append, button(ok, message(D, return, T?selection))),
	send(D, append, button(cancel, message(D, return, @nil))),
	send(D, default_button, ok),
	get(D, confirm_centered,
						F?area?center, Return),
	send(D, destroy),
	Return \== @nil,
	Return == windows_printer
	-> 	print_windows(F) 
	;   postscript(F).

clear_graph(F) :->
	"Clear the diagram"::
	get(F, member, picture, P),
	send(F, graph_noempty, P),
	ok_delete(F),
	send(P, clear),
	wipe_module(lat_graph).

find(F) :->
	"Find a node"::
	get(F, member, picture, P),
	send(F, graph_noempty, P),
	new(ND, dialog('Enter Node Name')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(TI, text_item(name))),
	send(ND, append, button(ok, message(ND, return, TI?selection))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, ok),

	get(ND, confirm_centered,
						F?area?center, Return),
	send(ND, destroy),
	Return \== @nil,
	get(Return, strip, Name),

	(	get_node_member(Name, NName)
	->  get(P, member(NName), Node),
		get(Node, position, point(NodeX, NodeY)),
		get(P, visible, area(_, _, W, H)),
		NewX = NodeX - W/2,
		NewY = NodeY - H/2,
		send(P, scroll_to, point(NewX, NewY)),
		send(F, report, status, 'Node located')
	;	send(F, report, status, 'Node not found')
	).

complete(F) :->
	lat_graph:top(T),
	get(F, node, T, TNode),
	lat_graph:bot(B),
	get(F, node, B, BNode),

	get_no_connected(L),
	connect_list_top(F, TNode, L),
	connect_list_bot(F, BNode, L),

	get_no_connected_top(ListT),
	connect_list_top(F, TNode, ListT),

	get_no_connected_bot(ListB),
	connect_list_bot(F, BNode, ListB).

connect_list_top(_, _, []).
connect_list_top(F, Top, [H|L]):-
	get(F, node, H, HNode),
	make_connect(HNode, Top, program),
	connect_list_top(F, Top, L).

connect_list_bot(_, _, []).
connect_list_bot(F, Bottom, [H|L]):-
	get(F, node, H, HNode),
	make_connect(Bottom, HNode, program),
	connect_list_bot(F, Bottom, L).

normalize(F) :->
	"Normalize the graph"::
	send(F, draw_graph),
	send(F, lattice),
	fill_from_lattice(F),
	send(F, set_edit_mode_off),
    
    get(F, member(dialog_eval), D),
	get(D, member, view, V),
    
	send(V, clear),
	current_output(Old),
	pce_open(V, write, Fd),
    
	set_output(Fd),
    lat:members(M),
	
	check_supr_inf(M,lat),
    
    
    close(Fd),
	set_output(Old),
    send(V, editable, @off),
    
    % The lattice is already normalized
	send(F, report, status, 'Graph has been normalized').
    
check_supr_inf([_],_).
check_supr_inf([H|T],Mod) :- call(supremum_and_infimum,H,T,Mod),check_supr_inf(T,Mod).

draw_graph(F) :->
	(	prepare_for_drawing,
		send(F, layout)
		;	lat_graph:members(L),
			layout_no_connected(F, L)
		).

layout(F) :->
	get(F, member, picture, P),
	send(P, clear),
	send(P, scroll_to, point(0, 0)),
	(	lat_graph:top(_)
	->  true
	;   get_top(T),
		assert_top(T)
	),
	(   lat_graph:bot(_)
	->  true
	;   get_bot(B),
		assert_bot(B)
	),
	get_no_connected(LNC),
	layout_no_connected(F, LNC),

	lat_graph:bot(B),
	LBottom = [(B, 1)],
	layering(LBottom, L, MaxLayer, MaxNodes),
	term_to_atom(MaxNodes, AtomMN),
	atom_number(AtomMN, MN),
	send(F, max_nodes_layer, MN),

	lat_graph:top(Top),
	get_node_layer(L, Top, Layer),
	term_to_atom(Layer, AtomLy),
	atom_number(AtomLy, NLy),
	(	NLy == 0
	->  NLayer is MaxLayer + 2
	;   NLayer is NLy - 1
	),
	add_semi_connected(L, NLayer, LL),

	draw_list(F, LL),
	darcs(F).

assert_top(Node) :-
	asserta(lat_graph:top(Node)).
assert_bot(Node) :-
	asserta(lat_graph:bot(Node)).

lattice_to_graph(F) :->
	get(F, member, view, V),
	get(V, text_buffer, B),
	get(B, modified, M),

	tmp_file_stream(text, File, Stream),
	close(Stream),
	send(B, save, File),
	(	(	copy_module(lat_graph, lat_redrawn); true	),
		wipe_module(lat_graph),
		lat_graph:consult(File)
	->	(	validate_lattice(lat_graph)
		->	send(F, draw_graph)
		;	send(F, report, error, 'Lattice in view hasn\'t a correct lattice format')
		)
	;	send(F, report, error, 'Lattice in view has errors')
	),
    
	send(B, modified, M),
	send(F, redrawn, @on).

undo_send_graph(F) :->
	get(F, redrawn, @on),
	validate_lattice(lat_redrawn),
	copy_module(lat_redrawn, lat_graph),
	send(F, draw_graph).

validate_lattice(M) :-
	M:members(L),
	no_duplicates(L).
	
no_duplicates([]).
no_duplicates([H|T]) :-
	\+ select(H, T, _),
	no_duplicates(T).
	
lattice(F) :->
	%% "Temporaly in buffer"
	tmp_file_stream(text, File, Stream),
	close(Stream),
	new(B, text_buffer),
	send(F, compose_buffer, B),
	send(B, save, File),
	wipe_module(lat),
	(	lat:consult(File)
	;	send(F, report, error, 'Consult result: lattice in use has predicate errors')
	),
    get(F, member, view, V),
	send(V, clear),
	send(V, text_buffer, B),
	send(F,lattice_to_graph).

compose_buffer(F, B) :->
	%% "What I have in lat_graph module"
	lat_graph:members(ListofNodes),
	with_output_to(codes(Codes), write(ListofNodes)),
	string_to_list(String, Codes),
	write_in_buffer(B, 'members(%s).\n', String),

	setof(Node, lat_graph:member(Node), L1),
	maplist(format_and_write(B), L1),

	(	lat_graph:top(Top); get_top(Top)	),
	write_in_buffer(B, 'top(%s).\n', Top),

	(	lat_graph:bot(Bottom); get_bot(Bottom)	),
	write_in_buffer(B, 'bot(%s).\n', Bottom),

	setof((From, To), lat_graph:arc(From, To), L4),
	maplist(write_arc_in_buffer(B), L4),

	%% "FROM prepare_for_drawing"
    ( not(get_load_state(F))
    -> write_in_buffer(B, 'leq(X, X).\n'),write_in_buffer(B, 'leq(X, Y):- arc(X, Z), leq(Z, Y).\n')
    ; true
    ),

    % Get layer list
    lat_graph:bot(Bot),
    LBottom = [(Bot, 1)],
    layering(LBottom, L, MaxLayer, _),
    % Write levels and distance in buffer
    maplist(write_level_in_buffer(B,L,MaxLayer), L1),
    
    % If the lattice is being normalized or the distance does not exist, then write the default distance
    ( ( not((compile_predicates([lat_graph:distance/3]),current_predicate(lat_graph:distance/3))) ; not(get_load_state(F)) )
    -> retractall(distance(_,_,_)),write_in_buffer(B,'distance(X,Y,Z):-level(X,L1), level(Y,L2), Z is abs(L1 - L2).\n')
    ; true
    ),
    
	%% "What I have in view"
	get(F, member, view, V),
	pce_open(V, read, File),
		repeat,
	(   read_term(File, Term, [variable_names(LVars)]),
		Term \== end_of_file
	->  with_output_to(atom(Atom), 
				write_term(Term, [variable_names(LVars), quoted(true)])),
		analize_predicate(Atom, (Name, Arity, _, _)),
		(	vary_pred(Name,Arity,F,B,Atom),filtered(Name, Arity)
		->  fail
		;   write_in_buffer(B, '%s.\n', Atom)
		),
		fail
	;	!
	),
	close(File).

filtered(members, 1).
filtered(member, 1).
filtered(top, 1).
filtered(bot, 1).
filtered(arc, 2).
filtered(level,2).
filtered(leq, 2).
filtered(distance,3). 

% Variable predicates, if the lattice is being normalized then write the default predicate
vary_pred(distance,3,F,B,Atom) :- get_load_state(F),write_in_buffer(B, '%s.\n', Atom).
vary_pred(leq,2,F,B,Atom) :- get_load_state(F),write_in_buffer(B, '%s.\n', Atom).
vary_pred(_,_,_,_,_).

analize_predicate(Atom, (Name, Arity, Head, Body)):-
	atomic_list_concat([Head, Body|_], ':-', Atom), !,
	term_to_atom(TermH, Head),
 	functor(TermH, Name, Arity).
	
analize_predicate(Atom, (Name, Arity, Head, Body)):-
	term_to_atom(TermH, Atom),
 	functor(TermH, Name, Arity),
	Head = Atom,
	Body = true.

format_and_write(B, Name):-
	% To get float point '.'
	with_output_to(codes(Codes), write(Name)),
	string_codes(String, Codes),
	write_in_buffer(B, 'member(%s).\n', String).
	
write_in_buffer(Buffer, Text, Name):-
	send(Buffer, append,  string(Text, Name)).
write_in_buffer_index(Buffer, Text, Index) :-
    send(Buffer, insert, Index, Text).
write_in_buffer(Buffer, Text):-
	send(Buffer, append,  Text).
write_arc_in_buffer(Buffer, (N1,N2)):-
	atomic_list_concat(['arc(', N1, ', ', N2, ').\n'], Text),
	send(Buffer, append, Text).
write_level_in_buffer(Buffer,L,MaxLayer,Node) :-
    % Get node's layer number
    get_node_layer(L, Node, Layer),
	term_to_atom(Layer, AtomLy),
	atom_number(AtomLy, NLy),
	(	NLy == 0
	->  NLayer is MaxLayer + 2
	;   NLayer is NLy - 1
	),
    atomic_list_concat(['level(', Node, ', ', NLayer,').\n'], Text),
    % Assert the new predicate
    assertz(lat_graph:level(Node,NLayer)),
	send(Buffer, append, Text).


replace_in_lattice(F, Old, New):-
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),
	atom_length(Old, Chars),
	Int = 0,
	repeat,
	(	get(B, find, Int, Old, 1, start, @on, @on, Index),
		Index < Length
	->	send(B, delete, Index, Chars),
		send(B, insert, Index, New),
		Int = Index + 1,
		send(F, report, 'Truth degree replaced in lattice'),
		fail
	;	!
	).

get_top(T) :-
	lat_graph:arc(_,T), \+ lat_graph:arc(T,_).
get_bot(B) :-
	lat_graph:arc(B,_), \+ lat_graph:arc(_,B).

bye(F) :->
	ok_changes(F),
	clear_memory,
	send(F, destroy).

clearspace(F) :->
	"Clear the work space"::
	clear_memory,
	get(F, member, picture, P),
	get(F, member, view, V),
	send(P, clear),
	send(V, clear),

	get(F, member(dialog_eval), D),
	get(D, member, connectives, Aggr),
	send(Aggr, clear),
	send_list(Aggr, append, [empty]),

	get_container_combo(D, C),
	forall(between(1, 6, I),
		(	get_term_name(I, StrText),
			get(C, member(StrText), DM),
			get_var_name(I, Var),
			send(DM, clear),
			send_list(DM, append, [Var])
		)),
	get(D, member, view, DV),
	send(DV, clear),
	wipe_module(lat_redrawn),
	send(F, redrawn, @off),
	send(F, max_nodes_layer, 0),
	send(F, report, status, '').

clear_memory :-
	wipe_module(lat_graph).

newnode(F, Position) :->
	get_selected_node(F),
	get(F, nodePos, noname, Position, NewNode),
	send(NewNode, rename).

nodePos(F, Name, Position, Node:lattice_node) :<-
	"In user mode, get (create) node with specified name"::
	get(F, member, picture, P),
	(	get(P, member, Name, Node)
	->  true
	;  	send(P, display, new(Node, lattice_node(Name, green)), Position)
	).

node(F, Name, Layer, Color, NNodes, Indx, Node:lattice_node) :<-
	"In layering mode, get (create) node with specified name"::
	get(F, member, picture, P),
	get(F, max_nodes_layer, NMax),
	(	get(P, member, Name, Node)
	->  true
	;	get(P, visible, area(_, _, W, H)),
		W1 is (NMax + 1) * 80,
		(	W1 > W, W2 is W1; W2 is W	),
		MX is W2 / (NNodes + 1),
				
		with_output_to(codes(Codes), write(Name)),
		length(Codes, Length),
		% 4 characters inside the circle
		(	Length > 4, MoveX is 6 * (Length - 4) // 2; MoveX is 0	),

		NX is MX * Indx - MoveX,
		NY is H - Layer * 60,
		send(P, display, new(Node, lattice_node(Name, Color)), point(NX, NY))
	).

fill_from_lattice(F) :-
	get(F, member(dialog_eval), Oper),
	
    create_predicates(Oper,connectives),
    create_predicates(Oper,second),
	
	lat_graph:members(L),
	maplist(atom_string, L, L1),
	get_container_combo(Oper, Container),
	fill_dragmenu(Container, 1, 6, L1).

% Fill the Aggregator Combobox given
create_predicates(Oper,Combo_name) :- 
    get(Oper, member, Combo_name, Aggr),
	send(Aggr, clear),
    % creates a list of prolog_predicate
	findall(NewPred, operator(_, NewPred), ListPP),
	(	ListPP == []
	->  send_list(Aggr, append, [empty])
	;   send_list(Aggr, append, [noselection]),
		maplist(fill_combo_aggr(Aggr), ListPP),
		send_list(Aggr, append, ['supremum/3','infimum/3']),
		send(Aggr, sort),
		send(Aggr, selection, noselection)
	).
    
get_name_for_file(F, Extension, Path) :-	
		atomic_concat('.', Extension, Ext),
    (	get_filename(F, FileName)
	->	file_base_name(FileName, JustName),
		file_directory_name(FileName, DirName),
		file_name_extension(BaseName, _, JustName),
		file_name_extension(BaseName, Ext, OutName),
		directory_file_path(DirName, OutName, _)
	;	Path = @nil
	),
	get(@finder, file, @off, Extension, DirName, BaseName, Path),
	(   exists_file(Path)
	->  get(F, msgbox, string('%s file already exists. Rewrite it?', Ext), RT),
		RT \= @nil
	;   true
	).

msgbox(F, Msg, Ret) :<-
	new(ND, dialog('Lattice control')),
	send(ND, transient_for, F),
	send(ND, modal, transient),
	send(ND, pen, 0),

	send(ND, append, new(L, label(alert, Msg))),
	send(L, alignment, center),
	send(ND, append, button(ok, message(ND, return, ok))),
	send(ND, append, button(cancel, message(ND, return, @nil))),
	send(ND, default_button, cancel),

	get(ND, confirm_centered,
						F?area?center, Ret),
	send(ND, destroy).

operator(P, P/Arity) :-
	current_predicate(lat:P/Arity),
	(	atom_concat('and_', _, P)
	;   atom_concat('or_', _, P)
	;   atom_concat('agr_', _, P)
    ;   atom_concat('distance',_,P)
	).

fill_combo_aggr(CB, N/A) :-
	change_item(N, NewN),
	atom_concat(NewN, '/', NT),
	atom_concat(NT, A, NewOP),
	send(CB, append, NewOP).
	
change_item(Oper, NewOper) :-
	var(NewOper),
	atom_concat('and_', SN, Oper),
	atom_concat('&_', SN, NewOper).
change_item(Oper, NewOper) :-
	var(NewOper),
	atom_concat('or_', SN, Oper),
	atom_concat('|_', SN, NewOper).
change_item(Oper, NewOper) :-
	var(NewOper),
	atom_concat('agr_', SN, Oper),
	atom_concat('@_', SN, NewOper).
change_item(Oper, NewOper) :-
	var(NewOper),
	atom_concat('distance', SN, Oper),
	atom_concat('distance', SN, NewOper).
change_item(Oper, NewOper) :-
	var(NewOper),
	atom_concat('infimum', SN, Oper),
	atom_concat('infimum', SN, NewOper).
change_item(Oper, NewOper) :-
	var(NewOper),
	atom_concat('supremum', SN, Oper),
	atom_concat('supremum', SN, NewOper).
	
change_item(Oper, NewOper) :-
	var(Oper),
	atom_concat('&_', SN, NewOper),
	atom_concat('and_', SN, Oper).
change_item(Oper, NewOper) :-
	var(Oper),
	atom_concat('|_', SN, NewOper),
	atom_concat('or_', SN, Oper).
change_item(Oper, NewOper) :-
	var(Oper),
	atom_concat('@_', SN, NewOper),
	atom_concat('agr_', SN, Oper).
change_item(Oper, NewOper) :-
	var(Oper),
	atom_concat('distance', SN, NewOper),
	atom_concat('distance', SN, Oper).
change_item(Oper, NewOper) :-
	var(Oper),
	atom_concat('infimum', SN, NewOper),
	atom_concat('infimum', SN, Oper).
change_item(Oper, NewOper) :-
	var(Oper),
	atom_concat('supremum', SN, NewOper),
	atom_concat('supremum', SN, Oper).

add_connective(F) :->
	send(F, get_edit_mode)
	->	new(ND, dialog('New connective')),
		send(ND, transient_for, F),
		send(ND, modal, transient),
		send(ND, pen, 0),

		add_label(ND, infor, 'Select the connective to add to the lattice',
							normal, blue, 12),
		send(ND, append, new(CBO, menu(connectives, cycle))),
		send_list(CBO, append, ['&_godel', '|_godel', '&_luka', '|_luka']),
		send_list(CBO, append, ['&_prod', '|_prod', '@_aver']),
		send(CBO, alignment, center),
		send(ND, append, button(add, message(ND, return, CBO?selection))),
		send(ND, append, button(cancel, message(ND, return, @nil))),
		send(ND, default_button, add),

		get(ND, confirm_centered,
							F?area?center, Return),
		send(ND, destroy),
		Return \== @nil,
		get(Return, strip, Name),
		change_item(Name, NA),
		get(F, member, view, V),
		append_in_view(V, NA),
		send(F, set_modified_state),
		send(F, report, status, '')
	;	send(F, report, status, 'Editor is in read-only mode').

clear_editor(F) :->
	send(F, get_edit_mode)
	->	ok_delete(F),
		get(F, member, view, V),
		send(V, clear),
		wipe_module(lat),
		send(F, report, status, '')
	;	send(F, report, status, 'Editor in read-only mode').

append_in_view(V, Aggr) :-
	Aggr == 'and_godel',
	send(V, append, 'and_godel(X,Y,Z) :- pri_min(X,Y,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == and_luka,
	send(V, append, 'and_luka(X,Y,Z) :- pri_add(X,Y,U1), pri_sub(U1,1,U2), pri_max(U2,0,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == 'and_prod',
	send(V, append, 'and_prod(X,Y,Z) :- pri_prod(X,Y,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == or_godel,
	send(V, append, 'or_godel(X,Y,Z) :- pri_max(X,Y,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == or_luka,
	send(V, append, 'or_luka(X,Y,Z) :- pri_add(X,Y,U1), pri_min(U1,1,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == or_prod,
	send(V, append, 'or_prod(X,Y,Z) :- pri_prod(X,Y,U1), pri_add(X,Y,U2), pri_sub(U2,U1,Z).\n').
append_in_view(V, Aggr) :-
	Aggr == agr_aver,
	send(V, append, 'agr_aver(X,Y,Z) :- pri_add(X,Y,U1), pri_div(U1,2,Z).\n').

% Get the connective selected in the combobox
get_connective(F,D,Combo_name,Name,NumA) :-
    get(D, member, Combo_name, Aggr),
    get(Aggr, selection, A),
    (   (A == noselection ; A == empty)
    ->  send(F, report, error, 'Please, select a connective.'),Name = ''
    ;   change_item(NA, A),
        atomic_list_concat([Name, Arity], '/', NA),
        atom_number(Arity, NumA)
    ).

eval_selected_connective(F) :->
	get(F, member(dialog_eval), D),
	get(D, member, view, V),
	get(D, member, box, Box1),
	send(Box1,fill_pattern,@nil),

	current_output(Old),
	pce_open(V, write, Fd),
	set_output(Fd),

    get_connective(F,D,connectives,Name,NumA),not(empty_aggr(Name)),
		(   ( Name == infimum ; Name == supremum)
			->  ( get_combo_term(D,1,E1),
				  get_combo_term(D,2,E2),
				  get_combo_term(D,3,R),
				  call(Name,E1,E2,R,lat)
				-> writef('The %w between %w and %w is %w',[Name,E1,E2,R]),send(Box1, fill_pattern, green)
				;  write(false),send(Box1,fill_pattern,red)
				)
			; (append_param(D, NumA, [], LParams),call_connective(Name, LParams, L)
			  ->  maplist(show_result(LParams), L),send(Box1, fill_pattern, green)
			  ;   write(false),send(Box1,fill_pattern,red)
			  )
		),
		send(F, report, status, '%s connective evaluated.', Name),
	close(Fd),
	set_output(Old),
    send(V, editable, @off).

append_param(D, I, L, NewL):-
	I < 1
	->  NewL = L
	;   get_combo_term(D,I,S),
		append([S], L, L1),
		J is I - 1,
		append_param(D, J, L1, NewL).
        
test_selected_connective(F) :->
	get(F, member(dialog_eval), D),
	get(F, slot, property, Prop),
	get(D, member, view, V),
	get(D, member, box, Box1),
	send(Box1,fill_pattern,@nil),
	
	send(V, clear),
	current_output(Old),
	pce_open(V, write, Fd),
    
	set_output(Fd),
    
    not((empty_prop(Prop),send(F, report, error, 'Please, select a valid property.'))),
    
    get_connective(F,D,connectives,Name,_),
    
    not(empty_aggr(Name)),

    (   two_connectives(Prop)
        -> ( two_params(Prop) 
			  -> get_connective(F,D,second,Name2,_),not(empty_aggr(Name2)),call(Prop,Name,Name2,Box1)
			; ( get_connective(F,D,second,Name2,_),not(empty_aggr(Name2)),call(Prop,Name,Name2)
				-> send(Box1, fill_pattern, green)
				; send(Box1, fill_pattern, red)
			  )
			)
        ; (
			two_params(Prop),Prop \= monotony, Prop \= implication 
			->	call(Prop,Name,Box1)
			; (call(Prop,Name)
				-> send(Box1,fill_pattern,green)
				; send(Box1,fill_pattern,red)
			  )
		  )
    ),
	close(Fd),
	set_output(Old),
    send(V, editable, @off),
	send(F, report, status, '%s connective tested.', Name).

empty_aggr('').
empty_prop(@nil).                        
    
eval_distance(F) :-> 
    get(F, member(dialog_eval), D),
	get(D, member, view, V),
	get(D, member, box, Box1),
	send(Box1,fill_pattern,@nil),
    
    send(V, clear),
	current_output(Old),
	pce_open(V, write, Fd),
    set_output(Fd),
    
    get_combo_term(D,1,E1),
    get_combo_term(D,2,E2),
    
    lat_graph:distance(E1,E2,Z),round(Z,R),
    writef('The distance between %w and %w is %w',[E1,E2,R]),
    
    close(Fd),
	set_output(Old),
    send(V, editable, @off).
    

check_distance(F) :->
    get(F, member(dialog_eval), D),
	get(D, member, view, V),
	get(D, member, box, Box1),
	send(Box1,fill_pattern,@nil),
    
	send(V, clear),
	current_output(Old),
	pce_open(V, write, Fd),
    
	set_output(Fd),
    
    writeln('Checking distance:\n'),
    
    ( call(valid_distance,lat_graph:distance)
		-> send(Box1,fill_pattern,green)
		; send(Box1,fill_pattern,red)
	),
	
    
    close(Fd),
	set_output(Old),
    send(V, editable, @off).

    
get_combo_term(D,I,S) :-
        get_term_name(I, StrTerm),
		get_container_combo(D, C),
		get(C, member, StrTerm, T),
        get_var_name(I,X),
		get_selection(T, S, X).
    
get_node_member(Name, NName):-
	lat_graph:members(L), 
	(	member(Name, L), NName = Name	
	; 	atom_number(Name, NName), member(NName, L)
	;	atom_codes(Name, Codes),
		select(44, Codes, 46, NCodes),
		number_codes(NName, NCodes),
		member(NName, L)
	),!.
		
wipe_module(M) :-
	forall(current_predicate(M:P/A), retract_abolish(M:P/A)).

copy_module(M, N) :-
	wipe_module(N),
	
	M:members(ListofNodes),
	N:assertz(N:members(ListofNodes)),
	forall(M:member(Node), N:assertz(N:member(Node))),
	forall(M:top(Node), N:assertz(N:top(Node))),
	forall(M:bot(Node), N:assertz(N:bot(Node))),
	forall(M:arc(From, To), N:assertz(N:arc(From, To))),
	N:assertz(N:leq(X, X)),
	N:assertz(':-'(N:leq(X, Y), (arc(X, Z), leq(Z, Y)))),
    forall(M:level(X, L), N:assertz(N:level(X, L))),
    N:assertz(':-'(N:distance(X, Y, Z), (level(X, L1), level(Y, L2), Z is abs(L1-L2)))).
    
retract_abolish(Module:Name/Arity) :-
	(   functor(Head, Name, Arity),
		predicate_property(Module:Head, dynamic)
	)
	->	retractall(Module:Head)
	;   abolish(Module:Name/Arity).

find_in(F) :->
	get(F, member, view, V),
	send(editor_find_dialog(V), open).

ok_normalize(F) :-
	get_modified_state(F)
	->  get(F, msgbox, 'Must normalize the lattice. Normalize now?', RT),
		RT \= @nil,
		send(F, normalize)
	;   true.

ok_changes(F) :-
	get_modified_state(F)
	->  get(F, msgbox, 'Current graph has been modified. Discard changes?', RT),
		RT \= @nil
	;   true.

ok_delete(F) :-
	get(F, msgbox, 'Content will be definitely lost. Delete anyway?', RT),
	RT \= @nil.

ok_node_mode(F) :-
	get_selected_node(F)
	->  true
	;   get(F, msgbox, 'Activate NODE mode?', RT),
		RT \= @nil,
		send(F, set_selected_node).

ok_arrow_mode(F) :-
	get_selected_arrow(F)
	->	true
	;	get(F, msgbox, 'Activate ARROW mode?', RT),
		RT \= @nil,
		send(F, set_selected_arrow).

ok_nonew(F) :-
	get(F, msgbox, 'You need to load a valid lattice', RT),
	RT \= @nil,
	false.
		
lattice_noempty(_) :->
	setof(L, lat_graph:members(L), _).
	
graph_noempty(F, P) :->
	send(P?graphicals, empty)
	->  send(F, report, error, 'No graph loaded'), fail
	;       true.

% "Select Node or Arrow option"
set_selected_node(F) :->
	send(F, slot, drawnode, @on),
	send(F, slot, drawarrow, @off),
	retractall(lat_graph:fromNode(_)).
set_selected_arrow(F) :->
	send(F, slot, drawnode, @off),
	send(F, slot, drawarrow, @on).
set_selected_nothing(F) :->
	send(F, slot, drawnode, @off),
	send(F, slot, drawarrow, @off).
get_selected_node(F) :-
	get(F, drawnode, @on).
get_selected_arrow(F) :-
	get(F, drawarrow, @on).
set_initial_state(F) :->
	send(F, set_new_state),
	send(F, slot, modified, @off),
	send(F, slot, editable, @off).
set_new_state(F) :->
	send(F, slot, new, @on),
	send(F, slot, modified, @off),
	send(F, slot, editable, @off).
set_nonew_state(F) :->
	send(F, slot, new, @off),
	send(F, slot, modified, @off),
	send(F, slot, editable, @off).
set_modified_state(F) :->
	send(F, slot, modified, @on).
set_nomodified_state(F) :->
	send(F, slot, modified, @off).
set_load_state(F) :->
    send(F, slot, loaded_lattice, @on).
set_noload_state(F) :->
    send(F, slot, loaded_lattice, @off).
get_load_state(F) :-
    get(F, loaded_lattice, @on).
get_modified_state(F) :-
	get(F, modified, @on); get_modified_text_lattice(F).
get_new_state(F) :-
	get(F, new, @on).
get_modified_text_lattice(F) :-
	get(F, member, view, V),
	get(V, text_buffer, B),
	get(B, modified, @on).

restore_view_leq(F) :-> 
        get(F, member, view, V),
        get(V, text_buffer, B),
        ( delete_all_pred(F,'leq',Index) ;  get(B,length,Length),Index = Length),
        write_in_buffer_index(B,'leq(X, Y):- arc(X, Z), leq(Z, Y).\n',Index),write_in_buffer_index(B, 'leq(X, X).\n' ,Index),
		send(F,lattice_to_graph).    

restore_view_distance(F) :-> 
        get(F, member, view, V),
        get(V, text_buffer, B),
        ( delete_all_pred(F,'distance',Index) ; get(B,length,Length),Index = Length),
        write_in_buffer_index(B, 'distance(X,Y,Z):-level(X,L1), level(Y,L2), Z is abs(L1 - L2).\n' ,Index),
		send(F,lattice_to_graph).

% Delete all the occurrences of the predicate in the view and return the index for insert the new predicate
delete_all_pred(F,Pred,Index) :-
    get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
    Init = 0,
    find_and_delete_all(B,Pred,Init,Index).

    
find_and_delete_all(B,Pred,Init,Index) :-
    % Search the first occurence
    get(B, find, Init, Pred, 1, start, @on, @on, Index),
    get(B,line_number,Index,Line),
    
    % Get the line of the previous index
    Prev is (Index-1),get(B,line_number,Prev,Line2),
    
    % If the previous and the current index are in the same line, it forms part of another predicate
    (Line =\= Line2
    
    % Delete the occurence from the current index to the end of the line and continue searching from the current index
    -> get(B, length, Length),findall(N,(between(Index,Length,N),get(B,line_number,N,L2),Line==L2),S), length(S,Count),send(B,delete,Index,Count), not(find_and_delete_all(B,Pred,Index,Index2))
    
    % Search the next occurence from the Index 
    ;  (Init2 is (Index+1),not(find_and_delete_all(B,Pred,Init2,Index2)))).

toggle_edit_mode(F) :->
	get(F, member, view, V),
	(	send(F, get_edit_mode)
	->	send(F, slot, editable, @off),
		send(V, editable, @off),
		send(F, report, status, 'Editor set in read-only mode')
	;	send(F, slot, editable, @on),
		send(V, editable, @on),
		send(F, report, status, 'Editor set in read-write mode')
	).
get_edit_mode(F) :->
	get(F, slot, editable, @on).
set_edit_mode_on(F) :->
	get(F, member, view, V),
	send(V, editable, @on),
	send(F, slot, editable, @on).
set_edit_mode_off(F) :->
	get(F, member, view, V),
	send(V, editable, @off),
	send(F, slot, editable, @off).
ok_edit_mode(F) :-
	send(F, get_edit_mode)
	->  true
	;	get(F, msgbox, 'Editor must be in write mode to rename truth degree references. Activate now?', RT),
		RT \= @nil,
		send(F, set_edit_mode_on).

set_filename(F, Name) :->
	send(F, slot, filename, Name).
get_filename(F, Name) :-
	get(F, filename, Name).

get_container_combo(D, G11):-
	get(D, member, group1, G1),
	get(G1, member, group11, G11).
get_container_optgroup(D, G12):-
	get(D, member, group1, G1),
	get(G1, member, group12, G12).
get_container_dist_buttons(D,G15) :-
    get(D,member,group1,G1),
    get(G1,member,distances,G15).
    
% unlink(GV) :->
	% free(@gvl),
	% free(@gvs).
	
:- pce_end_class.

sim_maker :-
	new(GV, sim_maker).
	% send(GV, open_centered).

if(X,Y,Z) :- (X->Y;Z).

try(X,F):-(catch(X,_,F),!;F).
	
:- pce_begin_class(sim_maker, lattice_maker).

% unlink(GV) :->
	% free(@gvl),
	% free(@gvs).

initialise(GV) :->
	"Create sim_maker"::
	% send(GV, send_super, initialise, 'Sim Maker'),
	send(GV, send_super, initialise),
	send(GV, frame_clear),
	send(GV, background, gainsboro),
	send(GV, label, 'Sim Maker'),
	
	send(GV, append, new(P, picture)),
	send(P, size, size(80, 350)),
	send(P, label, 'Graphical representation of sim'),

	send(new(DD, dialog(edit_toolbar)), below, P),
	send(DD, size, size(80, 25)),
	send(DD, background, gainsboro),
	send(DD, pen, 0),
	send(DD, append, new(TB2, tool_bar)),
	send(TB2, colour, gainsboro),
	send(TB2, gap, size(3, 3)),
	fill_editsim_toolbar(TB2),
	
	send(new(V, view), below, P),
	send(V, font, font(arial, normal, 12)),
	send(V, size, size(80, 15)),
    send(V, editable, @off),
	send(V, label, 'Connective definitions in sim'),

	new(Pm, message(GV, newnode, @event?position)),
	send(P, recogniser, click_gesture(left, '', single,
			  message(Pm, execute))),
	send(V, recogniser, popup_gesture(new(Pop, popup))),
	send_list(Pop, append,
			  [ menu_item('undo       Ctrl+Z', message(@arg1?editor, undo),
												condition := message(GV, get_edit_mode)),
				menu_item('copy       Ctrl+C', message(@arg1?editor, copy),
												condition := message(GV, get_edit_mode)),
				menu_item('paste      Ctrl+V', message(@arg1?editor, paste),
												condition := message(GV, get_edit_mode)),
				menu_item('cut         Ctrl+X', message(@arg1?editor, cut),
												condition := message(GV, get_edit_mode)),
				menu_item('find        Ctrl+F', message(GV, find_in),
												condition := message(GV, get_edit_mode)),
				menu_item('select_all Ctrl+A', message(@arg1?editor, mark_whole_buffer), end_group := @on,
												condition := message(GV, get_edit_mode)),
				menu_item(toggle_edit_mode, message(@arg1, toggle_edit_mode), end_group := @on),
				menu_item(show_key_bindings, message(@prolog, show_key_bindings, @arg1))
				]),

	send(V, key_binding, '\\C-f', message(GV, find_in)),
	send(V, key_binding, '\\C-a', message(V?editor, mark_whole_buffer)),
	
	new(Matrix, dialog),
	send(Matrix, name, dialog_matrix),
	send(Matrix, background, gainsboro),
	fill_matrix_dialog(Matrix),
	
	send(new(Oper, dialog),below, Matrix),
	send(Oper, name, dialog_oper),
	send(Oper, background, gainsboro),
	fill_operatorsim_dialog(Oper),
	send(Matrix, right, V),
	
	send(new(R, dialog(report)), below, V),
	send(R, background, gainsboro),
	lattice_maker:fill_dialog(R),

	send(new(Menu, dialog(dialog_menu)), above, P),
	% #D8D8D8 = gainsboro
	send(Menu, background, gainsboro),
	send(Menu, pen, 0),
	send(Menu, gap, size(3, 3)),
	send(Menu, append, new(MB, menu_bar)),
	fill_menu_sim(MB),
	send(Menu, append, new(TB, tool_bar)),
	send(TB, colour, gainsboro),
	send(TB, gap, size(3, 3)),
	fill_toolbar_sim(TB),
	send(TB, left, new(SimB, button(latticeMode, message(@prolog, lattice_gui), lattice_mode))),
	
	send(GV, icon, image('image.xpm')),
	send(GV, fit),
	send(GV, set_filename, ''),
	send(GV, set_initial_state),
	send(GV, set_selected_nothing).
	% send(GV, set_new_state),
	% send(GV, set_filename, ''),
	% send(GV, set_selected_node).

:- floper:consult('SimDCG.pl').

fill_menu_sim(MB) :-
	get(MB, frame, FrameMB),
	get(FrameMB, member, picture, P),

	send(MB, append, new(F, popup(file))),
	send(MB, append, new(E, popup(edit))),
	send(MB, append, new(G, popup(graphic))),
	send(MB, append, new(A, popup(connective))),
	send(MB, append, new(D, popup(distance))),
	send(MB, append, new(H, popup(help))),

	send(F, append, menu_item(new_lattice, message(FrameMB, new))),
	send(F, append, menu_item(load_sim, message(FrameMB, load))),
	send(F, append, menu_item(save_to_file, message(FrameMB, save),
									condition := message(FrameMB, lattice_noempty))),
	send(F, append, menu_item(save_as, message(FrameMB, saveAs),
									condition := message(FrameMB, lattice_noempty))),
	send(F, append, menu_item(print, message(FrameMB, print),
									condition := message(FrameMB, graph_noempty, P),
									end_group := @on)),
	send(F, append, menu_item(export_image, message(FrameMB, image),
									condition := message(FrameMB, graph_noempty, P))),
	send(F, append, menu_item(export_lattice_to_XML, message(FrameMB, export_xml),
									condition := message(FrameMB, lattice_noempty),
									end_group := @on)),
	send(F, append, menu_item(exit_program, message(FrameMB, bye))),

	send(E, append, menu_item(clear_text_editor, message(FrameMB, clear_editor),
									condition := message(FrameMB, get_edit_mode))),
	send(E, append, menu_item(toggle_edit_mode, message(FrameMB, toggle_edit_mode))),
	send(E, append, menu_item(add_connective, message(FrameMB, add_connective),
									condition := message(FrameMB, get_edit_mode))),
	send(E, append, menu_item(find_and_replace, message(FrameMB, find_in),
									condition := message(FrameMB, get_edit_mode),
									end_group := @on)),
	send(E, append, menu_item(redraw_graph_from_lattice, message(FrameMB, lattice_to_graph),
									condition := message(FrameMB, lattice_noempty))),
	send(E, append, menu_item(undo_redraw_graph, message(FrameMB, undo_send_graph),
									condition := message(FrameMB, lattice_noempty), 
                                    end_group := @on)),

    send(E, append, menu_item(restore_leq, message(FrameMB, restore_view_leq),
									condition := message(FrameMB, lattice_noempty))),
    
	send(G, append, menu_item(clear_graph_editor, message(FrameMB, clear_graph),
									condition := message(FrameMB, graph_noempty, P),
									end_group := @on)),
	send(G, append, menu_item(set_truth_degree_mode, message(FrameMB, set_selected_node))),
	send(G, append, menu_item(set_leq_arrow_mode, message(FrameMB, set_selected_arrow),
									end_group := @on)),
	send(G, append, menu_item(find_truth_degree, message(FrameMB, find),
									condition := message(FrameMB, graph_noempty, P))),
	send(G, append, menu_item(complete_graph, message(FrameMB, complete),
									condition := message(FrameMB, graph_noempty, P))),
	send(G, append, menu_item(normalize_graph, message(FrameMB, normalize),
									condition := message(FrameMB, graph_noempty, P))),

	send(A, append, menu_item(evaluate, message(FrameMB, eval_selected_connective),
									condition := message(FrameMB, lattice_noempty))),
	send(A, append, menu_item(test_connective, message(FrameMB, test_selected_connective),
									condition := message(FrameMB, lattice_noempty))),
									
	send(D, append, menu_item(evaluate, message(FrameMB, eval_distance),
									condition := message(FrameMB, lattice_noempty))),
    send(D, append, menu_item(test, message(FrameMB, check_distance),
									condition := message(FrameMB, lattice_noempty))),
	send(D, append, menu_item(generate, message(FrameMB, restore_view_distance),
									condition := message(FrameMB, lattice_noempty))),
    
	send_list(H, append, [ menu_item(user_manual, message(@helper, give_help, latticehelp, 'latticehelp')),
						   menu_item(about, message(FrameMB, about_dialog))
						   ]).

fill_toolbar_sim(TB) :-
	get(TB, frame, FrameTB),

	send(TB, append, tool_button(message(FrameTB, new), resource(imgnew), new_lattice)),
	send(TB, append, tool_button(message(FrameTB, load), resource(imgopen), load_sim)),
	send(TB, append, tool_button(message(FrameTB, save), resource(imgsave), save_to_file)),
	send(TB, append, tool_button(message(FrameTB, print), resource(imgprint), print)),
	send(TB, append, tool_button(message(FrameTB, image), resource(imgimage), export_image)),
	send(TB, append, tool_button(message(FrameTB, export_xml), resource(imgxml), export_to_xml)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, clear_graph), resource(imgclear), clear_graph_editor)),
	send(TB, append, tool_button(message(FrameTB, set_selected_node), resource(imgnode), set_truth_degree_mode)),
	send(TB, append, tool_button(message(FrameTB, set_selected_arrow), resource(imgarrow), set_leq_arrow_mode)),
	send(TB, append, tool_button(message(FrameTB, find), resource(imgfind), find_truth_degree)),
	% send(TB, append, tool_button(message(FrameTB, complete), resource(imggraph), complete)),
	send(TB, append, tool_button(message(FrameTB, sim_close), resource(imgnormalize), redraw_normalize_graph)),
	send(TB, append, gap),
	% send(TB, append, tool_button(message(@prolog, lattice_gui), resource(imgswitch), lattice_mode)),
	send(TB, append, tool_button(message(@helper, give_help, latticehelp, 'latticehelp'), resource(imghelp), help)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, bye), resource(imgquit), exit_program)),
	send(TB, append, gap),
	send(TB, append, gap),
	send(TB, append, gap),
	send(TB, append, gap),
	send(TB, append, gap).

	
fill_matrix_dialog(DM) :-
	send(DM, display, new(T, matrix)),
	send(DM, background, white),
	try(sim:lista(List),List = []),
	set_column_names(T, List),
	fill_matrix_row(T, List, List).

fill_matrix_row(_, _, []).
fill_matrix_row(T, List, [Item|List2]) :-
	get_nodesim_member(Item, String),
	send(T, append, String, bold, center),
	fill_matrix_row2(T, Item, List),
	fill_matrix_row(T, List, List2).
	
fill_matrix_row2(T, _, []):- send(T, next_row).
fill_matrix_row2(T, Item1, [Item2|List]) :-
	lat:bot(Bot),
	try(sim:r(Item1,Item2, Val), Val = Bot),
	new(Cell, matrix_cell(Val)),
	send(T, append, Cell),
	fill_matrix_row2(T, Item1, List).
	
fill_editsim_toolbar(TB) :-
	get(TB, frame, FrameTB),
	send(TB, append, tool_button(message(FrameTB, clear_editor), resource(imgclean), clear_text_editor)),
	send(TB, append, tool_button(message(FrameTB, toggle_edit_mode), resource(imgedit), toggle_edit_mode)),
	% send(TB, append, tool_button(message(FrameTB, add_connective), resource(imgadd), add_connective)),
	% send(TB, append, tool_button(message(FrameTB, find_in), resource(imgeditfind), find_and_replace)),
	send(TB, append, gap),
	send(TB, append, tool_button(message(FrameTB, sim_to_graph), resource(imgsendup), redraw_graph_from_lattice)),
	send(TB, append, tool_button(message(FrameTB, undo_send_graph), resource(imgundo), undo_redraw_graph)).
    % send(TB, append, gap),
    % send(TB, append, tool_button(message(FrameTB, restore_view_leq), resource(imgleq), restore_leq)).


fill_operatorsim_dialog(D) :-
	add_label(D, titlemat, 'Operators dialog', normal, black, 13).

get_linesim_editor(F, StringA, StringB, Index) :-
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),
	retractall(sim:indexA(_)),
	retractall(sim:indexB(_)),
	try((sim:lista(L),
		length(L, ListaNodos),
		Max is ListaNodos - 1
	), 
		false
	),
	
	%% Obtener posicion del primer elemento
	assertz(sim:indexA(-1)),!,
	between(0, Max, _),
		sim:indexA(Init1),
		Init1Fix is Init1 + 1,
		get(B, find, Init1Fix, StringA, 1, start, @on, @on, IndexA),
		IndexA < Length,
		retractall(sim:indexA(_)),
		assertz(sim:indexA(IndexA)),
		get(B, line_number, IndexA, LineA),
		
	%% Obtener posicion del segundo elemento
	assertz(sim:indexB(-1)),
	between(0, Max, _),
		sim:indexB(Init2),
		Init2Fix is Init2 + 1,
		get(B, find, Init2Fix, StringB, 1, start, @on, @on, IndexB),
		IndexB < Length,
		retractall(sim:indexB(_)),
		assertz(sim:indexB(IndexB)),
		get(B, line_number, IndexB, LineB),
			(	LineA == LineB
			->	!
			;	false
			),
	
	retractall(sim:indexA(_)),
	retractall(sim:indexB(_)),
	Index is min(IndexA, IndexB).

add_valsim_editor(F, StringA, StringB, Val, Pos) :-
	lattice_maker:lat:bot(Bot),
	get_node_member(Val, ValN),
	(	Bot = ValN
	;	get(F, member, view, V),
		get(V, editor, E),
		send(E, exact_case, @on),
		get(V, text_buffer, B),
		get(B, length, Length),
		new(NewValRule, string('%s ~ %s = %s.\n', new(string(StringA)), new(string(StringB)), new(string(Val)))),
		send(B, insert, Pos, NewValRule)
	).
	
add_valsim_editor(F, StringA, StringB, Val) :-
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),
	(	get(B, find, 0, '~tnorm', 1, start, @on, @on, Pos)
	->	true
	;	Pos is Length
	),
	add_valsim_editor(F, StringA, StringB, Val, Pos).

remove_valsim_editor(F, StringA, StringB) :-
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),!,
	get_linesim_editor(F, StringA, StringB, IndexStart),
	between(IndexStart, Length, IndAux),
	IndAux2 is IndAux + 1,
	%% Si no encuentra la linea a la que pertene es un error irrecuperable
	(	get(B, line_number, IndAux, Line1)
	;	try(Line1 is Line1, false),
		!,false
	),
	(	get(B, line_number, IndAux2, Line2)
	;	try(Line2 is Line2, false),
		IndAux2 is Length + 1,
		Line2 is Line1 + 1
	),
	(	1 is Line2 - Line1
	;	IndAux2 > Length
	),!,
	IndexEnd is IndAux,
	send(B, delete, IndexStart, IndexEnd + 1 - IndexStart).
	
rename_nodesim_editor(F, Old, New) :-
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),
	% get_nodesim_member(NOld, Old),
	% get_nodesim_member(NNew, New),
	% atom_length(Old, Chars),
	string_length(Old, Chars),
	Int = 0,
	repeat,
	(	get(B, find, Int, Old, 1, start, @on, @on, Index),
		Index < Length
	->	send(B, delete, Index, Chars),
		send(B, insert, Index, New),
		Int = Index + 1,
		% send(F, report, 'Item replaced in editor'),
		fail
	;	!
	).

rename_nodesim_matrix(F, NNew) :-
	get(F, member, dialog_matrix, DM),
	get(DM, member, matrix, Tab),
	sim:lista(L),
	nth1(X, L, NNew),
	get_nodesim_member(NNew, New),
	get(Tab, get_cell, X, 0, Cell),
	send(Cell, value, New),
	get(Tab, get_cell, 0, X, Cell2),
	send(Cell2, value, New).

set_sim_tnorm(F, Tnorm) :->
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length), !,
	(	lattice_maker:floper:sim_tnorm(TnormDef),
		TnormDef \== Tnorm
	->	name(TnormDef, TnormDefString),
		send(B, find, 0, TnormDefString, 1, start, @on, @on, Index),
		length(TnormDefString, TnormL),
		send(B, delete, Index, TnormL),
		name(Tnorm, TnormString),
		send(B, insert, Index, TnormString)
	;	
		(	send(B, find, 0, '~tnorm', 1, start, @on, @on, IndexStart),
			send(B, find, IndexStart, '.', 1, start, @on, @on, IndexEnd),
			send(B, delete, IndexStart, IndexEnd + 1 - IndexStart)
		;	IndexStart is Length
		),
		name(Tnorm, TnormString),
		new(TnormRule, string, '\n~tnorm = %s.', TnormString),
		send(B, insert, IndexStart, TnormRule)
		
	),
	retract(lattice_maker:floper:sim_tnorm(_)),
	assertz(lattice_maker:floper:sim_tnorm(Tnorm)).

update_editor(F) :->
	get(F, member, view, V),
	get(V, editor, E),
	send(E, exact_case, @on),
	get(V, text_buffer, B),
	get(B, length, Length),
	send(B, delete, 0 , Length + 1),
	sim:lista(L),
	sim_line_editor(F, L),
	get(B, length, Length2),
	lattice_maker:floper:sim_tnorm(Tnorm),
	name(Tnorm, TnormString),
	new(TnormRule, string('\n~tnorm = %s.', new(string(TnormString)))),
	send(B, insert, Length2, TnormRule).


sim_line_editor(F, [A]).
sim_line_editor(F, [A|L]) :-
	(
		select(B, L, _),
		(	sim:r(A, B, Val)
		->	get_node_member(ValStr, Val),
			get_nodesim_member(A, AStr),
			get_nodesim_member(B, BStr),
			add_valsim_editor(F, AStr, BStr, ValStr)
		;	true
		),
		false
	;	true
	),
	sim_line_editor(F, L).
	
sim_line_editor(F, _).

	
clearspace(F) :->
	"Clear the work space"::
	get(F, member, picture, P),
	get(F, member, view, V),
	send(P, clear),
	send(V, clear),

	% get(F, member(dialog_eval), D),
	% get(D, member, connectives, Aggr),
	% send(Aggr, clear),
	% send_list(Aggr, append, [empty]),

	% get_container_combo(D, C),
	% forall(between(1, 6, I),
		% (	get_term_name(I, StrText),
			% get(C, member(StrText), DM),
			% get_var_name(I, Var),
			% send(DM, clear),
			% send_list(DM, append, [Var])
		% )),
	% get(D, member, view, DV),
	% send(DV, clear),
	wipe_module(sim),
	% send(F, redrawn, @off),
	send(F, report, status, '').

set_selected_node(F) :->
	send(F, slot, drawnode, @on),
	send(F, slot, drawarrow, @off),
	retractall(sim:fromNode(_)).
	
sim_to_graph(F) :->
	get(F, member, view, V),
	get(V, text_buffer, B),
	get(B, modified, Mod),

	tmp_file_stream(text, File, Stream),
	close(Stream),
	send(B, save, File),
	
	%% Run floper
	loadLat,
	lattice_maker:floper:sim(File, Errors),
	
	sim:import(lattice_maker:floper:sim:r/3),
	sim:import(lattice_maker:floper:sim:lista/1),
	send(F, draw_graph),
	send(F, update_matrix),
	% send(DM, clear),
	% fill_matrix_dialog(DM),
	send(F, report, status, Errors),
	send(F, label, string('SimMaker')),
	send(F, fit),
	send(B, modified, Mod).

sim_close(F) :->
	get(F, member, view, V),
	get(V, text_buffer, B),
	get(B, modified, Mod),
	
	% trace,
	% current_predicate(sim:r/3),
	% lattice_maker:floper:sim_clean,
	% current_predicate(sim:r/3),
	% sim:export(lattice_maker:floper:sim:r/3),
	% sim:export(lattice_maker:floper:sim:lista/1),
	% notrace,
	lattice_maker:floper:close,
	% trace,
	% sim:import(lattice_maker:floper:sim:r/3),
	% sim:import(lattice_maker:floper:sim:lista/1),
	% notrace,
	
	send(F, draw_graph),
	send(F, update_matrix),
	send(F, update_editor),
	% send(DM, clear),
	% fill_matrix_dialog(DM),
	send(F, report, status, 'Relations normalized'),
	send(F, label, string('SimMaker')),
	send(F, fit),
	send(B, modified, Mod).
	
update_matrix(F) :-> 
	get(F, member, dialog_matrix, DM),
	send(DM, clear),
	fill_matrix_dialog(DM),
	send(F, fit).
	
new(F) :->
	ok_changes(F),
	send(F, clearspace),
	% wipe_module(sim),
	lat:top(Top),
	assertz(sim:lista([])),
	assertz(sim:r(X,X,Top)),
	get(F, member, dialog_matrix, DM),
	send(DM, clear),
	fill_matrix_dialog(DM),
	send(F, fit),
	
	loadLat,
	% get(F, member, view, V),
	% get(V, editor, E),
	% send(E, exact_case, @on),
	% get(V, text_buffer, B),
	% get(B, length, Length),
	% try(lattice_maker:floper:sim_tnorm(Tnorm), Tnorm = ''),
	% new(TextEditor, string('\n~tnorm = %s.', Tnorm)),
	% send(B, insert, Length, TextEditor),
	
	send(F, label, string('SimMaker')),
	send(F, set_new_state),
	send(F, set_filename, ''),
	send(F, set_selected_node).
	
load(F) :->
	"Ask for a file name"::
	ok_changes(F),
	get(F, member, view, V),
	get(V, editor, E),
	get(F, member, dialog_matrix, DM),
	get(@finder, file, @on, '.sim', FileName),
    send(F, set_filename, FileName),
	send(F, set_nonew_state),
	
	%% Run floper
	loadLat,
	lattice_maker:floper:sim(FileName, Errors),
	sim:import(lattice_maker:floper:sim:r/3),
	sim:import(lattice_maker:floper:sim:lista/1),
	
	send(F, draw_graph),
	send(DM, clear),
	fill_matrix_dialog(DM),
	send(V, load, FileName),
	send(V, editable, @off),
	% send(F, report, status, 'Parsing succeed - File %s read', FileName),
	send(F, report, status, Errors),
	send(F, label, string('SimMaker - %s', FileName)),
	send(F, fit).

save(F) :->
	"Write to file"::
	get_new_state(F)
	->	send(F, saveAs)
	;	
		% send(F, lattice_noempty),
		ok_normalizesim(F),
		(	get_filename(F, FileName)
		->	true
		;	get(@finder, file, save, '.sim', FileName)
		),
		add_generated_to_file(F, FileName, Path),

		get(F, member, view, V),
		send(V, save, Path),

		send(F, report, status, 'Saved in %s', Path),
		send(F, label, string('SimMaker - %s', Path)),		
		send(F, set_nomodified_state),
		send(F, set_filename, Path).

saveAs(F) :->
	"Write to file"::
	writeln('entro en saveas'),
	% send(F, lattice_noempty),
	ok_normalizesim(F),
	get(@finder, file, save, '.sim', FileName),
	add_generated_to_file(F, FileName, Path),
	
	get(F, member, view, V),
	send(V, save, Path),

	send(F, report, status, 'Saved in %s', Path),
	send(F, label, string('SimMaker - %s', Path)),		
	send(F, set_nomodified_state),
	send(F, set_filename, Path).

draw_graph(F) :->
	get(F, member, picture, P),
	send(P, clear),
	sim:lista(L),
	length(L,NL),
	Rad is (2*pi / NL),
	between(1,NL,Ind),
		nth1(Ind, L, Item),
		calc_pos(Rad, Ind, X, Y),
		get_nodesim_member(Item, Name),
		% (
			% (	0 is Ind mod 2 ->	get(F, node, Name, green, X - 30, Y - 15, Node));
			% (	1 is Ind mod 2 ->	get(F, node, Name, green, X, Y, Node));
			% (	2 is Ind mod 2 ->	get(F, node, Name, green, X + 15, Y + 30, Node))
		% ),
		(	Ind > NL/2
		->	get(F, node, Name, green, X*1.5, Y*1.5, Node)
		;	get(F, node, Name, green, X, Y, Node)
		),
	Ind=NL,
	darcsim(F).
	
get_nodesim_member((Name, Arity), String) :-
	(
		catch((
			atom_string(Name, S1),
			if(Arity>0, 
				string_concat('/', Arity, S2)
			,
				string_concat('', '', S2)
			),
			string_concat(S1, S2, String)
		),_,fail)
	;
		(	
			string_codes(String, Codes),
			if(append(NameC, [47|ArityC], Codes),(
				ArityC2 = ArityC
			),(
				NameC = Codes,
				ArityC2 = []
			)),
			get_nodesim_member_aux(NameC, Name, ArityC2, Arity)
		)
	),!.

get_nodesim_member_aux(NameC, Name, [], 0) :-
	atom_codes(Name, NameC).
	
get_nodesim_member_aux(NameC, Name, ArityC, Arity) :-
	atom_codes(Name, NameC),
	number_codes(Arity, ArityC).
	
newnode(F, Position) :->
	get_selected_node(F),
	get(F, nodePos, 'noname', Position, NewNode),
	send(NewNode, rename).
	
	%% Actualizar matrix
	% send(F, update_matrix).

nodePos(F, Name, Position, Node:sim_node) :<-
	"In user mode, get (create) node with specified name"::
	get(F, member, picture, P),
	(	get(P, member, Name, Node)
	->  true
	;  	send(P, display, new(Node, sim_node(Name, green)), Position)
	).
	
calc_pos(Rad, Ind, X, Y):-
	X is integer(cos(Rad * Ind) * 100),
	Y is integer(sin(Rad * Ind) * 100).
	
node(F, Name, Color, X, Y, Node:sim_node) :<-
	"In layering mode, get (create) node with specified name"::
	get(F, member, picture, P),
	(	get(P, member, Name, Node)
	->  true
	;	get(P, visible, area(_, _, W, H)),
		get(Name, value, Name2),
		with_output_to(codes(Codes), write(Name2)),
		length(Codes, Length),
		% 4 characters inside the circle
		(	Length > 4, MoveX is 6 * (Length - 4) // 2; MoveX is 0	),

		NX is X - MoveX + W/2 ,
		NY is Y + H/2 ,
		send(P, display, new(Node, sim_node(Name, Color)), point(NX, NY))
	).
	
loadLat :-
	% wipe_module(lattice_maker:floper:lat),
	lattice_maker:floper:lat:import(lat).
	
ok_normalizesim(F) :-
	get_modified_state(F)
	->  get(F, msgbox, 'Changes must be normalized. Normalize now?', RT),
		RT \= @nil,
		send(F, sim_close)
	;   true.

:- pce_end_class.

