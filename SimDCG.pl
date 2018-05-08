:- module(floper,[]).
:- use_module(library(pce)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%     FASILLER: Fuzzy LOgic Programming Environment for Research.         %%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROGRAM MENU OPTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Puts the file in a list %%
stream2list(Stream,Text) :- get_code(Stream,X),
 (X==9,Text=[' '|Text2],!,stream2list(Stream,Text2) % ignore 'tab' and puts a blank 
 ;X\==(-1),name(P,[X]),Text=[P|Text2],!,stream2list(Stream,Text2) 
 ;X==(-1),Text=[]). % EOF

%% Returns a char list from a code list.
to_char_list([A|B],[Ca|Cb]):-name(Ca,[A]),to_char_list(B,Cb).
to_char_list([],[]).

%% clean the list
clean(['%'|Text1],Text3) :- clean_simple_comment(Text1,Text2),clean(Text2,Text3). % ignore simple comment
clean(['/','*'|Text1],Text3) :- clean_complex_comment(Text1,Text2),clean(Text2,Text3). % ignore complex comment
clean([X|Text1],[X|Text2]) :- clean(Text1,Text2).
clean([],[]).

clean_simple_comment(['\r'|Text],['\r'|Text]) :- !. % RC  
clean_simple_comment(['\n'|Text],['\n'|Text]) :- !. % New line
clean_simple_comment([_|Text1],Text2) :- clean_simple_comment(Text1,Text2). % Any other symbol
clean_simple_comment([],[]). % sure terminate

clean_complex_comment(['*','/'|Text],Text) :- !.
clean_complex_comment([_|Text1],Text2) :- clean_complex_comment(Text1,Text2). % Any other symbol
clean_complex_comment([],_) :- error('Complex comment must end with \'*/\'.'). 

%% LIST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list :- try((loaded_file_pl(PL), list_pl(PL)),(write('No loaded files'),nl)),
	try((loaded_file_fpl(FPL), list_fpl(FPL)),write('No parsed files')).

list_pl(P):- nl, write('LOADED FILES CODE:'), nl,nl, list(P).
list_fpl(P):- nl, write('ORIGINAL FUZZY-PROLOG CODE:'), nl,nl, list(P),
		nl,write('GENERATED PROLOG CODE:'),nl,nl,list('tmp_fuzzy-prolog.pl').

list(F) :- open(F,read,Stream),stream2list(Stream,List),clean(List,Text),print_list(Text),nl,close(Stream).
print_list([]).
print_list([C|T]) :- (C == 'end_of_file'
		     ;write(C),print_list(T)).

append_list([],['']).
append_list([A], A).
append_list([A|B], C) :- append_list(B, C1), string_concat(A, C1, C).

get_error([],['']).
get_error([L],L).

%% CLEAN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clean :- fasiller_default, assert(loaded_file_lat('num.lat')),wipe_module(fpl).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SIMILARITIES MENU OPTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sim_filestream(Stream, Ret) :- stream2list(Stream,Text),close(Stream),
	% clean(Text,Input),malp_begin,blanks(Input,Inp2),sim2(Inp2, Status), malp_errorList(L1), get_error(L1, L2),append_list(L2, Ret1), string_concat(Status, Ret1, Ret), malp_end,
	% assert(sim:lista([])),create_list, close.

sim_clean :-
	wipe_module(sim).
	
%% Load similarity file
sim(File, Ret) :- write('Current similarity file: '),write(File), write('Current similarity file: none'),
	nl, write('Similarity File: '),write(File),wipe_module(sim),
	open(File,read,Stream),stream2list(Stream,Text),close(Stream),
	clean(Text,Input),malp_begin,blanks(Input,Inp2),sim2(Inp2, Status), malp_errorList(L1), get_error(L1, L2),append_list(L2, Ret1), string_concat(Status, Ret1, Ret), malp_end,
	assert(sim:lista([])),create_list, close. %% set_sim_tnorm, set_sim_tconorm, set_sim_lcut.

sim2(Input, Ret):- try((simprog(Input,[]), (Ret = 'Parsing succeed. \t' ,write(Ret),nl)),(nl, Ret = 'Parsing failed. \t',write(Ret),nl)),!.

%%%%%%%%%%%%%%%%%%%%% For SimMaker UI %%%%%%%%%%%%%%%%%%%%%%

get_sim_tnorms(L) :-
	setof( A, get_sim_tnorm(A), L).
	
get_sim_tnorm(G) :-
	current_predicate(lat:P/_), 
	name(P,Pn),
	append("and_",C,Pn),
	string_codes(G,C).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_sim_tnorm :- try(sim_tnorm(_), try((current_predicate(lat:P/_), 
	name(P,Pn),append("and_",C,Pn),name(G,C),assert(sim_tnorm(G))), (nl,write('WARNING: Tnorm not found.'),nl))).
set_sim_tnorm(A) :- retractall(sim_tnorm(_)), assert(sim_tnorm(A)).

set_sim_tconorm :- try(sim_tconorm(_), try((current_predicate(lat:P/_), 
	name(P,Pn),append("or_",C,Pn),name(G,C),assert(sim_tconorm(G))), (nl,write('WARNING: Tconorm not found.'),nl))).
set_sim_tconorm(A) :- retractall(sim_tconorm(_)), assert(sim_tconorm(A)).

set_sim_lcut :- try(sim_lcut(_), (lat:bot(B), retractall(sim_lcut(_)), assert(sim_lcut(B)))).
set_sim_lcut(B) :- retractall(sim_lcut(_)), assert(sim_lcut(B)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simprog --> simrule, simprog.
simprog --> [].

simrule --> ['~'], blanks, simrulek.
simrule -->idSim(A), blanks, need(['~'],'~'), blanks, idSim(B), blanks, 
	   tdSim(V), blanks, need(['.'],'.'), blanks, {assertz(sim:r(A,B,V))}.
simrulek --> [t,n,o,r,m], blanks, need(['='],'='), blanks, need(id(TN),'label'), blanks, need(['.'],'.'), blanks, {set_sim_tnorm(TN)}.
simrulek --> [t,c,o,n,o,r,m], blanks, need(['='],'='), blanks, need(id(TN),'label'), blanks, need(['.'],'.'), blanks,{set_sim_tconorm(TN)}.
simrulek --> [l,c,u,t], blanks, need(['='],'='), blanks, need(term(T),'Truth degree after \'=\''), blanks, need(['.'],'.'),blanks,
	{fpl2pl_term(T,PT), (lat:member(PT); malp_newError([PT,' is not a member of lattice'])), set_sim_lcut(PT)}, !.

idSim((A,N)) --> id(A), idSim2(N).
idSim2(N) --> ['/'], int(X),{to_string(X,C,_),append(C,[46],C1),read_from_chars(C1,N),!}.
%% idSim2(all) --> [].
idSim2(0) --> [].

tdSim(PT) --> ['='],blanks,need(term(T),'Truth degree after \'=\''), 
	{fpl2pl_term(T,PT), (lat:member(PT); malp_newError([PT,' is not a member of lattice']))}, !.
tdSim(T) --> [], {lat:top(T)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

may_insert(X) :- retract(sim:lista(L)), (exists(X,L), assert(sim:lista(L)) ; assert(sim:lista([X|L]))), !.

exists(X,[X|_]).
exists(X,[_|A]):-exists(X,A).

may_assert(r(A,B,V)) :- sim:r(A,B,W),lat:leq(V,W), !. %%W>V,!.
may_assert(r(A,B,V)) :- retractall(sim:r(A,B,_)),assertz(sim:r(A,B,V)).

%% Primero rellenamos la lista de símbolos

create_list :- sim:r(X,Y,_), may_insert(X), may_insert(Y), fail.
create_list.

%% Luego hacemos los cierres

% close_ref :- asserta(sim:r(A,A,1)).
% close_ref :- lat:top(T), asserta(sim:r(A,A,T)).
close_ref :- retractall(sim:r(X,X,_)), lat:top(T), asserta(sim:r(A,A,T)).

close_sim :- sim:lista(L), close_sim(L).
close_sim([]).
close_sim([A|L]):- (setof(sol(B,X),sim:r(A,B,X),S), close_sim_d(A,S);true),
		   (setof(sol(B,X),sim:r(B,A,X),S), close_sim_r(A,S);true),
		   close_sim(L).

close_sim_d(_,[]).
close_sim_d(A,[sol(B,X)|L]):- may_assert(r(B,A,X)), close_sim_d(A,L).
close_sim_r(_,[]).
close_sim_r(A,[sol(B,X)|L]):- may_assert(r(A,B,X)), close_sim_r(A,L).

close_trans :- setof(rel(A,B,V),sim:r(A,B,V),S), close_trans(S).
close_trans([]).
close_trans([rel(A,B,V)|L]):- (setof(sol(X,W),sim:r(B,X,W),S), close_trans(A,V,S);true),
			      close_trans(L).
close_trans(_,_,[]).
% close_trans(A,V,[sol(X,W)|L]) :- sim_tnorm(And),V=\=W , name(And,Land),name(and_,Pre),append(Pre,Land,Cand),name(Und,Cand), Q=..[Und,V,W,Z], lat:Q,
close_trans(A,V,[sol(X,W)|L]) :- sim_tnorm(And), name(And,Land),name(and_,Pre),append(Pre,Land,Cand),name(Und,Cand), Q=..[Und,V,W,Z], lat:Q,
	may_assert(r(A,X,Z)), may_assert(r(X,A,Z)), close_trans(A,V,L).

% close :- close_sim, close_trans, close_ref.
close :- close_sim, close_trans, close_ref.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GOAL MENU OPTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Returns a List and a Code_list from an Atom.---> [b,o,d,y] ---> [98,111,100,99] ---> body
to_string(List,Code_list,Atom) :-aux(List,Code_list), name(Atom,Code_list).
aux([],[]).
aux([H|T],[H1|T1]):-name(H,[H1]),aux(T,T1). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PARSER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

term2char(T,C) :- write_to_chars(T,N), to_char_list(N,C).

char2list([A|B],[Ca|Cb]):-name(Ca,[A]),char2list(B,Cb).
char2list([],[]).

list2id(L,Id) :- var(Id),!, char2list(C,L), name(Id,C).
list2id(L,Id) :- var(L), !, name(Id,C), char2list(C,L).

malp_end:- malp_errors, statistics(walltime,[E,_]), retract(malp_time(S)), retract(malp_lineCount(L)), T is (E-S)/1000,
	print_list(['Parsed: ',L,' lines. ',T,' seconds']),nl, retract(malp_errorCount(_)), retract(malp_warningCount(_)),
	retract(malp_errorList(_)), retract(malp_ruleCount(_)),retractall(malp_aVarIndex(_)), nl.

malp_errors :- malp_errorList(L), reverse(L,R), malp_errors2(R), malp_numErrors.
malp_errors2([]).
malp_errors2([A|B]) :- print_list(A),nl,malp_errors2(B).
malp_numErrors :- malp_errorCount(E), malp_warningCount(W),
	print_list([E,' errors; ',W,' warnings']),nl.

malp_begin :- retractall(malp_lineCount(_)), retractall(malp_errorList(_)), retractall(malp_ruleCount(_)),
	retractall(malp_errorCount(_)), retractall(malp_warningCount(_)),
	assert(malp_lineCount(1)), assert(malp_errorList([])), assert(malp_ruleCount(1)),
	assert(malp_errorCount(0)), assert(malp_warningCount(0)),
	retractall(malp_time(_)), statistics(walltime,[S,_]), assert(malp_time(S)),
	retractall(parsingState(_)), assert(parsingState(normal)),
	retractall(malp_aVarIndex(_)), assert(malp_aVarIndex(0)).

malp_resetAVar :- retractall(malp_aVarIndex(_)), assert(malp_aVarIndex(0)).

malp_newError(E):- retract(malp_errorCount(Er)), Err is Er+1, assert(malp_errorCount(Err)), retract(malp_errorList(L)), malp_lineCount(N), assert(malp_errorList([['Line: ',N,'. ERROR !: '|E]|L])), !.
malp_newError_foundExpected(E,[X|Y],[X|Y]) :- malp_newError(['\'',X,'\' found. \'',E,'\' expected']).
malp_newError_foundExpected(E,[],[]) :- malp_newError(['\'\\eof\' found. \'',E,'\' expected']).
malp_newWarning(E):- retract(malp_warningCount(Wa)), War is Wa+1, assert(malp_warningCount(War)), retract(malp_errorList(L)), malp_lineCount(N), assert(malp_errorList([['Line: ',N,'. WARNING: '|E]|L])), !.
malp_newLine :- retract(malp_lineCount(N)), M is N+1, assert(malp_lineCount(M)), !.
'malp_newLine?'(X) :- ((name(X,[10]);name(X,[13])), malp_newLine;true).
malp_newRule(N) :- retract(malp_ruleCount(N)), M is N+1, assert(malp_ruleCount(M)), !.

setParsingState(M) :- retract(parsingState(_)), assert(parsingState(M)).

%% sync, reads input until symbol '.'.
sync --> {setParsingState(syn)}, sync2.
sync2 --> ['\n'], {malp_newLine}, sync2.
sync2 --> ['\r'], {malp_newLine}, sync2.
sync2 --> ['/','*'], blank_comB, sync2.
sync2 --> ['%'], blank_comL, sync2.
sync2 --> ['.'], !.
sync2 --> [_], sync2.
sync2 --> [], !.

%% allbut(C,S), reads S until symbol C. If no C, then raises error. Actualices malp_lineCount and malp_errorList
allbut(C,['\r'|Id]) --> ['\r'],!,{malp_newLine}, allbut(C,Id).
allbut(C,['\n'|Id]) --> ['\n'],!,{malp_newLine}, allbut(C,Id).
allbut(C,['\\',X|Id]) --> ['\\',X],!, allbut(C,Id).
allbut(C,[X|Id]) --> [X], {\+(X = C)},allbut(C,Id).
allbut(_,[]) --> [].

%% blank, blanks: Read white spaces, omit commentaries, actualizes malp_lineCount and malp_errorList
blank --> [' '].
blank --> ['\n'], {malp_newLine}.
blank --> ['\r'], {malp_newLine}.
blank --> ['\t'].
blank --> ['/','*'], blank_comB.
blank --> ['%'], blank_comL.
blank_comB --> ['*','/'].
blank_comB --> [X], {((X='\n';X='\r'),!,malp_newLine;true)}, blank_comB.
blank_comB --> [], malp_newError_foundExpected('*/').
blank_comL --> ['\n'], {malp_newLine}.
blank_comL --> ['\r'], {malp_newLine}.
blank_comL --> [_], blank_comL.
blank_comL --> [].
blanks --> blank, blanks, !.
blanks --> [].

%% need(X,E): El NT X es obligatorio. El input esperado es E.
need(_,_) --> {parsingState(syn)},!.
need(X,_) --> X, !.
need(_,E) --> malp_newError_foundExpected(E), sync.

%% En "(8+1)/2", "(8+1)" es considerado una tupla de un elemento. Luego se corrige.
term(T) --> expr(T),!.
termX(T) --> string(T),!.
termX(T) --> list(T),!.
termX(T) --> var(T),!.
termX(T) --> number(T),!.
termX('#'(I,N,L)) --> id(I),!, blanks, termX2(N,L),blanks,!.
termX('#'(',',N,[T|L])) --> ['('],!, blanks, need(term(T),'Term after \'(\''), terms(M,L), need([')'],')'), {N is M+1}.
termX2(N,[T|L]) --> ['('],!, blanks, need(term(T),'Term after \'(\''), terms(M,L), need([')'],')'), {N is M+1}.
termX2(0,[]) --> [].
terms(N,[A|B]) --> [','],!, blanks, need(term(A),'Term after \',\''), blanks, terms(M,B), {N is M+1}.
terms(0,[]) --> [].

expr(T) --> 'S'(A), blanks, expr2(A,T).
expr2_(A,'#'('=<',2,[A,B]))--> ['=','<'], !,blanks, need(expr(B),'Expression after \'=<\''),!.
expr2_(A,'#'('>=',2,[A,B]))--> ['>','='], !,blanks, need(expr(B),'Expression after \'>=\''),!.
expr2_(A,'#'('=',2,[A,B]))--> ['='], !,blanks, need(expr(B),'Expression after \'=\''),!.
expr2_(A,'#'('~',2,[A,B]))--> ['~'], !,blanks, need(expr(B),'Expression after \'~\''),!.
expr2_(A,'#'('<',2,[A,B]))--> ['<'], !,blanks, need(expr(B),'Expression after \'<\''),!.
expr2_(A,'#'('>',2,[A,B]))--> ['>'], !,blanks, need(expr(B),'Expression after \'>\''),!.
expr2(A,B) --> expr2_(A,B).
expr2(A,A) --> [].
'S'(T) --> 'F'(A), blanks, 'S2'(A,T).
'S2'(A,'#'('+',2,[A,B])) --> ['+'], !,blanks, need('S'(B),'Expression after \'+\''),!.
'S2'(A,'#'('-',2,[A,B])) --> ['-'], !,blanks, need('S'(B),'Expression after \'-\''),!.
'S2'(A,A) --> [].
'F'(T) --> termX(A),blanks,'F2'(A,T).
'F2'(A,'#'('*',2,[A,B])) --> ['*'], !,blanks, need('F'(B),'Expression after \'*\''),!.
'F2'(A,'#'('/',2,[A,B])) --> ['/'], !,blanks, need('F'(B),'Expression after \'/\''),!.
'F2'(A,A) --> [].

id(I) --> ['\''],!, allbut('\'', D), need(['\''],'\''), blanks, {list2id(D,I)}, !.
id(I) --> id2(I).
id2(I) --> [X], {minus(X)}, id_(D), {list2id([X|D],I)},!.
id_([X|D]) --> [X], {(letter(X);number(X);X='_')}, id_(D).
id_([]) --> [].

var(var(V)) --> [X], {(mayus(X);X='_')}, !, id_(D), {(X='_', malp_aVarConvert(['_'|D],V);list2id([X|D],V))}, blanks.

malp_aVarConvert(['_'],X) :- !, retract(malp_aVarIndex(N)), M is N+1, assert(malp_aVarIndex(M)), list2id(V,N),list2id(['_'|V],X).
malp_aVarConvert(['_'|D],X):- list2id(['_','_'|D],X).

list(L) --> ['['],!, blanks, list2(L), blanks, need([']'],']').
list2('#'('.',2,[T,R])) --> term(T),!,blanks, terms(_,L), blanks, list3(S), {terms2PlList(L,S,R)}.
list2('#'([],0,[])) --> [].
list3(T) --> ['|'], !, blanks, need(term(T),'term after \'|\'').
list3('#'([],0,[])) --> [].
terms2PlList([],S,S).
terms2PlList([T|L],S,'#'('.',2,[T,R])):-terms2PlList(L,S,R).

string(C) --> ['"'],!, allbut('"',S), need(['"'],'\"'), blanks, {char2list(C_,S),listize(C_,C)}.
listize([],'#'([],0,[])).
listize([A|B],'#'('.',2,[A,S])) :- listize(B,S).

letter(X) :- name(X,[C]), (C>=97, C=<122; C>=65, C=< 90).
minus(X) :- name(X,[C]), C>=97, C=<122.
mayus(X) :- name(X,[C]), C>=65, C=<90.

number(num(X)) --> ['-'], need(int(A),'Integer after \'-\''), number_(B), {append(['-'|A],B,C),list2id(C,X)}.
number(num(X)) --> int(A), number_(B), {append(A,B,C),list2id(C,X)}.
number_(['.'|A]) --> ['.'], int(A).
number_([])--> [].
int([X|Y]) --> [X], {number(X)},!, int_(Y).
int_([X|Y])--> [X], {number(X)},!, int_(Y).
int_([])--> [].


fpl2pl_term('#'(Id,_,L),S):- !,fpl2pl_terms(L,R),S=..[Id|R],!.
fpl2pl_term(var(X),X):- !.
fpl2pl_term(num(X),X):- !.
fpl2pl_term(S,S):- !.
fpl2pl_terms([],[]).
fpl2pl_terms([A|B],[C|D]):-fpl2pl_term(A,C),!,fpl2pl_terms(B,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIARY PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% try(dangerous block, exception): obtain a fasiller value with no errors
try(X,F):-(catch(X,_,F),!;F).

%% nth element
nth_(1,[E|L],E,L).
nth_(N,[E|L],X,[E|R]) :- N>0, M is N-1, nth_(M,L,X,R).

%% wipe_module(M): erase a module M
wipe_module(M) :- 
        (   current_predicate(M:P/A), 
            (\+(current_predicate(license:P/A)) %Evitamos que coja '=', '<', etc..., que aparecen como predicados de 'license'
            ; fail
            ),
            abolish(M:P,A), 
            fail 
        ;   true 
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SETUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% reverse a list.
rev(L1,L2):- rev(L1,L2,[]).
rev([],L,L).
rev([H|T],L,S):- rev(T,L,[H|S]).

changedir(D):- getdir(D,Dir),catch(chdir(Dir),_,write('SICStus')).
getdir(D,Dir):- name(D,L),rev(L,R),eraseLastPart(R,Rdir),rev(Rdir,Ldir),name(Dir,Ldir).
eraseLastPart([47|L],[47|L]):- !.
eraseLastPart([_|L],R):- eraseLastPart(L,R).

fasiller_default:- retractall(fasiller_depth(_)), retractall(fasiller_goal(_)), retractall(fasiller_ismode(_)),
	retractall(loaded_file_fpl(_)),retractall(loaded_file_pl(_)),retractall(loaded_file_lat(_)),
	assert(fasiller_depth(12)), 
	% intro('p(X)'), 
	assert(fasiller_ismode('m')), assert(sim_tnorm(luka)),
	assert(sim_tconorm(luka)).

% ?-prolog_load_context(source,Name),write('Name: '),write(Name),nl,nl,changedir(Name), lat:consult('num.lat'), 
  ?-use_module(library(lists)), %% This package defines operations on lists.
  use_module(library(system)), %% This package contains utilities for invoking services from the operating system.
  use_module(library(charsio)), %% This package defines I/O predicates that read from, or write to, a code-list.
  % fasiller_default, 		%% Default setting	
  assert(loaded_file_lat('num.lat')),retractall(parsingGoal),assert(fasiller_lcut(0)),
  retractall(rule(_,_,_,_,_)), retractall(state(_,_)), retractall(sim:r(_,_,_)).
  % title.