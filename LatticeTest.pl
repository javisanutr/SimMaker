 frontier_top(Aggr) :-
    write('Frontier Top: '),lat:top(T),test_idemp(Aggr,T),write('Success\n').
    
frontier_bot(Aggr) :-
    write('Frontier Bot: '),lat:bot(B),test_idemp(Aggr,B),write('Success\n').
    
increasing(Aggr,S) :-
    writeln('Increasing:\n'),growth_test(Aggr,test_in1,test_in2,S).
    
non_increasing(Aggr,S) :-
	writeln('Non increasing:\n'),growth_test(Aggr,test_nin1,test_nin2,S).
    
decreasing(Aggr,S) :-
	writeln('Decreasing:\n'),growth_test(Aggr,test_de1,test_de2,S).
    
non_decreasing(Aggr,S) :-
	writeln('Non decreasing:\n'),growth_test(Aggr,test_nde1,test_nde2,S).
    
switchness(Aggr1,Aggr2) :-
    write('Switchness: '),test_sw(Aggr1,Aggr2),writeln('Success').
    
associativity(Aggr) :-
    write('Associativity: '),test_sw(Aggr,Aggr),writeln('Success').
    
monotony(Aggr) :-
	writeln('Monotony:\n'),test_mono(Aggr).
    
adjointness(Aggr1,Aggr2) :-
	( test_adj(Aggr1,Aggr2) -> writeln('\nAdjointness: Success') ; writeln('\nAdjointness: Failure'),fail).

idempotency(Aggr) :-
    write('Idempotency: '),test_idemp_all(Aggr),writeln('Success').

commutativity(Aggr) :-
    write('Commutativity: '),test_com(Aggr),writeln('Success').
    
distributivity(Aggr1,Aggr2,S) :-
    write('Distributivity:\n\n'),test_distr(Aggr1,Aggr2,S).
    
t_norm(Aggr) :- 
    ( test_tnorm(Aggr) -> writeln('\nT-NORM: SUCCESS') ; writeln('\nT-NORM: FAILURE\n'),fail).
    
t_conorm(Aggr) :-
    ( test_tconorm(Aggr) -> writeln('\nT-CONORM: SUCCESS') ; writeln('\nT-CONORM: FAILURE\n'),fail).
    
implication(Aggr) :-
     writeln('Implication:\n'),test_imp(Aggr).   
	 
aggregator(Aggr) :-
	writeln('Aggregator:\n'),test_aggr(Aggr).
    
valid_distance(Aggr) :- 
	write('d(X,Y) >= 0: '),test_check_dist1(Aggr),writeln('Success'),
    write('d(X,X) == 0: '),test_check_dist2(Aggr),writeln('Success'),
    write('d(X,Y) == d(Y,X): '), test_check_dist3(Aggr),writeln('Success'),
    write('d(X,Z) <= d(X,Y) + d(Y,Z): '),test_check_dist4(Aggr),writeln('Success'),
    writeln('\nIt is a valid distance').
    
supremum_and_infimum(H,T,Mod) :-
    supr_inf(H,T,Mod).

 

% Extract an element from a given list 
extract([X|_],X).
extract([_|T],X):- extract(T,X).

% Get all the triplets (X,Y,Z) where X < Y and X != Y
getXltY(L) :- findall((X,Y,Z),(lat:members(L),extract(L,X),extract(L,Y),extract(L,Z),lat:leq(X,Y),X\=Y),L).

% Get all the pairs of three elements
getAllTriplet(L) :- findall((X,Y,Z),(lat:members(L),extract(L,X),extract(L,Y),extract(L,Z)),L).

% Round a float 
round(X,Y) :- Y is round(X*10^6)/10^6.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					BASICS				 	   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test the two growth tests in both parameters using the given connective and change the semaphore color
% Both parameters are successful -> Green
% One parameter is sucessful -> Orange
% Both parameters fail -> Red 
growth_test(Aggr,Test1,Test2,S):- 
                                do_test(Aggr,Test1) ->
                                  % Test1 True
                                  (writeln('First parameter: Success\n'),send(S,fill_pattern,orange),do_test(Aggr,Test2),
								   writeln('Second Parameter: Success\n'),send(S,fill_pattern,green)
								   )
                                ; % Test1 False
                                  (	do_test(Aggr,Test2) 
									-> writeln('Second Parameter: Success\n'),send(S,fill_pattern,orange) 
									; send(S,fill_pattern,red)
								  ).

% Get all the triplets (X,Y,Z) and do the growth test given
do_test(Aggr,Test) :- getXltY(L),forall(member((X,Y,Z),L),call(Test,X,Y,Z,Aggr)).


% INCREASING

% Increasing on the first parameter. If it fails, the counterexample will be displayed
% If X < Y => $(X,Z) < $(Y,Z)
test_in1(X,Y,Z,Aggr ):-
                      (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V1,V2),V1\=V2) 
                      ; (writef('First parameter: Failure\nCounterexample:\n%w(%w, %w) >= %w(%w,%w)\n\n', [Aggr,X,Y,Aggr,Y,Z]),fail).

% Increasing on the second parameter
% If X < Y => $(Z,X) < $(Z,Y)
test_in2(X,Y,Z,Aggr) :-
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V1,V2),V1\=V2) 
                        ; (writef('Second parameter: Failure\nCounterexample:\n%w(%w, %w) >= %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).


% NON-DECREASING

% Non-Decreasing on the first parameter
% If X < Y => $(X,Z) =< $(Y,Z)
test_nde1(X,Y,Z,Aggr) :- 
                            (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V1,V2)) 
                            ; (writef('First parameter: Failure\nCounterexample:\n%w(%w, %w) > %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Non-Decreasing on the second parameter
% If X < Y => $(Z,X) =< $(Z,Y)
test_nde2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V1,V2)) 
                        ; (writef('Second parameter: Failure\nCounterexample:\n%w(%w, %w) > %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).


% DECREASING

% Decreasing on the first parameter 
% If X < Y => $(X,Z) > $(Y,Z)
test_de1(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V2,V1),V1\=V2) 
                        ; (writef('First parameter: Failure\nCounterexample:\n%w(%w, %w) =< %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Decreasing on the second parameter 
% If X < Y => $(Z,X) > $(Z,Y)
test_de2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V2,V1),V1\=V2) 
                        ; (writef('Second parameter: Failure\nCounterexample:\n%w(%w, %w) =< %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).
                        
                        
% NON INCREASING

% Non-Increasing on the first parameter 
% If X < Y => $(X,Z) >= $(Y,Z)
test_nin1(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,X,Z,V1),call(lat:Aggr,Y,Z,V2),lat:leq(V2,V1)) 
                        ; (writef('First parameter: Failure\nCounterexample:\n%w(%w, %w) < %w(%w,%w)\n\n', [Aggr,X,Z,Aggr,Y,Z]),fail).

% Non-Increasing on the second parameter 
% If X < Y => $(Z,X) >= $(Z,Y)
test_nin2(X,Y,Z,Aggr) :- 
                        (call(lat:Aggr,Z,X,V1),call(lat:Aggr,Z,Y,V2),lat:leq(V2,V1)) 
                        ; (writef('Second parameter: Failure\nCounterexample:\n%w(%w, %w) < %w(%w,%w)\n', [Aggr,Z,X,Aggr,Z,Y]),fail).
                        

% IDEMPOTENCY
% $(X,X) == X

test_idemp_all(Aggr) :- lat:members(L),forall(member(X,L),test_idemp(Aggr,X)).

test_idemp(Aggr,X) :- ( call(lat:Aggr,X,X,V),X==V 
						-> true 
						; writef('Failure\nCounterexample:\n%w(%w,%w) =\\= %w\n',[Aggr,X,X,X]),fail
					  ).


% COMMUTATIVITY
% $(X,Y) == $(Y,X)

test_com(Aggr) :- 
                  findall((X,Y),(lat:members(L),extract(L,X),extract(L,Y)),L),
                  forall(member((X,Y),L),(call(lat:Aggr,X,Y,V1),call(lat:Aggr,Y,X,V2),V1==V2
                  ;  fail_com(Aggr,X,Y))
                  ).
                  
fail_com(Aggr,X,Y) :- writef('Failure\nCounterexample:\n%w(%w,%w) =\\= %w(%w,%w)\n',[Aggr,X,Y,Aggr,Y,X]),fail. 

% MONOTOMY
% Non decreasing in both parameters

test_mono(Aggr) :- writeln('Non decreasing:\n'),do_test(Aggr,test_nde1),writeln('First parameter: Success\n'),do_test(Aggr,test_nde2),writeln('Second parameter: Success\n').
						
						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			COMBINED PROPERTIES				   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% T-NORM
% Aggregator is and, is commutative, associative, monotone, and identity element is top

test_tnorm(Aggr) :- lat:top(T),identity_element(Aggr,T,tnorm),print_connective(Aggr,tnorm),commutativity(Aggr),associativity(Aggr),monotony(Aggr).

%Type is t-norm or t-conorm
identity_element(Aggr,E,Type) :- writef('Identity element %w: ',[E]),forall(lat:member(X),test_iden(Aggr,X,E,Type)),writeln('Success').
test_iden(Aggr,X,E,Type) :- 
            ( call(lat:Aggr,X,E,V),X==V 
                -> true 
                ; writef('Failure\nCounterexample:\n%w(%w,%w) =\\= %w\n',[Aggr,X,E,X]),print_connective(Aggr,Type,neg),fail 
            ).

            
print_connective(Aggr,tnorm) :- writef('%w is a T-Norm connective\n',[Aggr]).
print_connective(Aggr,tconorm) :- writef('%w is a T-Conorm connective\n',[Aggr]).
print_connective(Aggr,tnorm,neg) :- writef('%w is not a T-Norm connective',[Aggr]).
print_connective(Aggr,tconorm,neg) :- writef('%w is not a T-Conorm connective',[Aggr]).


% T-CONORM
% Aggregator is or, is commutative, associative, monotone, and identity element is top

test_tconorm(Aggr) :- lat:bot(B),identity_element(Aggr,B,tconorm),print_connective(Aggr,tconorm),commutativity(Aggr),associativity(Aggr),monotony(Aggr).                
                
% IMPLICATION
% Increasing in the first parameter and decreasing in the second one

test_imp_1(Aggr) :- lat:members(L),lat:bot(B),lat:top(T),writef('%w(Y, %w) = %w: ',[Aggr,B,T]),forall(member(Y,L),(call(lat:Aggr,Y,B,T); writef('Failure\nCounterexample:\n%w(%w, %w) =\\= %w',[Aggr,Y,B,T]),fail)),write('Success\n').

test_imp_2(Aggr) :- lat:members(L),lat:top(T),writef('%w(Y, %w) = Y: ',[Aggr,T]),forall(member(Y,L),(call(lat:Aggr,Y,T,Y); writef('Failure\nCounterexample:\n%w(%w, %w) =\\= %w',[Aggr,Y,T,Y]),fail)),write('Success\n').

test_imp_3(Aggr) :- writef('%w(%w(Z, X), Y) = %w(%w(Z, Y), X): ',[Aggr,Aggr,Aggr,Aggr]),
					getAllTriplet(L),
					forall(member((X,Y,Z),L), 
						(	call(lat:Aggr,Z,X,U1),call(lat:Aggr,U1,Y,V1),
							call(lat:Aggr,Z,Y,U2),call(lat:Aggr,U2,X,V2),V1==V2 
							; writef('Failure\nCounterexample:\n%w(%w(%w, %w), %w) =\\= %w(%w, %w(%w, %w))',[Aggr,Aggr,Z,X,Y,Aggr,Aggr,Z,Y,X]),fail
						)
					),write('Success\n').

test_imp(Aggr) :- writeln('Non-Decreasing:\n'),do_test(Aggr,test_nde1),writeln('First parameter: Success\n'),writeln('Non-Increasing:\n'),do_test(Aggr,test_nin2),writeln('Second parameter: Success\n'),test_imp_1(Aggr),test_imp_2(Aggr),test_imp_3(Aggr).


% AGGREGATOR
% Frontier bot, top and monotony

test_aggr(Aggr) :- frontier_top(Aggr),frontier_bot(Aggr),monotony(Aggr).

						
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					MULTIPLES				  	%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

						
% SWITCHNESS
% $1($2(X,Y),Z) == $2(X,$1(Y,Z))

test_sw(Aggr1,Aggr2) :- 
                    getAllTriplet(L),
                    forall(member((X,Y,Z),L),
								(
									% $1($2(X,Y),Z) 
									call(lat:Aggr2,X,Y,U1),call(lat:Aggr1,U1,Z,V1),
									% $2(X,$1(Y,Z))
									call(lat:Aggr1,Y,Z,U2),call(lat:Aggr2,X,U2,V2),
									V1==V2
									;   fail_sw(Aggr1,Aggr2,X,Y,Z)
								)
						   ).

% Counterexample message
fail_sw(Aggr1,Aggr2,X,Y,Z) :- writef('Failure\nCounterexample:\n%w(%w(%w,%w),%w) =\\= %w(%w,%w(%w,%w))\n',[Aggr1,Aggr2,X,Y,Z,Aggr2,X,Aggr1,Y,Z]),fail.

% DISTRIBUTIVITY

% First parameter
% $2($1(X,Y),Z) == $1($2(X,Z),$2(Y,Z))
test_distr1(Aggr1,Aggr2,X,Y,Z) :- call(lat:Aggr1,X,Y,V),call(lat:Aggr2,V,Z,V1),
								  call(lat:Aggr2,X,Z,U1),call(lat:Aggr2,Y,Z,U2),call(lat:Aggr1,U1,U2,V2),
								  V1==V2 
                                  ; fail_distr1(Aggr1,Aggr2,X,Y,Z).
                                  
fail_distr1(Aggr1,Aggr2,X,Y,Z) :- writef('First parameter: Failure\nCounterexample: %w(%w(%w,%w),%w) =\\= %w(%w(%w,%w), %w(%w,%w))\n\n',[Aggr2,Aggr1,X,Y,Z,Aggr1,Aggr2,X,Z,Aggr2,Y,Z]),fail.

% Second parameter
% $2(X,$1(Y,Z)) == $1($2(X,Y),$2(X,Z))
test_distr2(Aggr1,Aggr2,X,Y,Z) :- call(lat:Aggr1,Y,Z,V),call(lat:Aggr2,X,V,V1),
								  call(lat:Aggr2,X,Y,U1),call(lat:Aggr2,X,Z,U2),call(lat:Aggr1,U1,U2,V2),
								  V1==V2 
                                  ; fail_distr2(Aggr1,Aggr2,X,Y,Z).
                                  
fail_distr2(Aggr1,Aggr2,X,Y,Z) :- writef('Second parameter: Failure\nCounterexample: %w(%w(%w,%w),%w) =\\= %w(%w(%w,%w), %w(%w,%w))\n\n',[Aggr2,X,Aggr1,Y,Z,Aggr1,Aggr2,X,Y,Aggr2,X,Z]),fail.


% Execute the distributivity tests in both parameters and change the semaphore color
% Both parameters are successful -> Green
% One parameter is sucessful -> Orange
% Both parameters fail -> Red 
test_distr(Aggr1,Aggr2,S) :-
                            (
                                getAllTriplet(L),
                                forall(member((X,Y,Z),L),test_distr1(Aggr1,Aggr2,X,Y,Z))
                            ->
								% Test1 Success
                                writeln('First parameter: Success\n'),send(S,fill_pattern,orange),forall(member((X,Y,Z),L),test_distr2(Aggr1,Aggr2,X,Y,Z)),writeln('Second parameter: Success\n'),send(S,fill_pattern,green)
                            ;
								% Test1 Fail
                                ( getAllTriplet(L),forall(member((X,Y,Z),L),test_distr2(Aggr1,Aggr2,X,Y,Z)) 
								->  writeln('Second parameter: Success\n'),send(S,fill_pattern,orange)
								;   send(S,fill_pattern,red)
								)
                            ).
                            
% ADJOINTNESS

% First connective is a t-norm, the second one is an implication and both verifies that:
% X <= $2(Y,Z) <==> $1(X,Z) <= Y
test_adj(Aggr1,Aggr2) :- t_norm(Aggr1),implication(Aggr2),adjoint(Aggr1,Aggr2).


% X <= $2(Y,Z) <==> $1(X,Z) <= Y
adjoint(Aggr1,Aggr2) :- 
                            getAllTriplet(L),
                            forall(member((X,Y,Z),L),
                                (
                                    call(lat:Aggr2,Y,Z,V1),
                                    call(lat:Aggr1,X,Z,V2),
                                    bicond(Aggr1,Aggr2,X,Y,Z,V1,V2)
                                )
                            ).

% Biconditional				
bicond(Aggr1,Aggr2,X,Y,Z,V1,V2) :- ( lat:leq(X,V1),lat:leq(V2,Y) 
                                    -> true
                                    ; ( not(lat:leq(X,V1)),not(lat:leq(V2,Y)) 
                                        -> true
                                        ; writef("Failure\nCounterexample:\n%w <= %w(%w, %w) <=\\=> %w(%w, %w) <= %w\n",[X,Aggr2,Y,Z,Aggr1,X,Z,Y]),fail
                                      )
                                    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			   DISTANCE CHECK			   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%d(X,Y) >= 0
test_check_dist1(Aggr) :- lat:members(M),
						  forall(member(X,M), 
										(greater(X,L),
										 forall(member(Y,L),
													(call(lat:Aggr,X,Y,V),round(V,D), R is D * 1.0,R >= 0 ; fail_dist(Aggr,X,Y))
											    )
									    )
								).

% d(X,X) == 0
test_check_dist2(Aggr) :- lat:members(M),forall(member(X,M),(call(lat:Aggr,X,X,V),round(V,D),R is D*1.0, R == 0.0 ; fail_dist(Aggr,X))).

%d(X,Y) == d(Y,X)
test_check_dist3(Aggr) :- findall((X,Y),(lat:members(L),extract(L,X),extract(L,Y)),L),
						  forall(member((X,Y),L),(call(lat:Aggr,X,Y,V1),call(lat:Aggr,Y,X,V2),round(V1,R1), round(V2,R2), R1 =< R2
						  ;  fail_dist3(Aggr,X,Y))
						  ).

% d(X,Z) <= d(X,Y) + d(Y,Z)
test_check_dist4(Aggr) :- triplet(L),forall(member((X,Y,Z),L),((call(lat:Aggr,X,Z,V1),sum(Aggr,X,Y,Z,V2),round(V1,R1), round(V2,R2), R1 =< R2) ; fail_dist(Aggr,X,Y,Z) )).

% Get all the triplet (X,Y,Z) where X is the initial element, Z is the final element, and Y is the intermediate element
triplet(L) :- findall((X,Y,Z),(lat:members(LX),extract(LX,X),greater(X,LZ),extract(LZ,Z),inter(X,Z,LY),extract(LY,Y)),L).

% Get all the intermediate elements between X and Z
inter(X,Z,Y) :- findall(E, (lat:members(M),extract(M,E),lat:leq(X,E),lat:leq(E,Z)),Y).

% Get all the elements greater or equal than X 
greater(X,Z) :- setof(E,(lat:members(M),extract(M,E),lat:leq(X,E)),Z).

% Addition of distances
sum(Aggr, X,Y,Z,V) :- call(lat:Aggr,X,Y,V1),call(lat:Aggr,Y,Z,V2),V is V1+V2.

fail_dist(Aggr,X) :- writef('\nFailure\nCounterexample\n%w(%w, %w) =\\= 0\n',[Aggr,X,X]),fail.
fail_dist(Aggr,X,Y) :- writef('\nFailure\nCounterexample\n%w(%w, %w) < 0\n',[Aggr,X,Y]),fail.
fail_dist3(Aggr,X,Y) :- writef('\nFailure\nCounterexample\n%w(%w, %w) =\\= %w(%w, %w)\n',[Aggr,X,Y,Aggr,Y,X]),fail.
fail_dist(Aggr,X,Y,Z) :- writef('\nFailure\nCounterexample\n%w(%w,%w) > %w(%w,%w) + %w(%w,%w)\n',[Aggr,X,Z,Aggr,X,Y,Aggr,Y,Z]),fail. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			SUPREMUMS AND INFIMUMS			   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check if X is minor than the rest of elements in L
is_min(X,L,Mod) :- forall(member(Y,L),Mod:leq(X,Y)).

% Find the minimum upper bound, Min, in L  
min_supremum_list(L,Min,Mod) :- setof(X,(extract(L,X),is_min(X,L,Mod)),Min). 

% Get all the upper bounds for X,Y.
list_supr(X,X,[],_).
list_supr(X,Y,L,Mod) :- Mod:members(M),setof(E,(extract(M,E),Mod:leq(X,E),Mod:leq(Y,E)),L).

% Return the supremum of X and Y
supremum(X,Y,S,Mod) :- X \= Y,list_supr(X,Y,L,Mod),min_supremum_list(L,[S],Mod). 


% Check if one element is major than the rest of elements in the list
is_max(X,L,Mod) :- forall(member(Y,L),Mod:leq(Y,X)).

% Find the maximum lower bound in the list  
max_infimum_list(L,Max,Mod) :- setof(X,(extract(L,X),is_max(X,L,Mod)),Max).  

% Get all the lower bounds for X,Y 
list_inf(X,X,[],_).
list_inf(X,Y,L,Mod) :- Mod:members(M),setof(E,(extract(M,E),Mod:leq(E,X),Mod:leq(E,Y)),L).

% Return the infimum for X and Y
infimum(X,Y,I,Mod) :- X \= Y,list_inf(X,Y,L,Mod),max_infimum_list(L,[I],Mod). 

% Check if all the elements in the List has supremum and infimum, displaying a message for those wich don't have
supr_inf(X,List,Mod) :-
            findall((X,Y),(extract(List,Y),X \= Y),L),
            forall(member((X,Y),L),
                (
                    (supremum(X,Y,_,Mod) ; fail_sup(X,Y)),
                    (infimum(X,Y,_,Mod) ; fail_inf(X,Y))
                )
            ).

			
fail_sup(X,Y) :- writef('IMPORTANT ERROR:\n supremum(%w,%w) does not exist\n',[X,Y]).
fail_inf(X,Y) :- writef('IMPORTANT ERROR:\n infimum(%w,%w) does not exist\n',[X,Y]).


% Check if one element has supremum and infimum
supr_inf_one(X,Mod) :- Mod:members(M),
			delete(M,X,L),
            forall(member(Y,L),
                (
                    supremum(X,Y,_,Mod),infimum(X,Y,_,Mod)
                )
            ).