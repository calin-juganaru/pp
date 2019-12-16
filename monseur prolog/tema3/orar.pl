:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').

% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.

is_empty([]).
is_empty([_|_]) :- fail.

anything_empty([days(A), times(B), rooms(C), groups(D), activities(E), staff(F)])
	:- is_empty(A); is_empty(B); is_empty(C); is_empty(D); is_empty(E); is_empty(F).

is_staff(Activities, Staff)
	:- forall(member((X, _), Activities), (member((_, Y), Staff), member(X, Y))).

hours([], 0).
hours([(_, A)|Activities], Result) :- hours(Activities, Rest), Result is A + Rest.

is_time(Days, Times, Activities)
	:- length(Days, D), length(Times, T), hours(Activities, A), A @=< D * T.

is_room(Days, Times, Groups, Rooms, Activities)
	:- length(Days, D), length(Times, T), length(Groups, G), length(Rooms, R),
	   hours(Activities, A), T * D * R @>= G * A.

conditions([days(Days), times(Times), rooms(Rooms), groups(Groups),
		   activities(Activities), staff(Staff)])
	:-	is_staff(Activities, Staff),
		is_time(Days, Times, Activities),
		is_room(Days, Times, Groups, Rooms, Activities).

generate(Days, Times, Rooms, Groups, Activities, Staff, Result)
	:- 	findall( slot(A, G, D, T, R, S),
		(
			member(D, Days), member(T, Times),
			member(R, Rooms), member(G, Groups),
			member((A, _), Activities),
			member((S, SA), Staff),
			member(A, SA)
		), Result ).

aux(List, N, [Elem|Rest]) :- N > 1, select(Elem, List, R), N1 is N - 1, aux(R, N1, Rest).
aux(List, 1, [Elem]) :- member(Elem, List).

arrangements(List, N, Result) :- findall(X, aux(List, N, X), Result).

/*
not_overlap(slot(_, G1, D1, T1, R1, S1), slot(_, G2, D2, T2, R2, S2))
	:- (G1 \= G2; D1 \= D2; T1 \= T2).%(R1 == R2; S1 == S2).

good_arrangement([_]) :- false.
good_arrangement([Slot|Slots]) :- member(X, Slots), not_overlap(Slot, X).%, good_arrangement(NewSlot, Slots).
*/

/*
delete([], []).
delete([X|Xs], Ys)
	:-
    (
		good_arrangement(X, Xs) ->
		Ys = [X|Ys2];
		Ys = Ys2
    ),
	delete(Xs, Ys2).
*/

find_one([Days, Times, Rooms, Groups, Activities, Staff], Result)
	:-	random_permutation(Days, Ds), random_permutation(Times, Ts),
			random_permutation(Rooms, Rs), random_permutation(Groups, Gs),
			random_permutation(Activities, As), random_permutation(Staff, Ss),
		member(D, Ds), member(T, Ts), member(R, Rs), member(G, Gs),
		member((A, _), As), member((S, SA), Ss), member(A, SA),
		Result = [slot(A, G, D, T, R, S)].
	%good_arrangement(slot(A, G, D, T, R, S), Result),
	%R = Result, append([slot(A, G, D, T, R, S)], R, Result).

solutions([Days, Times, Rooms, Groups, Activities, Staff], Sol)
	:- 	%generate(Days, Times, Rooms, Groups, Activities, Staff, Result1),
		%length(Groups, G), hours(Activities, A), N is G * A,
		%arrangements(Result1, N, Result2),
		%delete(Result2, Result3),
		%exclude(good_arrangement, Result2, Result3),
		%findall(R, (member(G, Groups), find_one([Days, Times, Rooms, [G], Activities, Staff], R)), Result3),
		find_one([Days, Times, Rooms, Groups, Activities, Staff], Result3),
		Sol = Result3.

all_solutions([days(Days), times(Times), rooms(Rooms), groups(Groups),
			  activities(Activities), staff(Staff)], Sol)
	:- solutions([Days, Times, Rooms, Groups, Activities, Staff], Result), Sol = Result.

schedule(Context, Sol)
	:-
	(
	anything_empty(Context) ->
	Sol = (Context, []);
		(
			conditions(Context) ->
			all_solutions(Context, S),
			Sol = (Context, S);
			fail
		)
	).

% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
cost(_, 0).

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(Context, Sol, Cost) :- schedule(Context, S), Sol = S, Cost = 0.













