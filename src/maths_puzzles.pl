%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  File     : maths_puzzles.pl
%  Author   : Ivan Ken Weng Chee
%  Origin   : Sunday 15 October 2017

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Holds when Puzzle is the representation of a solved maths puzzle
% Arg1 : Puzzle
puzzle_solution(P) :- valid_puzzle(P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checks if a Puzzle representation is valid or not
% First checks for same-value diagonals
% Then checks if all rows are valid
% Finally transposes the puzzle and checks if all columns are valid
% Arg1 : Puzzle
valid_puzzle(P) :-
  P = [_|Ps],
  same_diagonals(Ps),
  valid_rows(Ps),
  transpose(P, Q),
  Q = [_|Qs],
  valid_rows(Qs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checks through rows and columns of the puzzle
% Arg1 : Puzzle
valid_rows([]).
valid_rows([H|T]) :-
  valid_row(H),
  valid_rows(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Checks that a row/column contains no repeated digits,
% has values between 1-9, and whether the heading of each row/column
% holds either the sum or product of all digits in that row/column
% Arg1 : List
valid_row([]).
valid_row([H|T]) :-
  all_distinct(T),
  maplist(between(1,9), T),
  (sum_of_list(T, H); product_of_list(T, H)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Holds when the sum of all elements in the list equals the value S
% Arg1 : List
% Arg2 : Expected sum of all elements
sum_of_list([], 0).
sum_of_list([H|T], S) :-
  S #= S1 + H,
  sum_of_list(T, S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Holds when the product of all elements in the list equals the value P
% Arg1 : List
% Arg2 : Expected product of all elements
product_of_list([], 1).
product_of_list([H|T], P) :-
  P #= P1 * H,
  product_of_list(T, P1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Holds when all diagonals of a puzzle are the same
% Arg1 : Puzzle
same_diagonals(P) :-
  P = [H|_],
  nth0(1, H, D),
  check_diagonals(P, D, 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper function used by same_diagonals()
% Arg1 : Puzzle
% Arg2 : Value of first diagonal
% Arg3 : Index of diagonal
check_diagonals([], _, _).
check_diagonals([H|T], D, I) :-
  nth0(I, H, D),
  I1 is I + 1,
  check_diagonals(T, D, I1).
