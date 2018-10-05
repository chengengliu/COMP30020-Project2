% You can use this code to get started with your fillin puzzle solver.
% Make sure you replace this comment with your own documentation.

:- ensure_loaded(library(clpfd)).
:- use_module(file_reader).

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved).
	%print_puzzle(SolutionFile, Solved).

%% Simple output predicate, 
write_test(File, Text):-
    open(File, write, Stream),
    write(Stream, Text), 
    close(Stream).

/**
 * Now it is time to restate the whole algorithm
 * The algorithm should be 
 * 1. Replace all underscores with logical variables. 
 * 2. Build a list of slots, where each slot is a list of logical var
 * representing a single square in the puzzle. This step is important 
 * if the same var is used for the same slot in either hot or ver when
 * unifying the var, it will be correctly unified hor and ver
 * 3. Fill in words in to the puzzle until solved. 
 * Selecting a slot and a word and unifying the word with the slot 
 * and recursing. Should be careful when choosing the best slot and word. 
 * 
*/

solve_puzzle(Puzzle, _, FilledPuzzle):-
    fill_with_vars(Puzzle,FilledPuzzle),
    write_test('result1.txt', FilledPuzzle).


% FilledPuzzle is the same puzzle with the input puzzle, except thst 
% All '_' will be replaced by logical variable. 
fill_with_vars([],[]).
fill_with_vars(Puzzle, FilledPuzzle):-
    maplist(fill_row_with_vars, Puzzle, FilledPuzzle).
% Row is a row of an unfilled puzzle, and FilledRow is filled with 
% Logical variables. 
fill_row_with_vars([],[]).
fill_row_with_vars(Row, FilledRow):-
    maplist(fill_underscore_var, Row, FilledRow).

% Replace all undersocres with logical variables and keep everything else
% The same. 
fill_underscore_var('_', _).
fill_underscore_var(Char, Char):- Char \= '_'.









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: Need to change to tail recursive to improve efficiency. 
qsort([],[]).
qsort([H|T], Sorted):-
    qpartition(H, T, Less, Greater),
    qsort(Less, SortedLess),
    qsort(Greater, SortedGreater),
    append(SortedLess, [H|SortedGreater], Sorted).


qpartition(_,[],[],[]).
qpartition(Pivot, [E|Rest], [E|Less], Greater):-
    E =< Pivot,
    qpartition(Pivot, Rest, Less, Greater).
qpartition(Pivot, [E|Rest], Less, [E|Greater]):-
    E > Pivot,
    qpartition(Pivot, Rest, Less, Greater).


/**This is the print out part.  
 * TODO: Maybe later separated into another file.
 * 
 * 
 * */
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).
% solve_puzzle(Puzzle0, WordList, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from WordList.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  WordList is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.