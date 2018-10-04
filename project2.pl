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

solve_puzzle(Puzzle, _, Puzzle):-
    convert_variable(Puzzle,FilledPuzzle),
    write_test('result1.txt', FilledPuzzle).

% TODO: 首先要把 puzzle 转换成variable 填充进去。 
% Prepare the pusslze with logical variable.
%  填充的时候是按照行的顺序吧？？？大概。 
% TODO: 10/4 ntoes: It seems works fine when checking 
% main('test2.txt','words1.txt','result1.txt'). with test files. 
%%% Test Line : [[’#’,’_’,’#’], [’_’,’_’,’_’], [’#’,’_’,’#’]]
convert_variable(Puzzle, FilledPuzzle):-
    convert_variable(Puzzle, [], FilledPuzzle).
convert_variable([], Acc, Acc).
convert_variable([H|Rest], Acc, FilledPuzzle):-
    horizontal_variable(H,RowVar),
    append(Acc, [RowVar], NewAcc),
    convert_variable(Rest, NewAcc, FilledPuzzle).

horizontal_variable(R, Filled):-
    horizontal_variable(R,[], Filled).
horizontal_variable([],Acc, Acc).
%  如果遇到了‘#’ 那么直接append， 遇到了’_‘再加var。
horizontal_variable([H|Rest], Acc, Filled):-
    % An underscore means we need to add a free var. 
    (H = '_'
        ->
        FreeSlot = var(_);
        % Just add '#' when see it. Append it to the NewAcc. 
        H = '#'
        ->  % Else we put an extra variable into the new Accumulator. 
        append(Acc, [H], NewAcc);
        FreeSlot = var(H)
    ),
    append(Acc, [FreeSlot], NewAcc),
    horizontal_variable(Rest, NewAcc, Filled).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 * Now it is the job to get the free position from the puzzle. 
 * Meanwhile it is important to do the 90 degree rotation . 
 * This is done by 
 * 1. Look through each row (horizontally)
 * 2. Transposed the puzzle(Rotate 90 degree). After doing so, there will be some 
 * common variables that are being used by both rows and columns. 
 * */

get_all_slots(FilledPuzzle, ListOfSlots):-
    get_slots(FilledPuzzle, HorizontalSlots),
    transpose(FilledPuzzle, VerticalPuzzle),
    get_slots(VerticalPuzzle, VerticalSlots),
    append(HorizontalSlots, VerticalSlots).


/**
 * Take a pusslze that is already filled with logical variables (FilledPuzzle)
 * and firstly check the rows. 
*/
get_slots(FilledPuzzle, FilledSlots):-
    get_slots(FilledPuzzle,[], FilledSlots).
% Base case. No rows left. 
get_slots([], Acc, Acc).
% H is a single row. 
get_slots([H|Rest], Acc, FilledSlots):-
    get_slots_oneRow(H, TempFilledSlots),
    % TODO: What happened if the row has no slots? 
    %%%% Create a new variable .
    append(Acc, TempFilledSlots, NewFilledSlots),
    get_slots(Rest, NewFilledSlots, Slots).


/**
 * Get slots from one row. 
 * Slots : List of slots for the row. 
 * 
*/
get_slots_oneRow(Row, Slots):-
    















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