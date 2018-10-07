% COMP30020 2018 Declarative Programming Project 2 
% Student: Chengeng Liu     Student ID : 813174
% IO-code: Peter Schachte

:- ensure_loaded(library(clpfd)).
% TODO: file-level doc needs more. 
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

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 * solve_puzzle/3 takes Puzzle and Wordlist as two arguments and FilledPuzzle 
 * will be the solved solution for the puzzle. It will call fill_with_vars/2,
 * construct_slots/2 and fill_words/2. As mentioned above, the three steps 
 * algorithm is done by these three predicates. 
 * More details about each of the three predicates will be introduced before 
 * each predicate is used. 
 * 
*/
solve_puzzle(Puzzle, Wordlist, FilledPuzzle):-
    fill_with_vars(Puzzle,FilledPuzzle),
    construct_slots(FilledPuzzle,Slots),
    fill_words(Slots, Wordlist).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * Using a sequence of operations(like a pipeline) to traverse the puzzle 
 * to eliminate the undersocres '_'.
 * FilledPuzzle: is the same puzzle with the input puzzle but every undersocre 
 * will be replaced by a logical variable. 
 * Puzzle: input puzzle.
 * 
*/

fill_with_vars([],[]).
fill_with_vars(Puzzle, FilledPuzzle):-
    maplist(fill_row_with_vars, Puzzle, FilledPuzzle).


% Row is a row of an unfilled puzzle, and FilledRow is the row filled with 
% logical variables. 
fill_row_with_vars([],[]).
fill_row_with_vars(Row, FilledRow):-
    maplist(fill_underscore_var, Row, FilledRow).


% Replace all undersocres with logical variables and keep everything else that
% is not '_' the same.
fill_underscore_var('_', _).
fill_underscore_var(Ch, Ch):- Ch \= '_'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * construct_slots/2 will return a list of all slots of the puzzle. 
 * Slots: is a list of slots in both horizontal and vertical of the puzzle. 
 * FilledPuzzle: A list of chars that have been fileld with logical variables 
 * in the previous fill_with_vars/2. 
*/
construct_slots(FilledPuzzle, Slots):-
    rows_slots(FilledPuzzle, HorizontalSlots),
    transpose(FilledPuzzle, NewFilledPuzzle),
    rows_slots(NewFilledPuzzle, VerticalSlots),
    % The length of a slot has to be greated than one. 
    include(length_more_than1,HorizontalSlots,NewHorizontalSlots),
    include(length_more_than1,VerticalSlots,NewVerticalSlots),
    append(NewHorizontalSlots, NewVerticalSlots,Slots).


% Helper function. In order to justify if the length of a list is greater than
% one.  
length_more_than1(List):-
    length(List,Length),
    Length > 1.


% Slots: is a list of all the slots in all Rows of the puzzle. 
% This predicate can be reused later when we transpose the puzzle. 
% Rows: is a list of lists of Char of the puzzle. 
% Every row os slot of the puzzle will be stored in Slots. 
rows_slots([], []).
rows_slots([R|Rows], Slots):-
    one_row_slot(R, RowSlots),
    append(RowSlots,TempSlots, Slots),
    rows_slots(Rows, TempSlots).

% one_row_slot/2 will return Slots which contain a list of slots in the Row.
% Slots: will be the result of splitting the Row with '#'.
% one_row_slot/2 calls one_row_slot/3 with a tail recursion (initialising
%     the acc as []).
one_row_slot(Row, Slots):-
    one_row_slot(Row, [], Slots).


/**
 * one_row_slot/3 return the slots in one row. 
 * It uses Acc as an accumulator, recursively adding chars into it 
 * until it meets a '#'. When meeting a '#', it adds the Acc into Slots and 
 * resets Acc to be empty lists. 
*/
% Notice that if there is no '#' till the end of the row, then Acc is made into
% a list with itself. (This is another base case that needs to be handled.)
% This is different from the seoncd base case. Since the seoncd base case is 
% just the termination of one_row_slot/3
one_row_slot([], Acc, [Acc]):- 
    Acc \= [].
one_row_slot([],[],[]).
% If there is a '#', add the Acc to the list of slots and reset Acc to an empty
% list.
one_row_slot([H|Rest], Acc, Slots):-
    H == '#',
    Slots = [Acc|Slots1],
    one_row_slot(Rest,[], Slots1).
% If not meeting the '#', keep adding to Acc. 
% New slot appended after the previous one, in order to keep the correct 
% order. 
one_row_slot([H|Rest], Acc,Slots):-
    H \== '#',
    append(Acc,[H],Acc1),
    one_row_slot(Rest,Acc1,Slots).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * fill_words/2 is to fill a word in Wordlist to all slots in Slots. 
 * select_best_slot/3 will be called to find the best slot to fill in words. 
 * Each time we filled one slot with one word, repeat fill_words/2 with the 
 * rest of the slots. The word that has been filled will not be filled again. 
 * The bset slot is the slot that has the fewest matching words in order to 
 * cut off the seatch space. More about selecting will be introduced later of 
 * the predicate select_best_slot/3.
 * Slots: a list of slots in the puzzle. 
 * Wordlist: a list of words being filled into the slots. 
*/

fill_words([],[]).
fill_words(Slots, Wordlist):-
    select_best_slot(Slots, Wordlist, Best),
    exclude(\=(Best), Wordlist, Match),
    member(Word, Match),
    Best = Word,
    % Delete the choosen slot and the word that has been filled.
    % Repeat selecting until Slots is empty. 
    exclude(==(Word), Wordlist, RestOfWords),
    exclude(==(Best), Slots, RestOfSlots),
    fill_words(RestOfSlots, RestOfWords).
    

/**
 * select_best_slot/3 will return the best slot, which is defined as the least
 * amount of matching words that can be used to fill(this will reduce the 
     search space and backtrack space)
 * The predicate will use select_best_slot/5, which will try to unify a slot
 * with a word. (A word is said to be 'match' only when the word could unify
     the slot).More explaination will be introduced later.
 * [H|Rest]: is a list of slots in the puzzle. 
 * Wordlist: is a list words being filled into the puzzle. 
 * Best: is the slot with the fewest matching words in the Wordlist.
 * 
*/
select_best_slot([H|Rest], Wordlist, Best):-
    % count_/3 will return Number as the number of the matching words.
    % Slot here is considered as the 'temp'-best slot so far. It will
    % be passed to select_best_slot/5 to test if it is the real best 
    count_(H, Wordlist, MatchNumber),
    select_best_slot(Wordlist,MatchNumber, Rest, H, Best).


/**
 * count_/3 will call count_/4 with a Acc.
 * The wordlist will be traversed to check if there is one word that can 
 * be fit into the slot. 
 * MatchNumber: is the number of words in the wordlist that can fit in the slot. 
 * Slot: is a slot in the puzzle. 
 * Wordlist: is a list of words being filled into the puzzle. 

*/
count_(Slot, Wordlist, MatchNumber):-
    count_(Slot, Wordlist, 0, MatchNumber).

/**
 * Acc is the accumulator. The Wordlist is being traversed to check all the 
 * wordlist.
 * Slot: is a slot of the puzzle. 
 * MatchNumber: the number of matching words of the slot. 
*/
count_(_,[], Acc, Acc).
count_(Slot, [W|Wordlist], Acc, MatchNumber):-
    % If the Slot can't be unified by W, keep the number unincreased. 
    % Otherwise increment by one. 
    (Slot \= W->
        Acc1 is Acc;
        Acc1 is Acc+1
    ),
    count_(Slot, Wordlist, Acc1, MatchNumber).


/**
 * select_best_slot(Wordlist, MatchNumber, Slots, CBest, Best) is to compute
 * the best slot. This is the main algorithm
 * select_best_slot/5 will also use count/3 to traverse the wordlist to find
 * the MatchNumber. It then compute the best slot by selecting the slot 
 * with the fewest matching words. 
 * MatchNumber is the number of matching words that the CurrentBest has
 * If Count < MatchNumber(which the CurrentBest has), then a better slot
 * that has fewer matching words is found. Change the CurrentBest and the 
 * MatchNumber then. 
*/
select_best_slot(_,_, [], Best, Best).
select_best_slot(Wordlist, MatchNumber, [S|Slots], CurrentBest, Best):-
    count_(S, Wordlist, Count),
    (Count >= MatchNumber->
        CurrentBest1 = CurrentBest, 
        MatchNumber1 = MatchNumber;
        CurrentBest1= S,
        MatchNumber1 = Count
    ),
    select_best_slot(Wordlist, MatchNumber1, Slots, CurrentBest1, Best).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IO part. Printing predicates and Reading predicates. 
% These predicates are provided from starter.pl written by Peter Schachte

/**
 * print_puzzle/2 
 * SolutionFile: name of the file that is being printed to. 
 * Puzzle: this is passed as Solved when print_puzzle/2 is called. This will 
 * ensure that the whole Puzzle(Solved) to the SolutionFile. 
*/
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

/**
 * print_row/2 will write the Row to Stream that is provided. 
 * Stream: an opened file in read mode 
 * Row: a row fo the solved puzzle. 
*/
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).
/**
 * put_puzzle_char/2 will write '_' to the Stream if Char is a var. 
 * If Char is not a var, it will write the Char itself to the Stream. 
 * Stream: an opened file in read mode
 * Char: a char
*/
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).
/**
 * valid_puzzle/1 verifies that each row in the puzzle has the same 
 * length. 
 * Rows: a list of lists and each of the list has the same length. 
*/
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).

/**
 * samelength/2 verifies two lists have the same length. 
*/
samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).

/**
 * read_file/2 will read all contents from Filename line by line. 
 * Filename: name of the file that is being read. 
 * Content: list of list of characters. 
*/

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

/**
 * read_lines/2 will read all lines of the file. 
 * Stream: an opened file in read mode. 
 * Content: list of list of chars. One string per line of the opened stream. 
*/
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true  % end of file
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).
/**
 * read_line/3 will read a line. Last is a flag, which shows whether the line 
 * is the last line of the file or not. 
 * Stream: an opend file in read mode. 
 * Line: a line of the file that is being read
 * Last: true/false, works like a boolean type. True if it meets the end of file,
 * False if it meets a newline char. 
 * 
*/
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file  % End of file
	->  Line = [],
	    Last = true
	; Char = '\n'  % Newline 
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],   % Normal read.
	    read_line(Stream, Line1, Last)
	).