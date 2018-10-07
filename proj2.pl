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
% Every row os slot of the puzzle will be stored in slots. 
rows_slots([], []).
rows_slots([R|Rows], Slots):-
    one_row_slot(R, RowSlots),
    append(RowSlots,TempSlots, Slots),
    rows_slots(Rows, TempSlots).

/**
 * one_row_slot/2 will return Slots which contain a list of slots present 
 * in the Row. aka the Slots is the reuslt of splitting the Row on '#'
 * one_row_slot/2 calls one_row_slot/3 with a tail recursion (initialising
     the acc as []).
*/
one_row_slot(Row, Slots):-
    one_row_slot(Row, [], Slots).
/**
 * one_row_slot/3 return the slots in one row. 
 * It uses Acc as an accumulator, recursively adding chars into it 
 * until it meets a '#'. When meeting a '#', it adds the Acc into Slots and 
 * resets Acc to be empty lists. 
*/

one_row_slot([],[],[]).
% Notice that if there is no '#' at the end of the row, then Acc is made into
% a list with itself. (This is another base case that i should handle.)
% TODO:  This is where I got stuck....
one_row_slot([], Acc, [Acc]):- 
    Acc \= [].

% If there is a '#', add the Acc to the list of slots and reset Acc to an empty
% list.
one_row_slot([H|Rest], Acc, Slots):-
    H == '#',
    Slots = [Acc|Slots1],
    one_row_slot(Rest,[], Slots1).


% If not meeting the '#', keep adding to Acc. 
one_row_slot([H|Rest], Acc,Slots):-
    %%%%% I found the bug.......... \==
    H \== '#',
    append(Acc,[H],Acc1),
    one_row_slot(Rest,Acc1,Slots).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fill_words/2 should accept a slots as argument. This predicate comes true if 
% slots are capable of being filled with Wordlist. 
% Wordlist is a list of the words that are being filled into the puzzle. 
% We need to find the best fit slot to place a word into and choose a word to 
% be placed into that slot.
% The best slot is defined as the one that has the fewest matching words 
% in the wordlist. 


fill_words([],[]).
fill_words(Slots, Wordlist):-
    select_best_slot(Slots, Wordlist, Best),
    % TODO: Zzz
    exclude(\=(Best), Wordlist, Match),
    member(Word, Match),
    Best = Word,
    exclude(==(Word), Wordlist, RestOfWords),
    exclude(==(Best), Slots, RestOfSlots),
    fill_words(RestOfSlots, RestOfWords).
    
/**
 * select_best_slot/3 will return the best slot, which is defined as the least
 * amount of matching words that cane be used to fill(this will reduce the 
     search space and backtrack space)
 * [H|Rest] is a list of slots in the puzzle. 
 * Wordlist is a list words being filled into the puzzle. 
 * Best is the slot with the fewest matching words in the Wordlist.
 * 
 * The predicate will use select_best_slot/5, which will try to unify a slot
 * with a word. (A word is said to be 'match' only when the word could unify
     the slot).
 * More explaination will be introduced later.
 * 
*/
select_best_slot([H|Rest], Wordlist, Best):-
    % count_/3 will return Number as the number of the matching words.
    % Slot here is considered as the 'temp'-best slot so far. It will
    % be passed to select_best_slot/5 to test if it is the real best 
    count_(H, Wordlist, MatchNumber),
    select_best_slot(Wordlist,MatchNumber, Rest, H, Best).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * count_/3 will call count_/4 with a Acc.
 * Number is the number of words in the wordlist that can fit in the slot. 
 * Slot is a slot in the puzzle. 
 * Wordlist is a list of words being filled into the puzzle. 
 * The wordlist will be traversed to check if there is one word that can 
 * be fit into the slot. 
*/
count_(Slot, Wordlist, MatchNumber):-
    count_(Slot, Wordlist, 0, MatchNumber).

/**
 * Acc is the accumulator
 * Count will be the total count of the number of words that matching 
 * the slot. 
*/
count_(_,[], Acc, Acc).
count_(Slot, [W|Wordlist], Acc, MatchNumber):-
    (Slot \= W->
        Acc1 is Acc;
        Acc1 is Acc +1
    ),
    count_(Slot, Wordlist, Acc1, MatchNumber).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/**
 * select_best_slot(Wordlist, MatchNumber, Slots, CBest, Best) is to compute
 * the best slot. This is the main algorithm
 * select_best_slot/5 will also use count_ to traverse the wordlist to find
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
    (Count < MatchNumber->
        CurrentBest1 = S, 
        MatchNumber1 = Count;
        CurrentBest1= CurrentBest,
        MatchNumber1 = MatchNumber
    ),
    select_best_slot(Wordlist, MatchNumber1, Slots, CurrentBest1, Best).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
% solve_puzzle(Puzzle0, Wordlist, Puzzle)
% should hold when Puzzle is a solved version of Puzzle0, with the
% empty slots filled in with words from Wordlist.  Puzzle0 and Puzzle
% should be lists of lists of characters (single-character atoms), one
% list per puzzle row.  Wordlist is also a list of lists of
% characters, one list per word.
%
% This code is obviously wrong: it just gives back the unfilled puzzle
% as result.  You'll need to replace this with a working
% implementation.


read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).