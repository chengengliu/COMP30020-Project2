## COMP30020 Declarative Programming Project2 

## These are some basic ideas about the project. 
Brute forcely making guess is possible but cannot meet the efficiency here. 
Some rough sketch can be(this idea was written before i started coding):

    1. filling the empty slot with logical varaible(most basic operation, 
    even is the beginning of the brute force. )

    2.  finding slots in which to put those words. 

    3.  generate words with the corresponding order that maybe correct word for each slot. 

    4.  these slots are sorted(somehow I don't know how to implement sorting yet in prolog, but 
    possibly since we have implemented that in workshop. ).

    5.  After sorting, pick up the slot with the fewest possible words.

    6.  I think I will go with hint 4 rather than hint3. 

    7.  Finish the project and pass 7 tests on the server. Already submit on 6/10/2018

    8.  Quite a painful project anyway. From now on util it is due, I will be focusing on 
    commenting/file&function level doc/code quality & style/.

## self_test.sh usage:
    bash test_all.sh -- for all test cases in the dir or
    bash test_all.sh "2 3a 4" -- for some of them
## Purpose of the project and main Algorithm:
    % This is the program for Fillin Puzzles. 
    % The program tries to solve a Fillin Puzzle.
    % A Fillin Puzzle is to provide a puzzle and a list of words, which 
    % the player needs to select words to fit in the given puzzle. 
    % main/3 and IO-code is provided by Peter Schachte.
    % We are asked to implement solve_puzzle/3 to solve the Fillin Puzzle. 

    % Algorithm used for solve_puzzle/3 is written below. 
    % Further algorithm explaination will be written in more depth later 
    % of the program. 
    % 1. Replace all underscores with logical variables. 
    % 2. Build a list of slots, where each slot is a list of logical var
    % representing a single square in the puzzle. This step is important 
    % if the same var is used for the same slot in either horizontal or vertical
    % direction when unifying the var, it will be correctly unified 
    % horizontaly and vertically.
    % 3. Fill in words in to the puzzle until solved. 
    % Selecting a slot and a word and unifying the word with the slot 
    % and recursing. Good choices should be made when selecting the best slot to 
    % filling in. 
