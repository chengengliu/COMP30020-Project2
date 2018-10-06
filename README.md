## COMP30020 Declarative Programming Project2 

## This is some basic ideas about the project. 
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

## Algorithm truly used in the project: 
    1.  Find dall underscores in the puzzle and fill the puzzle with logical vairables.

    2.  Build a list of slots, which each slot is bunch of logical variables, representing a single square of the puzzle. If the same variable is used for the same slot(either in horizontal or vertical direction), it will be unified/simplified(don't know how to say it in English), which Prolog will automatically do (as described in the pdf file).

    3.  Start filling in the words. Main algorithm is that selecting a slot and a word each time, meanwhile traverse the wordlist to pickup the word. Keep track of the minimum number of matching word of the slot and keep updating the number. The word with the samllest matching number will be selected as the best match. 
