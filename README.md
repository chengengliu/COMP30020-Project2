#COMP30020 Project2 

## This is some basic ideas about the project. 
Brute forcely making guess is possible but cannot meet the efficiency here. 
Some rought process can be: 

    1. filling the empty slot with logical varaible(most basic operation, 
    even is the beginning of the brute force. )

    2.  finding slots in wich to put those words. 

    3.  generate words with the corresponding order that maybe correct word for each slot. 

    4.  these slots are sorted(somehow I don't know how to implement sorting yet in prolog, but 
    possibly since we have implemented that in workshop. ).
    
    5.  After sorting, pick up the slot with the fewest possible words. 