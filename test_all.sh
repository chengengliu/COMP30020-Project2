#!/bin/sh

[ "$#" -ne 1 ] && n=$(ls -1v filled* | cut -c7-) || n=$1


BORDER="------------------------------"
out=$(mktemp)

for i in $n
do
	[ ${i:0:1} -eq 3 ] && j=${i:0:1} || j=$i
	echo "Puzzle$j, Words$j, Filled$i" >> $out

	prompt="main(puzzle$j, words$j, tmp_sol)"

	start=$(date +%s.%N)
	swipl -O -q --no-signal -g "[proj2]" -t "$prompt"
	end=$(date +%s.%N)

	echo "Done $prompt"
	
	diff="Time = $(echo "$end - $start" | bc)"

	[ -s tmp_sol ] && diff tmp_sol filled$i > tmp_diff

	[ -e tmp_diff ] && ( [ -s tmp_diff ]  && cat tmp_diff >> $out || echo "Identical" >> $out ) || echo "Returned false" >> $out

	echo $diff >> $out
	echo $BORDER >> $out

	[ -e tmp_diff ] && rm -f tmp_diff
	rm -f tmp_sol
done

echo $BORDER
cat $out
rm -f $out