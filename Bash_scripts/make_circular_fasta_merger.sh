FILES="/home/sitterl/scratch/multiple_sequence_alignment/aurelie_plasmids/*"
for f in $FILES
do
	echo $f
	filename=$(basename "$f")
	echo ">$filename" >> all.fasta
	cat $f | sed '/>/d' >> all.fasta
	wait
	cat $f | sed '/>/d' >> all.fasta
	wait
done
sed -i 's/A>/A\n>/g' all.fasta
wait
sed -i 's/C>/C\n>/g' all.fasta
wait
sed -i 's/G>/G\n>/g' all.fasta
wait
sed -i 's/T>/T\n>/g' all.fasta
