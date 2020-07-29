FILES="/home/sitterl/serverhome/scratch/raw_trimmer_seq_reads/*/"
for f in $FILES
do
	FOLDER_NAME=${f##*reads/}
#	echo $FOLDER_NAME
	FILE_1=/home/sitterl/scratch/raw_trimmer_seq_reads/${FOLDER_NAME}${FOLDER_NAME%/}_1_val_1.fq  
	FILE_2=/home/sitterl/scratch/raw_trimmer_seq_reads/${FOLDER_NAME}${FOLDER_NAME%/}_2_val_2.fq  
#	echo ${FILE_1}
	cd ${FOLDER_NAME}
	# Make the library file for A5
	echo -ne "lib1 bowtie $FILE_1 $FILE_2 350 0.75 FR" > ${FOLDER_NAME%/}_gapfiller_config
	condor_run -a request_cpus=16 "perl /home/sitterl/software/GapFiller_v1-10_linux-x86_64/GapFiller.pl -l ${FOLDER_NAME%/}_gapfiller_config -s ${FOLDER_NAME%/}_scaffold_extension/${FOLDER_NAME%/}_scaffold_extension.final.scaffolds.fasta -m 40 -o 5 -r 0.7 -n 40 -d 50 -t 10 -T 16 -i 10 -b ${FOLDER_NAME%/}.closed_gaps.final" &
	
	cd ../
done


