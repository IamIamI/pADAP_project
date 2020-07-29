FILES="/home/sitterl/serverhome/scratch/raw_trimmer_seq_reads/*/"
for f in $FILES
do
	FOLDER_NAME=${f##*reads/}
#	echo $FOLDER_NAME
	FILE_1=/home/sitterl/scratch/raw_trimmer_seq_reads/${FOLDER_NAME}${FOLDER_NAME%/}_1_val_1.fq  
	FILE_2=/home/sitterl/scratch/raw_trimmer_seq_reads/${FOLDER_NAME}${FOLDER_NAME%/}_2_val_2.fq  
#	echo ${FILE_1}
#	OUTPUT_DIR="${FOLDER_NAME}${FOLDER_NAME%/}_A5/"
#	echo $OUTPUT_DIR

	cd ${FOLDER_NAME}
	# Make the library file for A5
	echo -ne "lib1 bowtie $FILE_1 $FILE_2 350 0.75 FR" > ${FOLDER_NAME%/}_sspace_config
	condor_run -a request_cpus=16 "SSPACE_Standard_v3.0.pl -l ${FOLDER_NAME%/}_sspace_config -s ${FOLDER_NAME%/}.out.contigs.fasta -x 1 -m 40 -o 20 -k 5 -a 0.7 -b ${FOLDER_NAME%/}_scaffold_extension" &
	
	cd ../
done