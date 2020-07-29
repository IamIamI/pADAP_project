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
	echo -ne "[LIB]\np1=$FILE_1\np2=$FILE_2\nins=350" > ${FOLDER_NAME%/}_a5_config
	
	condor_run -a request_cpus=16 "a5_pipeline.pl ${FOLDER_NAME%/}_a5_config ${FOLDER_NAME%/}.out" &
	
	cd ../
done
 
