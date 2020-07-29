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
#	echo -ne "lib1 bowtie $FILE_1 $FILE_2 350 0.75 FR" > ${FOLDER_NAME%/}_gapfiller_config
#	bowtie-build ${FOLDER_NAME%/}.closed_gaps.final/${FOLDER_NAME%/}.closed_gaps.final.gapfilled.final.fa assembly_db
#	wait
#	condor_run -a request_cpus=16 "bowtie assembly_db -q -1 $FILE_1 -2 $FILE_2 -X 600 --fr -y -t -p 16 -S --sam-RG TAG:PAIR | samtools view -bS - > output.bam" &
	condor_run -a request_cpus=16 "samtools sort output.bam output.sorted.bam" &
	cd ../
done


