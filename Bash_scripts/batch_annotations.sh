FILES="/home/sitterl/serverhome/scratch/Plasmid_prokka/Plasmids/*"
for f in $FILES
do
	FOLDER_NAME=${f##*Plasmids/}
	PLASMID_NAME=${FOLDER_NAME%\.fasta}
	STRAIN_NAME=${FOLDER_NAME%\.plasmid*\.fasta}
#	echo $STRAIN_NAME
#	echo $PLASMID_NAME
#	echo $FOLDER_NAME
	echo $STRAIN_NAME\/$PLASMID_NAME
	FILE_1=/home/sitterl/serverhome/scratch/Plasmid_prokka/Plasmids/${FOLDER_NAME}  
#	echo ${FILE_1}
	# Make the library file for A5
	condor_run -a request_cpus=16 "prokka --outdir ${PLASMID_NAME}_prokka/ --compliant --rfam --prefix ${PLASMID_NAME} --addgenes --locustag ${PLASMID_NAME}_ --strain ${STRAIN_NAME} --kingdom Bacteria --proteins /home/sitterl/scratch/bacterial_db/reference_protein_db.fasta --cpus 16 $FILE_1" &
done


