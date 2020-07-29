FILES="/home/sitterl/archive/Raw_Reads/Macrogen_14_11_2016/*/"
for f in $FILES
do
        FOLDER_NAME=${f##*2016/}
#       echo $FOLDER_NAME
        FILE_1=/home/sitterl/archive/Raw_Reads/Macrogen_14_11_2016/${FOLDER_NAME}${FOLDER_NAME%/}_1.fastq.gz
        FILE_2=/home/sitterl/archive/Raw_Reads/Macrogen_14_11_2016/${FOLDER_NAME}${FOLDER_NAME%/}_2.fastq.gz
#       echo ${FOLDER_NAME}${FILE_1}
        OUTPUT_DIR="/home/sitterl/scratch/raw_trimmer_seq_reads/${FOLDER_NAME}"
#       echo $OUTPUT_DIR
        mkdir -p $OUTPUT_DIR
        condor_run -a request_cpus=16 "trim_galore_local --fastqc --illumina -q 30 --dont_gzip --max_n 1 --retain_unpaired -o $OUTPUT_DIR --suppress_warn --paired $FILE_1 $FILE_2" &
done