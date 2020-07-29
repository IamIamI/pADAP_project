FILES="/home/sitterl/scratch/repA_region_per_plasmid/prokka_backbone/*fa"
for f in $FILES
do
	BASE_FILE=${f##*/}
	OUTDIR="${BASE_FILE}_prokka"
	echo $BASE_FILE
	echo $OUTDIR
	prokka --outdir prokka/${OUTDIR} --prefix $BASE_FILE --addgenes --locustag $BASE_FILE --genus Serratia --species proteamaculans --strain $BASE_FILE --kingdom Bacteria --proteins /home/sitterl/scratch/pADAP/pADAP_genebank_CDS.fa --cpus 16 $f
	wait
	echo "$BASE_FILE" >> info.txt
	cut -d '\t' -f2
	grep "atoX" X.fa.gff | cut -f 1,4,5,7 | tr -d "\n" >> pilLinfo.txt
	echo -e "\t" | tr -d "\n" >> pilLinfo.txt
	grep "pilL" X.fa.gff | cut -f 4,5,7 >> pilLinfo.txt
	wait
done
