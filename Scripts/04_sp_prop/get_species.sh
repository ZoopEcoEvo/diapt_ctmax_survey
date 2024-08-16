#!/bin/bash

if ! [ -f Scripts/03_blast/bin/acartiadb.ndb ]; then
  echo "Making Blast Database"

  # The first time, run makeblastdb to set up the local data base
  ./Scripts/03_blast/bin/makeblastdb -in ./Scripts/03_blast/bin/reference_haplotypes.fasta -out ./Scripts/03_blast/bin/acartiadb -dbtype 'nucl' -hash_index
fi


echo "Blasting new sequences against the database"

# This command will blast the COI sequences against the data base
# Outputs results as a CSV file with columns: 'qaccver saccver pident length mismatch gapopen qstart qend sstart send evalue bitscore'
./Scripts/03_blast/bin/blastn -query Output/Sequence_data/COI/Sanger_contigs_unalignment.fa -task blastn -db ./Scripts/03_blast/bin/acartiadb -out ./Output/COI_blast/acartia_blast.csv -outfmt 10 -evalue 10 -word_size 4 -num_threads 3

echo "Making a best guess at the clade ID for each sample"
Rscript Scripts/03_blast/clade_matches.R
