#!/bin/bash

if ! [ -f Scripts/04_sp_prop/bin/skistodb.ndb ]; then
  echo "Making Blast Database"

  # The first time, run makeblastdb to set up the local data base
  ./Scripts/04_sp_prop/bin/makeblastdb -in ./Scripts/04_sp_prop/bin/reference_haplotypes.fasta -out ./Scripts/04_sp_prop/bin/skistodb -dbtype 'nucl' -hash_index
fi


echo "Blasting new sequences against the database"

# This command will blast the COI sequences against the data base
# Outputs results as a CSV file with columns: 'qaccver saccver pident length mismatch gapopen qstart qend sstart send evalue bitscore'
./Scripts/04_sp_prop/bin/blastn -query Output/Sequences/Sanger_contigs_unalignment.fa -task blastn -db ./Scripts/04_sp_prop/bin/skistodb -out ./Output/Sequences/COI_blast/skisto_blast.csv -outfmt 10 -evalue 10 -word_size 4 -num_threads 3

echo "Making a best guess at the clade ID for each sample"
#Rscript Scripts/04_sp_prop/clade_matches.R
