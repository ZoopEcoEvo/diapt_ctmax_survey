#!/bin/bash

if [[ "$1" == "-h" || "$1" == "--help" ]]; then
   echo "pipeline to examine relationships between Skistodiaptomus individuals using muscle for alignment and mr bayes."
   echo
   echo "Syntax: ./COI_pipeline.sh input_fasta plot_name"
   echo
   echo "Mandatory input:"
   echo "input_fast         file containing the new samples to analyze. In fasta format. This file should be in the main directory with all scripts"
   echo "plot_name          name for output plot for this run"
   echo
   echo "This script requires a number of fasta files containing the reference sequences in addition to the scripts in the main directory. Do not modify these files"
   echo
   echo "all output will be directed to ./output"
  exit 0
fi

if [[ ! -f "$1" ]]; then
    echo "The fasta file "$1" does not exist. Exiting."
    exit
fi

if [[ ! -e output ]]; then
    mkdir output
fi

#we will start with fasta files. these need to be aligned, then run mr bayes

#merge new samples with reference samples
cat "$1" \
    Scripts/03_COItree/reference_haplotypes.fasta \
    > ./Output/Sequences/COItree/to_align.fasta


# align all data
echo "alignment starting"
muscle -align ./Output/Sequences/COItree/to_align.fasta \
       -output ./Output/Sequences/COItree/aligned.fasta
       
echo "alignment done"
echo " "

# make mr bayes file
## need to have ape and stringr installed
echo "making mr bayes file"

Rscript Scripts/03_COItree/make_mr_bayes.R
echo "Initial mr bayes R script run successfully"


# run mr bayes

# https://www.ebi.ac.uk/Tools/services/rest/muscle/result/muscle-I20200720-155944-0426-80668140-p1m/aln-fasta
echo " "
echo "Mr bayes starting. This can take a long time (>20 minutes)."

mb ./Output/Sequences/COItree/skisto_mb.nex > ./Output/Sequences/COItree/mr_bayes_log.txt

if grep -q "Error" ./Output/Sequences/COItree/mr_bayes_log.txt; then
    echo "ERROR in Mr Bayes! Are there spaces in your fasta names? \nExiting."
    exit
fi


echo " "
echo "Mr bayes done"
echo " "
# HKY + I + G for sub model
# for HKY: nst=2
# I + G = invariable site plus discrete Gamma model: rates=invgamma


#plot output
echo "generating plot"
Rscript Scripts/03_COItree/plot_tree.R

mv ./Output/Figures/tree_plot.pdf ./Output/Figures/"$2"_plot.pdf



