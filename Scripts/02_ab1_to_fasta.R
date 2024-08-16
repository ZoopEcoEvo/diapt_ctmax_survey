if(process_sequences == T){
  
  library(sangeranalyseR)
  library(seqinr)
  library(msa)
  library(tidyverse)
  library(kableExtra)
  library(pegas)
  library(adegenet)
  
  alignments = SangerAlignment(ABIF_Directory      = paste0("Raw_data/seq_data/"),
                               processMethod       = "REGEX",
                               REGEX_SuffixForward = paste0("_f_.*ab1$"),
                               REGEX_SuffixReverse = paste0("_r_.*ab1$"))
  
  writeFasta(alignments, outputDir = "Output/Sequences/")
  
  generateReport(alignments,
                 outputDir = paste0("Output/Sequences/"),
                 includeSangerRead = T, 
                 includeSangerContig = F)
}


