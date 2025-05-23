library(ape)
library(stringr)

myseqs.fn <- "./Output/Sequences/COItree/aligned.fasta" # input file
myseqs.nex.fn <- "./Output/Sequences/COItree/skisto.nex" # output in Nexus format
myseqs.mb <- "./Output/Sequences/COItree/skisto_mb.nex" # output in Nexus with MrBayes block

myseqs <- read.dna(myseqs.fn,format="fasta",as.matrix=FALSE)
myseqs.names <- names(myseqs)

write.nexus.data(as.character(myseqs),myseqs.nex.fn,interleaved=FALSE,gap="-",missing="N")
myseqs.nex <- readLines(myseqs.nex.fn)


# We then write part of the MrBayes block that specifies the model.
#GTR + I + G, from figuerra MS

# set outgroup to L. macrurus
# help lset
#    For example, "lset nst=6 rates=gamma" would set the model to a general model of DNA substitution (the GTR) with gamma-distributed rate variation across sites.

mbblock1 <- "
begin mrbayes;
  set autoclose=yes nowarn=yes;
  lset nst=2 rates=invgamma ngammacat=4;
    outgroup OP830097.1_Limnocalanus_macrurus;
"

mbblock3 <- "
  mcmc ngen=750000 nruns=2 nchains=2 samplefreq=1000;
    sump burninfrac=0.25;
    sumt burninfrac=0.25;
    quit;
end;
"

myseqs.nexus.withmb <- paste(paste(myseqs.nex,collapse="\n"),
                    mbblock1,mbblock3,sep="")
write(myseqs.nexus.withmb,file=myseqs.mb)


# We can now run MrBayes with the command mb with the output filename as the single argument.
