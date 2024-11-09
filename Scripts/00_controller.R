# Load in required packages
library(rmarkdown)
library(tidyverse)
library(ggridges)
library(elevatr)
source("Scripts/project_functions.R")

#Determine which scripts should be run
process_data = F #Runs data analysis 
process_sequences = F #Aligns the COI sequence data
make_tree = F #Makes a ML tree from the COI sequences
make_sp_prop = T #Plots the species frequency at each site
make_report = F #Runs project summary
knit_manuscript = F #Compiles manuscript draft

skisto_cols = c("Skistodiaptomus reighardi" = "#8DAB7F",
               "Skistodiaptomus pallidus" = "#A77698",
               "Skistodiaptomus oregonensis" = "#F8C425",
               "Skistodiaptomus mississippiensis" = "#AF4308",
               "Skistodiaptomus pygmaeus" = "#14A3A1",
               "Skistodiaptomus carolinensis" = "#F78A50",
               "Leptodiaptomus minutus" = "lightpink2", 
               "Leptodiaptomus nudus" = "lightpink3",
               "Aglaodiaptomus spatulocrenatus" = "indianred3")

############################
### Read in the RAW data ###
############################

site_data = read.csv("Raw_data/site_list.csv") %>% 
  drop_na(lat) %>% 
  mutate(lat = as.numeric(lat), 
         site = fct_reorder(site, lat))

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

source(file = "Scripts/02_ab1_to_fasta.R") 


##################################
### Read in the PROCESSED data ###
##################################

elev_data = read.csv(file = "Output/Output_data/elev_data.csv")

ctmax_data = read.csv(file = "Output/Output_data/ctmax_data.csv") %>% 
  select(-collection_date) %>% 
  inner_join(select(site_data, site, lat, collection_date, collection_temp), 
             by = "site") %>% 
  inner_join(elev_data, by = "site") %>% 
  mutate(site = as.factor(site), 
         lat = as.numeric(lat),
         site = fct_reorder(site, lat),
         collection_date = as_date(collection_date)) %>% 
  group_by(sample_id) %>% 
  mutate(mean_egg = mean(c_across(starts_with("egg")), na.rm = TRUE),
         radius = sqrt(mean_egg / pi),
         egg_volume = (4/3)*pi*radius^3,
         total_egg_volume = egg_volume * fecundity) %>% 
  ungroup()

data_summary = ctmax_data %>% group_by(site, species) %>%  
  summarise(mean_ctmax = mean(ctmax, na.rm = T),
            n(),
            se_ctmax = sd(ctmax) / sqrt(n()))

if(make_tree == T){
  #### Analyzes COI sequence data ####
  ## Takes the new COI sequences and aligns them to a curated set of Acartia COI sequences
  ## Then uses MrBayes to generate a phylogeny of the COI sequences
  ## NOTE: This is a slow step. Takes a while to run
  system("./Scripts/03_COItree/COI_pipeline.sh Output/Sequences/Sanger_contigs_alignment.fa skisto_phylogeny")
}

if(make_sp_prop == T){
  ## Takes the new unaligned fasta file and runs a local blast against the curated set of Acartia COI sequences 
  ## Filters the results and then checks for consistent identification of clades
  ## Then plots clade proportion for each population
  
  system("./Scripts/04_sp_prop/get_species.sh")
}


if(make_report == T){
  render(input = "Output/Reports/report.Rmd", #Input the path to your .Rmd file here
         #output_file = "report", #Name your file here if you want it to have a different name; leave off the .html, .md, etc. - it will add the correct one automatically
         output_format = "all")
}

##################################
### Read in the PROCESSED data ###
##################################

if(knit_manuscript == T){
  render(input = "Manuscript/manuscript_name.Rmd", #Input the path to your .Rmd file here
         output_file = paste("dev_draft_", Sys.Date(), sep = ""), #Name your file here; as it is, this line will create reports named with the date
                                                                  #NOTE: Any file with the dev_ prefix in the Drafts directory will be ignored. Remove "dev_" if you want to include draft files in the GitHub repo
         output_dir = "Output/Drafts/", #Set the path to the desired output directory here
         output_format = "all",
         clean = T)
}
