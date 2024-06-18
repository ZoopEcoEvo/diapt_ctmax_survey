# Load in required packages
library(rmarkdown)
library(tidyverse)
library(ggridges)
source("Scripts/project_functions.R")

#Determine which scripts should be run
process_data = T #Runs data analysis 
process_sequences = F #Analyzes the COI sequence data
make_report = T #Runs project summary
knit_manuscript = F #Compiles manuscript draft

############################
### Read in the RAW data ###
############################

if(process_data == T){
  source(file = "Scripts/01_data_processing.R")
}

source(file = "Scripts/02_ab1_to_fasta.R") 


site_data = readxl::read_excel("Raw_data/site_list.xlsx") %>% 
  drop_na(lat) %>% 
  mutate(lat = as.numeric(lat), 
    site = fct_reorder(site, lat))

ctmax_data = read.csv(file = "Output/Output_data/ctmax_data.csv") %>% 
  inner_join(select(site_data, site, lat, collection_temp), 
             by = "site") %>% 
  mutate(site = as.factor(site), 
         lat = as.numeric(lat),
         site = fct_reorder(site, lat))

##################################
### Read in the PROCESSED data ###
##################################

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
