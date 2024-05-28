# Identifies all files for the CTmax experiments 
ctmax_files = dir(path = "Raw_data/ctmax_data/")
ctmax_times = ctmax_files[str_detect(ctmax_files, pattern = "_obs")]
ctmax_temps = ctmax_files[str_detect(ctmax_files, pattern = "_temp")]

#### Processing the CTmax data ####
ctmax_data = data.frame()
for(file in ctmax_times){
  
  meta_info = str_split_fixed(file, pattern = "obs", n = 2)
  
  time_data = read.csv(file = paste("Raw_data/ctmax_data/", file, sep = ""), na.strings = "NA") %>% 
    mutate(fecundity = as.numeric(fecundity))
  temp_data = read.csv(file = paste("Raw_data/ctmax_data/", meta_info[1], "temp.csv", sep = ""))
  
  exp_data = est_ctmax(temp_data = temp_data, time_data = time_data)
  
  ctmax_data = bind_rows(ctmax_data, exp_data)
}

# Write the data to files in the Output directory
write.csv(ctmax_data, file = "Output/Output_data/ctmax_data.csv", row.names = F)

