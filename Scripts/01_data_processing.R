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
# 
# 
# ### Getting elevations for the different sampling sites
# diapt_coords = site_data %>%
#   select(x = long, y = lat, site) %>%
#   distinct() %>%
#   as.data.frame()
# 
# crs_dd <- 4326 # EPSG:4326
# 
# diapt_elev = data.frame()
# for(i in 1:dim(diapt_coords)[1]){
#   print(i)
#   point_elev = data.frame(get_elev_point(diapt_coords[i,], prj = crs_dd, src = "epqs")) %>%
#     select(site, elevation)
#   
#   diapt_elev = bind_rows(diapt_elev, point_elev)
# }
# 
# na_elev = filter(diapt_coords,
#                  !site %in% drop_na(diapt_elev)$site)
# 
# for(i in 1:dim(na_elev)[1]){
#   point_elev = data.frame(get_elev_point(na_elev[i,], prj = crs_dd, src = "epqs")) %>%
#     select(site, elevation)
#   
#   diapt_elev = bind_rows(diapt_elev, point_elev)
# }
# 
# diapt_elev = diapt_elev %>%  
#   drop_na() %>% 
#   distinct()
# 
# write.csv(diapt_elev, file = "Output/Output_data/elev_data.csv", row.names = F)

# Identifies all files for the respiration experiments 
resp_files = dir(path = "Raw_data/F3_resp_data/o2_data_utf8/")

tpc_rates = tibble()

for(i in resp_files){
  
  test_temp = parse_number(str_split_fixed(i, pattern = "_", n = 5)[4])
  test_date = paste0(str_split_fixed(i, pattern = "_", n = 5)[c(1,2,3)], collapse = "-")
  
  file_path <- paste("Raw_data/F3_resp_data/o2_data_utf8/", i, sep = "") # Replace with the actual path to your file
  lines <- readLines(file_path, skipNul = T)
  treatment_line <- grep("TREATMENTS", lines)
  calib_line <- grep("CALIBRATION", lines)
  start_line = grep("Date", lines)[2]
  
  ## Pull treatment IDs (in between "TREATMENTS" and "CALIBRATION")
  treatment_codes = read.csv(file_path,
                             sep = "\t",
                             skip = treatment_line, 
                             nrows = (calib_line - treatment_line) - 2) %>% 
    select("treatment_id" = ID, "treatment" = Name) %>% 
    mutate(treatment_id = if_else(treatment_id == "X", NA, treatment_id), 
           treatment_id = as.numeric(treatment_id)) %>% 
    filter(treatment != "")
  
  ## Pull well treatments and individual numbers (Three lines: wells, organisms, treatment ID)
  ind_numbers = read.csv(file_path,
                         sep = "\t",
                         skip = calib_line, 
                         nrows = 7, 
                         na.strings = "X") %>% 
    filter(Wells %in% c("Organisms")) %>% 
    select(Wells:D6) %>% 
    pivot_longer(cols = c(A1:D6), 
                 values_to = "individuals", 
                 names_to = "well_id") %>% 
    select(-Wells)
  
  treatment_ids = read.csv(file_path,
                           sep = "\t",
                           skip = calib_line, 
                           nrows = 7, 
                           na.strings = "X") %>% 
    filter(Wells %in% c("Treatment ID")) %>% 
    select(Wells:D6) %>% 
    pivot_longer(cols = c(A1:D6), 
                 values_to = "treatment_id", 
                 names_to = "well_id") %>% 
    select(-Wells) %>% 
    filter(!is.na(treatment_id)) %>% 
    left_join(treatment_codes) %>%  
    left_join(ind_numbers)
  
  oxy_data = read.csv(file_path,
                      sep = "\t",
                      skip = start_line-1) %>% 
    janitor::clean_names() %>%  
    separate_wider_delim(cols = relative_time_hh_mm_ss, 
                         delim = ":",
                         names = c("hour", "minute", "second")) %>% 
    mutate(timepoint = as.numeric(minute) + 60*as.numeric(hour) + as.numeric(second)/60) %>% 
    select(timepoint, ends_with("_oxygen")) %>% 
    pivot_longer(cols = (a1_oxygen:d6_oxygen), 
                 names_to = c("well_id", NA), 
                 values_to = "perc_oxy_sat", 
                 names_sep = "_oxygen") %>% 
    mutate(well_id = toupper(well_id))
  
  
  all_rates = data.frame()
  
  for(j in 1:dim(treatment_ids)[1]){
    
    select_well = treatment_ids$well_id[j]
    treatment = treatment_ids$treatment[j]
    ind_num = treatment_ids$individuals[j]
    
    well_data = oxy_data %>% 
      filter(well_id == select_well) %>% 
      select(timepoint, perc_oxy_sat)
    
    auto_rate = suppressMessages(auto_rate(well_data, plot = F, width = 0.2))$summary[1,] %>% 
      select(rate, rsq)
    
    rates = data.frame(
      well_id = select_well, 
      treatment = treatment,
      ind_num = ind_num,
      rate = auto_rate$rate, 
      rsq = auto_rate$rsq, 
      temp = test_temp
    )
    
    all_rates = bind_rows(all_rates, rates)
    
  }
  
  blanks = all_rates %>% 
    filter(treatment == "Blank") %>% 
    group_by(temp) %>% 
    summarise(blank_resp = mean(rate))
  
  resp_rates = all_rates %>% 
    filter(treatment != "Blank") %>% 
    mutate(rate = ((rate / ind_num) * -1) + blanks$blank_resp,
           date = test_date)
  
  tpc_rates = bind_rows(tpc_rates, resp_rates)
}

write.csv(tpc_rates, "Output/Output_data/resp_data.csv", row.names = F)
