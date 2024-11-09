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
