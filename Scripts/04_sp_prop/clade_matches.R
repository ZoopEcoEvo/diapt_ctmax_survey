suppressPackageStartupMessages({
  library(tidyverse)
})

blast_results = read.csv(file = "Output/Sequences/COI_blast/skisto_blast.csv",
                         col.names = c("sample", "ref", "pident", "length", "mismatch", "gapopen", 
                                       "qstart", "qend", "sstart", "send", "evalue", "bitscore"))

### Used to create a manually curated set of labels with Clade IDs for some species
# ref_clades = blast_results %>%  
#   mutate(group = str_split_fixed(ref, pattern = "_", n = 2)[,2], 
#          group = str_replace(group, "_", " ")) %>% 
#   select(ref, group) %>% 
#   distinct()
# 
# write.csv(ref_clades, file = "Output/Sequences/ref_clades.csv", row.names = F)

unique_samples = unique(blast_results$sample)

clade_counts = blast_results %>% 
  inner_join(clade_ids, by = join_by("ref" == "label")) %>% 
  filter(length > 300) %>% 
  filter(pident > 90) %>% 
  group_by(sample, ref) %>% 
  filter(evalue == min(evalue)) %>% 
  mutate(group = str_split_fixed(ref, pattern = "_", n = 2)[,2], 
         group = str_replace(group, "_", " ")) %>% 
  ungroup() %>% 
  select(sample, group, clade) %>% 
  distinct() %>% 
  group_by(sample, group, clade) %>%  
  dplyr::count()

table(clade_counts$sample)

singles = clade_counts %>% 
  ungroup() %>% 
  group_by(sample) %>%  
  dplyr::count() %>% 
  filter(n == 1) %>% 
  select(sample)

identified = clade_counts %>% 
  filter(sample %in% singles$sample) %>%  
  select(-n)

checks = clade_counts %>% 
  filter(!sample %in% singles$sample) %>% 
  bind_rows(data.frame("sample" = unique_samples[!unique_samples %in% identified$sample]))

manual_ids = data.frame(
  "sample" = checks$sample) %>% 
  mutate("group" = case_when(
    str_detect(sample, pattern = "CR") ~ "Skistodiaptomus pygmaeus",
    str_detect(sample, pattern = "FR") ~ "Leptodiaptomus nudus", 
    str_detect(sample, pattern = "OW") ~ "Skistodiaptomus mississippiensis",
    str_detect(sample, pattern = "MH") ~ "Skistodiaptomus pygmaeus",
    str_detect(sample, pattern = "Py") ~ "Skistodiaptomus mississippiensis"),
    "clade" = case_when(
      str_detect(sample, pattern = "CR") ~ "S. pygmaeus",
      str_detect(sample, pattern = "FR") ~ "L. nudus", 
      str_detect(sample, pattern = "OW") ~ "S. mississippiensis - New",
      str_detect(sample, pattern = "MH") ~ "S. pygmaeus",
      str_detect(sample, pattern = "Py") ~ "S. mississippiensis - New"))

new_samples = identified %>% 
  bind_rows(manual_ids) %>% 
  separate_wider_position(cols = sample,
                          widths = c("pop" = 2, "ind" = 2),
                          too_few = "align_start", too_many = "drop") %>% 
  left_join(site_data, join_by(pop == code), relationship = "many-to-many") %>% 
  ### Have to manually correct the LW S. mississippiensis individuals - appears to be a misidentified copepod in NCBI that throws off the assignment
  mutate(group = if_else(pop == "LW" & group == "Skistodiaptomus reighardi", "Skistodiaptomus mississippiensis", group)) %>% 
  drop_na(lat) %>% 
  ungroup() %>% 
  mutate(pop = factor(pop),
         pop = fct_reorder(pop, lat, .fun = mean))

species_prop_plot = new_samples %>%
  ggplot(aes(x = pop, fill = group)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = skisto_cols,
                    na.value = "black") + 
  labs(x = "Population", 
       y = "Proportion") + 
  theme_bw(base_size = 18) + 
  theme(panel.grid = element_blank())

ggsave(species_prop_plot, file="Output/Figures/species_prop_plot.pdf", h=3, w=16)
ggsave(species_prop_plot, file="Output/Figures/species_prop_plot.png", h=3, w=16)

clade_prop_plot = new_samples %>% 
  filter(group == "Skistodiaptomus pallidus") %>% 
  ggplot(aes(x = pop, fill = clade)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c(
    "S. pallidus - Clade A" = "#9CC6B2",
    "S. pallidus - Clade B" = "#208052",
    "S. pallidus - Clade C" = "#054F2C",
    "S. pallidus - Clade D" = "#04391F"
  )) +
  labs(x = "Population", 
       y = "Proportion") + 
  theme_bw(base_size = 18) + 
  theme(panel.grid = element_blank())

ggsave(clade_prop_plot, file="Output/Figures/clade_prop_plot.pdf", h=3, w=8)
ggsave(clade_prop_plot, file="Output/Figures/clade_prop_plot.png", h=3, w=8)

if(dim(checks)[1] > 0){
  print(paste0("You need to double check some of the clade IDs: ", paste(checks$sample, collapse = ", ")))
}else{
  print("All individuals assigned with some confidence")
}


