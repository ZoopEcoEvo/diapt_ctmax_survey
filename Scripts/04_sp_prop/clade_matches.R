suppressPackageStartupMessages({
  library(tidyverse)
})

blast_results = read.csv(file = "Output/COI_blast/acartia_blast.csv",
                         col.names = c("sample", "ref", "pident", "length", "mismatch", "gapopen", 
                                       "qstart", "qend", "sstart", "send", "evalue", "bitscore"))

ref_clades = read_tsv("Scripts/03_blast/labels.txt",  show_col_types = FALSE) %>% distinct()

unique_samples = unique(blast_results$sample)

clade_counts = blast_results %>% 
  filter(length > 300) %>% 
  filter(pident > 95) %>% 
  group_by(sample, ref) %>% 
  filter(evalue == min(evalue)) %>% 
  left_join(ref_clades, join_by(ref == label)) %>%  
  group_by(sample) %>%  
  count(Clade)

singles = clade_counts %>% 
  ungroup() %>% 
  group_by(sample) %>%  
  count() %>% 
  select(sample)
  
identified = clade_counts %>% 
  filter(sample %in% singles$sample) %>%  
  select(-n)

checks = clade_counts %>% 
  filter(!sample %in% singles$sample) %>% 
  bind_rows(data.frame("sample" = unique_samples[!unique_samples %in% identified$sample]))

new_samples = identified %>% 
  separate_wider_position(cols = sample,
                          widths = c("pop" = 2, "ind" = 2),
                          too_few = "align_start")

clade_prop_plot = new_samples %>%
  mutate(pop = fct_relevel(pop, c("TC", "GW", "MI"))) %>%
  ggplot(aes(x = pop, fill = Clade)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("A_hudsonica" = "#1B9E77",
                                "A_lilljeborgi" = "#D95F02",
                                "F" = "#7570B3",
                                "IV" = "#E7298A",
                                "out_group" = "#666666",
                                "S" = "#66A61E",
                                "SB" = "#E6AB02",
                                "X" = "#A6761D"),
                     na.value = "black") + 
  labs(x = "Population", 
       y = "Proportion") + 
  theme_bw(base_size = 18) + 
  theme(panel.grid = element_blank())

ggsave(clade_prop_plot, file="Output/Figures/clade_prop_plot.pdf", h=6, w=9)

if(dim(checks)[1] > 0){
  print(paste0("You need to double check some of the clade IDs: ", paste(checks$sample, collapse = ", ")))
}else{
  print("All individuals assigned with some confidence")
}


