suppressPackageStartupMessages({
  library(tidyverse)
})

blast_results = read.csv(file = "Output/Sequences/COI_blast/skisto_blast.csv",
                         col.names = c("sample", "ref", "pident", "length", "mismatch", "gapopen", 
                                       "qstart", "qend", "sstart", "send", "evalue", "bitscore"))

ref_clades = blast_results %>%  
  mutate(group = str_split_fixed(ref, pattern = "_", n = 2)[,2], 
         group = str_replace(group, "_", " ")) %>% 
  select(ref, group) %>% 
  distinct()

unique_samples = unique(blast_results$sample)

clade_counts = blast_results %>% 
  filter(length > 300) %>% 
  filter(pident > 90) %>% 
  group_by(sample, ref) %>% 
  filter(evalue == min(evalue)) %>% 
  mutate(group = str_split_fixed(ref, pattern = "_", n = 2)[,2], 
         group = str_replace(group, "_", " ")) %>% 
  ungroup() %>% 
  select(sample, group) %>% 
  distinct() %>% 
  group_by(sample) %>%  
  count(group)

table(clade_counts$sample)

singles = clade_counts %>% 
  ungroup() %>% 
  group_by(sample) %>%  
  count() %>% 
  filter(n == 1) %>% 
  select(sample)

identified = clade_counts %>% 
  filter(sample %in% singles$sample) %>%  
  select(-n)

checks = clade_counts %>% 
  filter(!sample %in% singles$sample) %>% 
  bind_rows(data.frame("sample" = unique_samples[!unique_samples %in% identified$sample]))

manual_ids = data.frame(
  "sample" = c("LW8", "Py2", "Py3", "Py4", "Py5", "Py6", "OwP", "OwP1", "OwP2")) %>% 
  mutate("group" = if_else(sample == "LW8", "Skistodiaptomus reighardi", "Skistodiaptomus mississippiensis"))

new_samples = identified %>% 
  bind_rows(manual_ids) %>% 
  separate_wider_position(cols = sample,
                          widths = c("pop" = 2, "ind" = 2),
                          too_few = "align_start") %>% 
  left_join(extractions, join_by(pop == Code)) %>% 
  mutate(pop = factor(pop),
         pop = fct_reorder(pop, lat, .fun = mean))

clade_prop_plot = new_samples %>%
  ggplot(aes(x = pop, fill = group)) +
  geom_bar(position = "fill") + 
  scale_y_continuous(breaks = c(0, 0.5, 1)) + 
  scale_fill_manual(values = c("Skistodiaptomus reighardi" = "#8DAB7F",
                                "Skistodiaptomus pallidus" = "#A77698",
                                "Skistodiaptomus oregonensis" = "#F8C425",
                                "Skistodiaptomus mississippiensis" = "#AF4308",
                                "Skistodiaptomus pygmaeus" = "#14A3A1",
                                "Skistodiaptomus carolinensis" = "#F78A50"),
                    na.value = "black") + 
  labs(x = "Population", 
       y = "Proportion") + 
  theme_bw(base_size = 18) + 
  theme(panel.grid = element_blank())

ggsave(clade_prop_plot, file="Output/Figures/clade_prop_plot.pdf", h=3, w=12)

if(dim(checks)[1] > 0){
  print(paste0("You need to double check some of the clade IDs: ", paste(checks$sample, collapse = ", ")))
}else{
  print("All individuals assigned with some confidence")
}


