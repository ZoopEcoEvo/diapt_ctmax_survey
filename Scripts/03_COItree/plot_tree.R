
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtree)
  library(treeio)
})

tree <- read.nexus(file="./Output/Sequences/COItree/skisto_mb.nex.con.tre")

group_labels <- as_tibble(tree) %>% 
  mutate(group = str_split_fixed(label, pattern = "_", n = 2)[,2], 
         group = if_else(str_detect(label, "FR"), "Leptodiaptomus nudus", group),
         group = str_replace(group, "_", " "),
         group = if_else(str_detect(group, "sicilis"), "Leptodiaptomus sicilis", group),
         group = if_else(group == "", "New", group),
         group = if_else(label %in% c("MZ964920.1_Skistodiaptomus_reighardi", "MZ964919.1_Skistodiaptomus_reighardi"), "Skistodiaptomus mississippiensis", group))

# group_labels %>% as.data.frame() %>% 
#   filter(group != "New") %>% 
#   arrange(group) %>% 
#   write.csv(file = "Output/Sequences/ref_clades.csv", row.names = F)

pout = ggtree(tree) %<+% group_labels + 
  geom_tiplab(aes(color=group), size=0.9) +
  geom_tippoint(aes(color=group), size=1.2) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  scale_color_manual(values = c("Skistodiaptomus reighardi" = "#114264",
                                "Skistodiaptomus pallidus" = "#7DB979",
                                "Skistodiaptomus oregonensis" = "#F8C425",
                                "Skistodiaptomus mississippiensis" = "#B6D0E2",
                                "Skistodiaptomus pygmaeus" = "#4682B4",
                                "Skistodiaptomus carolinensis" = "#A3A3A3",
                                "Leptodiaptomus minutus" = "#DEBABF", 
                                "Leptodiaptomus nudus" = "#C4828B",
                                "Leptodiaptomus sicilis" = "#F78A50",
                                "Aglaodiaptomus spatulocrenatus" = "#AF4308"),
                     na.value = "black") + 
  labs(colour = "Species") + 
  theme_tree() + theme(legend.position=c(0.2, 0.9))



ggsave(pout, file="./Output/Figures/tree_plot.pdf", h=14, w=7) 
ggsave(pout, file="./Output/Figures/tree_plot.png", h=14, w=7) 

