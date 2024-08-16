
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggtree)
  library(treeio)
})

tree <- read.nexus(file="./Output/Sequences/COItree/skisto_mb.nex.con.tre")

x <- as_tibble(tree) %>% 
  mutate(group = str_split_fixed(label, pattern = "_", n = 2)[,2], 
         group = str_replace(group, "_", " "),
         group = if_else(group == "", "New", group))

pout = ggtree(tree) %<+% x + 
  geom_tiplab(aes(color=group), size=0.9) +
  geom_tippoint(aes(color=group), size=1.2) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  scale_color_manual(values = c("Skistodiaptomus reighardi" = "#8DAB7F",
                                "Skistodiaptomus pallidus" = "#A77698",
                                "Skistodiaptomus oregonensis" = "#F8C425",
                                "Skistodiaptomus mississippiensis" = "#AF4308",
                                "Skistodiaptomus pygmaeus" = "#14A3A1",
                                "Skistodiaptomus carolinensis" = "#F78A50"),
                     na.value = "black") + 
  labs(colour = "Species") + 
  theme_tree() + theme(legend.position=c(0.8, 0.2))



ggsave(pout, file="./Output/Figures/tree_plot.pdf", h=14, w=7) 

