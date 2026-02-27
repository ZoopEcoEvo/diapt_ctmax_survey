Diaptomid Thermal Limits GitHub Change Testing
================
2026-02-27

- [Site Map](#site-map)
- [CTmax in response to Coll. Temp. Generally and
  Specifically](#ctmax-in-response-to-coll-temp-generally-and-specifically)
- [CTmax in response to Coll. Temp.
  Specifically](#ctmax-in-response-to-coll-temp-specifically)
- [(Use one of these two, probably the second
  option)](#use-one-of-these-two-probably-the-second-option)

## Site Map

``` r
coords = ctmax_data %>%
  inner_join(site_data, by = c("site", "lat", "collection_temp")) %>% 
  dplyr::select(site, long, lat, collection_temp, elevation) %>%
  drop_na(collection_temp) %>% 
  distinct()

map_data("world") %>% 
  filter(region %in% c("USA", "Canada")) %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group),
               fill = "grey92", colour = "grey40", linewidth = 0.1) + 
  coord_map(xlim = c(-110,-60),
            ylim = c(25, 55)) + 
  geom_point(data = coords,
             mapping = aes(x = long, y = lat),
             size = 2) +
  labs(x = "Longitude", 
       y = "Latitude",
       colour = "Elev. (m)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/sampled-sites-1.png" style="display: block; margin: auto;" />

## CTmax in response to Coll. Temp. Generally and Specifically

``` r
###unsure of size yet
ctmax_temp_plot = ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = collection_temp, y = ctmax)) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point(size = 3) + 
  labs(x = "Collection Temp. (°C)", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

## CTmax in response to Coll. Temp. Specifically

## (Use one of these two, probably the second option)

``` r
ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = collection_temp, y = ctmax, colour = species)) + 
  facet_wrap(species~.) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point() + 
  labs(x = "Collection Temp. (°C)",
       y = "CTmax (°C)") + 
  scale_color_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r

###{r fig.width=10, fig.height=6}
ctmax_data %>% 
  filter(str_detect(species, pattern = "skisto") | 
           str_detect(species, pattern = "lepto") | 
           str_detect(species, pattern = "aglao")) %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  group_by(collection_date, species, collection_temp) %>% 
  summarise(mean_ctmax = mean(ctmax),
            ctmax_sd = sd(ctmax),
            ctmax_n = n(), 
            ctmax_se = ctmax_sd / sqrt(ctmax_n)) %>% 
  ggplot(aes(x = collection_temp, y = mean_ctmax, colour = species)) + 
  geom_smooth(method = "lm", se=F, linewidth = 2) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = mean_ctmax - ctmax_se, 
                    ymax = mean_ctmax + ctmax_se),
                width = 0.3, linewidth = 1) + 
  labs(x = "Collection Temp. (°C)",
       y = "CTmax (°C)") + 
  scale_colour_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-2-2.png" style="display: block; margin: auto;" />

\##S. pallidus egg volume

``` r
ctmax_data %>% 
  filter(species == "skistodiaptomus_pallidus") %>%
  mutate(site = fct_reorder(site, lat, .desc = T)) %>% 
  # group_by(site) %>% 
  # summarise(size = mean(size, na.rm = T), 
  #          total_egg_volume = mean(total_egg_volume, na.rm = T)) %>% 
  ggplot(aes(x = size, y = total_egg_volume)) + 
  geom_smooth(method = "lm", formula = y ~ exp(x), 
              colour = "black") + 
  geom_point(aes(colour = site))+
  scale_color_viridis_d(direction = 1, 
                        option = "D") + 
  labs(x = "Prosome Length (mm)",
       y = "Total Egg Volume (mm^3)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

\##F3 Body Size (Prosome Length)

``` r
f3_data %>%
  ggplot(aes(x = site, y = size)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  labs(x = "Site", 
       y = "Prosome Length (mm)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

\##F3 CTmax

``` r
f3_data %>%
ggplot(aes(x = site, y = ctmax)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  labs(x = "Site", 
       y = "CTmax (°C)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

\##F3 Clutch Size

``` r

ggplot(f3_data, aes(x = site, y = fecundity)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  labs(x = "Site", 
       y = "Clutch Size (eggs per female)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />
