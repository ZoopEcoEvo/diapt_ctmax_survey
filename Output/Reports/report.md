Diaptomid Thermal Limits
================
2025-05-05

- [Site Map](#site-map)
- [CTmax Data](#ctmax-data)
- [High throughput size
  measurements](#high-throughput-size-measurements)
- [COI Barcoding](#coi-barcoding)

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
             mapping = aes(x = long, y = lat, colour = elevation),
             size = 3) +
  scale_colour_viridis_c(option = "G", direction = -1) + 
  labs(x = "Longitude", 
       y = "Latitude",
       colour = "Elev. (m)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/sampled-sites-1.png" style="display: block; margin: auto;" />

## CTmax Data

``` r
temp_lat_plot = ctmax_data %>% 
  ggplot(aes(x = lat, y = collection_temp)) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point(size = 3) + 
  labs(x = "Latitude", 
       y = "Collection Temp. (°C)") + 
  theme_matt() + 
  theme(legend.position = "right")

ctmax_temp_plot = ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = collection_temp, y = ctmax)) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point(aes(colour = species), 
             size = 3) + 
  labs(x = "Collection Temp. (°C)", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "right")

ctmax_lat_plot = ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = lat, y = ctmax)) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point(aes(colour = species), 
             size = 3) + 
  labs(x = "Latitude", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "right")

ctmax_elev_plot = ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = elevation, y = ctmax)) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point(aes(colour = species), 
             size = 3) +
  labs(x = "Elevation (m)", 
       y = "CTmax (°C)") +
  scale_colour_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "right")

ggpubr::ggarrange(temp_lat_plot, ctmax_temp_plot, ctmax_lat_plot, ctmax_elev_plot, common.legend = T, legend = "right", nrow = 2, ncol = 2, labels = "AUTO")
```

<img src="../Figures/markdown/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

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

<img src="../Figures/markdown/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = collection_temp, y = size, colour = species)) + 
  facet_wrap(species~.) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point() + 
  labs(x = "Collection Temp. (°C)",
       y = "Prosome Length (mm)") + 
  scale_color_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = collection_temp, y = egg_volume, colour = species)) + 
  facet_wrap(species~.) + 
  geom_smooth(method = "lm", colour = "black") + 
  geom_point() + 
  labs(x = "Collection Temp. (°C)",
       y = "Egg Volume (mm^3)") + 
  scale_color_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  select(elevation, collection_temp) %>% 
  distinct() %>% 
  ggplot(aes(x = elevation, y = collection_temp)) + 
  geom_point(size = 3) +
  labs(x = "Elevation (m)", 
       y = "Collection Temp. (°C)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = size, y = ctmax, colour = species)) + 
  facet_wrap(.~species) + 
  geom_point(size = 1) + 
  theme_matt() + 
  labs(x = "Length (mm)", 
       y = "CTmax (°C)") + 
  scale_colour_manual(values = skisto_cols) + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(species = str_replace(species, "_", " "),
         species = str_to_sentence(species)) %>% 
  ggplot(aes(x = size, y = fecundity, colour = species)) + 
  facet_wrap(.~species) + 
  geom_point(size = 1) + 
  theme_matt() + 
  labs(x = "Length (mm)", 
       y = "Fecundity (# eggs)") + 
  scale_colour_manual(values = skisto_cols) + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
ggplot(ctmax_data, aes(x = size, y = total_egg_volume)) + 
  geom_smooth(method = "lm", formula = y ~ exp(x)) + 
  geom_point()+
  labs(x = "Prosome Length (mm)",
       y = "Total Egg Volume (mm^3)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

Data for just *Skistodiaptomus pallidus* is shown below. Point color is
arranged according to latitude.

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

<img src="../Figures/markdown/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
model_data = ctmax_data %>% 
  mutate("genus" = str_split_fixed(species, pattern = "_", n = 2)[,1],
         genus = tools::toTitleCase(genus),
         "doy" = yday(collection_date)) %>% 
  select(site, collection_date, doy, collection_temp, lat, elevation, species, genus, sample_id, fecundity, total_egg_volume, size, ctmax) %>% 
  filter(genus != "MH") %>%  
  mutate(total_egg_volume = if_else(is.na(total_egg_volume), 0, total_egg_volume),
         collection_temp_sc = scale(collection_temp),
         lat_sc = scale(lat), 
         elevation_sc = scale(elevation),
         tev_sc = scale(total_egg_volume)) 

ctmax_overall.model = lm(data = model_data, 
                      ctmax ~ genus + collection_temp + lat + elevation + total_egg_volume)

drop1(ctmax_overall.model, test = "F")
## Single term deletions
## 
## Model:
## ctmax ~ genus + collection_temp + lat + elevation + total_egg_volume
##                  Df Sum of Sq    RSS     AIC  F value    Pr(>F)    
## <none>                        484.53   4.442                       
## genus             2   137.500 622.03 123.847  69.1001 < 2.2e-16 ***
## collection_temp   1   104.850 589.38  99.212 105.3842 < 2.2e-16 ***
## lat               1   108.901 593.43 102.596 109.4558 < 2.2e-16 ***
## elevation         1     5.316 489.85   7.832   5.3428   0.02122 *  
## total_egg_volume  1    21.492 506.03  23.881  21.6011 4.327e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#MuMIn::dredge(ctmax_temp.model)

performance::check_model(ctmax_overall.model)
```

<img src="../Figures/markdown/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r

summary(ctmax_overall.model)
## 
## Call:
## lm(formula = ctmax ~ genus + collection_temp + lat + elevation + 
##     total_egg_volume, data = model_data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.9283 -0.4896  0.1466  0.6709  2.3815 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          42.8587976  1.1049026  38.790  < 2e-16 ***
## genusLeptodiaptomus  -2.7980488  0.2819552  -9.924  < 2e-16 ***
## genusSkistodiaptomus -1.5100869  0.2734257  -5.523 5.44e-08 ***
## collection_temp       0.1644759  0.0160219  10.266  < 2e-16 ***
## lat                  -0.2069105  0.0197771 -10.462  < 2e-16 ***
## elevation            -0.0002194  0.0000949  -2.311   0.0212 *  
## total_egg_volume     40.4221043  8.6972342   4.648 4.33e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9975 on 487 degrees of freedom
## Multiple R-squared:  0.5978, Adjusted R-squared:  0.5929 
## F-statistic: 120.6 on 6 and 487 DF,  p-value: < 2.2e-16

emmeans::emmeans(ctmax_overall.model, specs = "genus") %>% 
  data.frame() %>% 
  mutate(genus = fct_reorder(genus, .x = emmean, .desc = T)) %>% 
  ggplot(aes(genus, y = emmean)) + 
  geom_point(size = 4) + 
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                width = 0.2, linewidth = 1) + 
  labs(x = "") + 
  theme_matt() + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = 0.5))
```

<img src="../Figures/markdown/unnamed-chunk-11-2.png" style="display: block; margin: auto;" />

``` r

ctmax_temp.model = lm(data = model_data, 
                      ctmax ~ species * collection_temp)

drop1(ctmax_temp.model, 
      scope = ~.,
      test = "F")
## Single term deletions
## 
## Model:
## ctmax ~ species * collection_temp
##                         Df Sum of Sq    RSS     AIC F value    Pr(>F)    
## <none>                               324.50 -179.60                      
## species                  4   15.6893 340.19 -164.28  5.8019 0.0001446 ***
## collection_temp          1    1.2044 325.70 -179.77  1.7816 0.1825843    
## species:collection_temp  4    2.9343 327.43 -183.16  1.0851 0.3631881    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

performance::check_model(ctmax_temp.model)
```

<img src="../Figures/markdown/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r

sp_means = emmeans::emmeans(ctmax_temp.model, "species") %>% 
  data.frame() %>% 
  drop_na() %>%  
  select(species, "ctmax" = emmean)

sp_plast = emmeans::emtrends(ctmax_temp.model, "species", var = "collection_temp") %>% 
  data.frame() %>% 
  drop_na() %>% 
  mutate(species = fct_reorder(species, .x = collection_temp.trend, .desc = T)) %>% 
  select(species, "plast" = collection_temp.trend)

sp_means %>% 
inner_join(sp_plast) %>% 
  mutate(species = fct_reorder(species, plast, .fun = min)) %>% 
  ggplot(aes(x = species, y = plast)) + 
  geom_hline(yintercept = 0) + 
  geom_bar(stat = "identity") + 
  theme_matt() + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = 0.5))
```

<img src="../Figures/markdown/unnamed-chunk-12-2.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(group_id = paste(site, species)) %>% 
  ggplot(aes(x = fecundity, y = site, fill = site)) + 
  geom_density_ridges(bandwidth = 2,
                      jittered_points = TRUE, 
                      point_shape = 21,
                      point_size = 1,
                      point_colour = "grey30",
                      point_alpha = 0.6,
                      alpha = 0.9,
                      position = position_points_jitter(
                        height = 0.1, width = 0)) + 
  scale_fill_viridis_d(option = "D", direction = -1) + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/fecundity-ridges-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(group_id = paste(site, species)) %>% 
  ggplot(aes(x = size, y = site, fill = site, group = group_id)) + 
  geom_density_ridges(bandwidth = 0.02,
                      jittered_points = TRUE, 
                      point_shape = 21,
                      point_size = 1,
                      point_colour = "grey30",
                      point_alpha = 0.6,
                      alpha = 0.9,
                      position = position_points_jitter(
                        height = 0.1, width = 0)) + 
  scale_fill_viridis_d(option = "D", direction = -1) + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/size-ridges-1.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(group_id = paste(site, species)) %>% 
  ggplot(aes(x = ctmax, y = site, fill = site, group = group_id)) + 
  geom_density_ridges(bandwidth = 0.3,
                      jittered_points = TRUE, 
                      point_shape = 21,
                      point_size = 1,
                      point_colour = "grey30",
                      point_alpha = 0.6,
                      alpha = 0.9,
                      position = position_points_jitter(
                        height = 0.1, width = 0)) + 
  scale_fill_viridis_d(option = "D", direction = -1) + 
  labs(x = "CTmax (°C)") + 
  theme_matt() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/ctmax-ridges-1.png" style="display: block; margin: auto;" />

## High throughput size measurements

``` r
scan_sizes %>% 
  filter(sex == "female", stage == "adult") %>% 
  ggplot(aes(x = length, fill = species)) + 
  facet_wrap(site~., nrow = 3) + 
  geom_histogram(binwidth = 0.01, colour = "grey10", linewidth = 0.25) + 
  scale_fill_manual(values = skisto_cols) + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")
```

<img src="../Figures/markdown/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

``` r

scan_sizes %>% 
  filter(sex == "female", stage == "adult") %>%
  ggplot(aes(x = length, y = site, fill = species)) + 
  geom_density_ridges(bandwidth = 0.02,
                      jittered_points = TRUE,
                      position = position_points_jitter(yoffset = -0.15, width = 0, height = 0.1),
                      point_alpha = 0.3, point_colour = "grey30")  + 
  labs(y = "",
       x = "Prosome Length (mm)") + 
    scale_fill_manual(values = skisto_cols) + 
  theme_minimal(base_size = 20) + 
  theme(panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")
```

<img src="../Figures/markdown/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

## COI Barcoding

``` r
knitr::include_graphics("../Figures/species_prop_plot.png")
```

<img src="../Figures/species_prop_plot.png" width="4800" style="display: block; margin: auto;" />

``` r
knitr::include_graphics("../Figures/clade_prop_plot.png")
```

<img src="../Figures/clade_prop_plot.png" width="2400" style="display: block; margin: auto;" />

``` r
knitr::include_graphics("../Figures/tree_plot.png")
```

<img src="../Figures/tree_plot.png" width="2100" style="display: block; margin: auto;" />
