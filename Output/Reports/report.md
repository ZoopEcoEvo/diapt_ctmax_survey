Diaptomid Thermal Limits
================
2025-12-04

- [Site Map](#site-map)
- [CTmax Data](#ctmax-data)
- [F3 CTmax data](#f3-ctmax-data)
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
             mapping = aes(x = long, y = lat),
             size = 2) +
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
  select(lat, collection_temp) %>% 
  distinct() %>% 
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
  #facet_wrap(.~species) + 
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
## <none>                        485.97  -4.365                       
## genus             2   138.079 624.04 117.677  70.6070 < 2.2e-16 ***
## collection_temp   1   105.341 591.31  92.519 107.7332 < 2.2e-16 ***
## lat               1   113.236 599.20  99.203 115.8076 < 2.2e-16 ***
## elevation         1     6.509 492.48   0.341   6.6572   0.01016 *  
## total_egg_volume  1    21.484 507.45  15.438  21.9722 3.578e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#MuMIn::dredge(ctmax_temp.model)

car::Anova(ctmax_overall.model)
## Anova Table (Type II tests)
## 
## Response: ctmax
##                  Sum Sq  Df  F value    Pr(>F)    
## genus            138.08   2  70.6070 < 2.2e-16 ***
## collection_temp  105.34   1 107.7332 < 2.2e-16 ***
## lat              113.24   1 115.8076 < 2.2e-16 ***
## elevation          6.51   1   6.6572   0.01016 *  
## total_egg_volume  21.48   1  21.9722 3.578e-06 ***
## Residuals        485.97 497                       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

performance::check_model(ctmax_overall.model)
```

<img src="../Figures/markdown/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

``` r

egg.model = lm(data = model_data, 
                         ctmax ~ collection_temp + total_egg_volume + size)

performance::check_model(egg.model)
```

<img src="../Figures/markdown/unnamed-chunk-11-2.png" style="display: block; margin: auto;" />

``` r

effectsize::effectsize(egg.model)
## # Standardization method: refit
## 
## Parameter        | Std. Coef. |        95% CI
## ---------------------------------------------
## (Intercept)      |   6.39e-16 | [-0.07, 0.07]
## collection temp  |       0.61 | [ 0.54, 0.68]
## total egg volume |       0.15 | [ 0.08, 0.23]
## size             |       0.37 | [ 0.30, 0.44]

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

<img src="../Figures/markdown/unnamed-chunk-11-3.png" style="display: block; margin: auto;" />

``` r

ctmax_temp.model = lm(data = model_data, 
                      ctmax ~ species + collection_temp)

drop1(ctmax_temp.model, 
      scope = ~.,
      test = "F")
## Single term deletions
## 
## Model:
## ctmax ~ species + collection_temp
##                 Df Sum of Sq    RSS      AIC F value    Pr(>F)    
## <none>                       328.04 -196.438                      
## species          8    587.82 915.86  305.034  110.65 < 2.2e-16 ***
## collection_temp  1    116.01 444.05  -45.823  174.70 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

performance::check_model(ctmax_temp.model)
```

<img src="../Figures/markdown/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r

sp_means = emmeans::emmeans(ctmax_temp.model, "species") %>% 
  data.frame() %>% 
  drop_na() %>%  
  select(species, "ctmax" = emmean, lower.CL, upper.CL)


sp_means %>% 
  mutate(species = str_replace(species, pattern = "_", replacement = " "),
         species = str_to_sentence(species),
         species = fct_reorder(species, .x = ctmax)) %>% 
ggplot(aes(x = species, y = ctmax, colour = species)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.5) + 
  scale_colour_manual(values = skisto_cols) + 
  theme_matt() + 
  theme(axis.text = element_text(angle = 300, hjust = 0, vjust = 0.5), 
        legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-12-2.png" style="display: block; margin: auto;" />

``` r
ctmax_data %>% 
  mutate(group_id = paste(site, species, collection_date)) %>% 
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
  mutate(group_id = paste(site, species, collection_date)) %>% 
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
  mutate(group_id = paste(site, species, collection_date)) %>% 
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

## F3 CTmax data

*Skistodiaptomus pallidus* was collected from three sites (Centennial
Park - CO, Ochsner Pond - OH, and Center Springs Pond - CT) were reared
in the lab at 16°C for at least three generations. CTmax was measured
for these copepods to test for genetic variation in thermal limits in
this widely distributed species.

Lab reared copepods varied in size, with Centennial Park individuals
~0.1 mm longer than those from Ochsner Pond.

``` r
f3_data %>%
  ggplot(aes(x = site, y = size)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  labs(x = "Site", 
       y = "Prosome Length (mm)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

``` r
f3_size.model = lme4::lmer(data = f3_data,
                      size ~ site + (1 | experiment_date))

#performance::check_model(f3_size.model)

car::Anova(f3_size.model)
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: size
##       Chisq Df Pr(>Chisq)    
## site 25.398  1  4.664e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Upper thermal limit did not vary between the populations.

``` r
f3_data %>%
ggplot(aes(x = site, y = ctmax)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  labs(x = "Site", 
       y = "CTmax (°C)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r

f3_ctmax.model = lme4::lmer(data = f3_data, 
                      ctmax ~ site + (1|experiment_date) + (1|tube))

#performance::check_model(f3_ctmax.model)

car::Anova(f3_ctmax.model)
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: ctmax
##      Chisq Df Pr(>Chisq)
## site 0.827  1     0.3631
```

``` r
f3_data %>% 
  group_by(experiment_date, site) %>% 
  summarise(mean_ctmax = mean(ctmax), 
            se_ctmax = sd(ctmax) / sqrt(n())) %>% 
ggplot(aes(experiment_date, y = mean_ctmax, colour = site, group = site)) + 
  geom_point(data = f3_data,
             aes(x = experiment_date, y = ctmax, colour = site),
             size = 1, alpha = 0.3,
             position = position_jitterdodge(jitter.height = 0, jitter.width = 0.05, 
                                             dodge.width = 0.3)) + 
    geom_line(linewidth = 1.3,
              position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(ymin = mean_ctmax - se_ctmax, ymax = mean_ctmax + se_ctmax),
                position = position_dodge(width = 0.3),
                width = 0.25, linewidth = 1) + 
    geom_point(size = 3,
               position = position_dodge(width = 0.3)) + 
  labs(x = "Experiment Date", 
       y = "CTmax (°C)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

Fecundity also appears to vary between populations, even after rearing
in lab for several generations, although this difference does not appear
to be significant.

``` r

ggplot(f3_data, aes(x = site, y = fecundity)) + 
  geom_boxplot() + 
  geom_point(position = position_jitter(height = 0, width = 0.1)) + 
  labs(x = "Site", 
       y = "Clutch Size (eggs per female)") + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

``` r

# f3_fecund.model = glm(data = f3_data, 
#                       fecundity ~ site,
#                       family="poisson")

f3_fecund.model = lme4::glmer(data = f3_data, 
                      fecundity ~ site + (1|experiment_date),
                      family="poisson")

# performance::check_model(f3_fecund.model)

car::Anova(f3_fecund.model)
## Analysis of Deviance Table (Type II Wald chisquare tests)
## 
## Response: fecundity
##       Chisq Df Pr(>Chisq)
## site 2.1596  1     0.1417
```

To summarize the initial findings, Centennial Park copepods had larger
body sizes but smaller clutch sizes than copepods from Ochsner Pond.
CTmax was similar between the two populations.

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

<img src="../Figures/markdown/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

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

<img src="../Figures/markdown/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

``` r
scan_sizes %>% 
  filter(sex == "female", stage == "adult") %>%
  inner_join(site_data) %>% 
  ggplot(aes(x = collection_temp, y = length)) + 
  geom_point(position = position_jitter(width = 0.08, height = 0)) + 
  theme_matt()
```

<img src="../Figures/markdown/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

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
