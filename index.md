---
title: "index"
author: "Liz Loutrage"
format: 
  html:
    self-contained: true
    code-fold: true
editor: source
theme: minty
keep-md: true
execute:
  warning: false
  message : false
toc: true
toc-title: Sections
toc-location: left
page-layout: full
---



# Data load & preparation

::: {.cell}

```{.r .cell-code}
# Library ----
library(tidyr)
library(dplyr)
library(ggplot2)

# Isotopic data set ----
isotopic_dataset <-
  readxl::read_excel(here::here(
    "data",
    "C_N_isotopes_mesopelagic_2007-2021_BayofBiscay.xlsx"
  ),
  3) %>%
  # rename columns for simplicity
  rename(trawling_depth = `Trawling depth [m]`,
         size = `Standard length [cm] (individuals, mean)`,
         species = Taxa)%>%
  # create d15N column, when d15N untreated unavailable we take d15N cyclohexane-delipidated
  mutate(d15N= coalesce(`δ15N (untreated)`,`δ15N (cyclohexane-delipidated)`))

# Trawling data set ----
trawling_dataset <-
  readxl::read_excel(here::here(
    "data",
    "Length_depth_distributions_mesopelagic_2011-2021_BayofBiscay.xlsx"
  ),
  3) %>%
  # rename columns for simplicity
  rename(trawling_depth = `Trawling depth [m]`,
         size = `Total length [cm]`,
         species = Species)%>%
  # add column with depth layer (cf Loutrage et al., 2023)
  mutate(
    depth_layer = case_when(
      between(trawling_depth, 0, 174) ~ "Epipelagic",
      between(trawling_depth, 175, 699) ~ "Upper mesopelagic",
      between(trawling_depth, 700, 999) ~ "Lower mesopelagic",
      between(trawling_depth, 1000, 2000) ~ "Bathypelagic")) %>%
  mutate(across(depth_layer, factor, levels = c("Epipelagic", "Upper mesopelagic", "Lower mesopelagic", "Bathypelagic")))
```
:::



# 1. Relationships between size and depth

## Community level
- Bigger fish at depth? 

::: {.cell}

```{.r .cell-code}
# median size in each depth layer
median_size_data <- trawling_dataset %>%
  group_by(depth_layer) %>%
  summarise(median_size = median(size)) %>%
  mutate(across(
    depth_layer,
    factor,
    levels = c(
      "Epipelagic",
      "Upper mesopelagic",
      "Lower mesopelagic",
      "Bathypelagic"
    )
  ))

# plot
ggplot(trawling_dataset, aes(x = size)) +
  geom_density(
    alpha = 0.5,
    linewidth = 0.6,
    bw = 0.2,
    aes(col = depth_layer, fill = depth_layer)) +
  scale_fill_manual(values = c("#93C3FF", "#6799D3", "#3A72A8", "#002A58")) +
  scale_color_manual(values = c("#93C3FF", "#6799D3", "#3A72A8", "#002A58")) +
  facet_wrap( ~ depth_layer, ncol = 1, scales = "free_y") +
  theme_minimal() +
  scale_x_continuous(trans = 'log2') +
  theme(
    strip.text.x = element_text(size = 9, face = "bold"),
    strip.text.y = element_text(size = 9),
    axis.title = element_text(size = 9),
    axis.text =  element_text(size = 9, color = "grey50"),
    panel.background = element_rect(color = "white"),
    plot.background = element_rect(color = "white"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9)) +
  guides(fill = "none", col = "none") +
  xlab("Total length (cm)") +
  geom_vline(
    data = median_size_data,
    aes(xintercept = median_size, color = depth_layer),
    linewidth = 0.6,
    linetype = "dashed")
```

::: {.cell-output-display}
![](index_files/figure-html/size_depth_community-1.png){width=672}
:::

```{.r .cell-code}
# store the plot in the "figures" file in high resolution
ggsave("density_plot_community.png", path = "figures", dpi = 700, height = 6, width = 6)
```
:::


### Linear relationship

::: {.cell}

```{.r .cell-code}
lr_size_depth_community <- trawling_dataset %>%
  do(broom::tidy(lm(size ~ trawling_depth, .))) %>%
  mutate(across(where(is.numeric), round, 2))

htmltools::tagList(DT::datatable(lr_size_depth_community))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-c08d68c6079c56ebe944" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-c08d68c6079c56ebe944">{"x":{"filter":"none","vertical":false,"data":[["1","2"],["(Intercept)","trawling_depth"],[8.91,0],[0.4,0],[22.4,7.34],[0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>term<\/th>\n      <th>estimate<\/th>\n      <th>std.error<\/th>\n      <th>statistic<\/th>\n      <th>p.value<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


## Species level
### Linear relationships

#### Regressions

::: {.cell}

```{.r .cell-code}
lr_size_depth_sp <- trawling_dataset %>%
  group_by(species) %>%
  do(broom::tidy(lm(size ~ trawling_depth, .))) %>%
  mutate(across(where(is.numeric), round, 2))

htmltools::tagList(DT::datatable(lr_size_depth_sp))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-75b8e3d2945d829d6def" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-75b8e3d2945d829d6def">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"],["Aphanopus carbo","Aphanopus carbo","Arctozenus risso","Arctozenus risso","Argyropelecus olfersii","Argyropelecus olfersii","Lampanyctus crocodilus","Lampanyctus crocodilus","Lampanyctus macdonaldi","Lampanyctus macdonaldi","Melanostigma atlanticum","Melanostigma atlanticum","Myctophum punctatum","Myctophum punctatum","Notoscopelus kroyeri","Notoscopelus kroyeri","Searsia koefoedi","Searsia koefoedi","Serrivomer beanii","Serrivomer beanii","Stomias boa","Stomias boa","Xenodermichthys copei","Xenodermichthys copei"],["(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth","(Intercept)","trawling_depth"],[75.64,0.01,17.54,-0,6.7,0,9.6,0,14.12,0,3.55,0,7.06,-0,7.52,-0,12.22,0,53.92,0,25.09,0,9.09,0],[8.32,0.01,0.5,0,0.29,0,0.19,0,0.5600000000000001,0,0.87,0,0.14,0,0.2,0,1.25,0,3.52,0,1.99,0,0.32,0],[9.09,1.53,34.75,-1.21,22.99,0.38,50.14,7.92,25.12,0.52,4.09,5.21,50.63,-2.06,38.29,-0.11,9.800000000000001,0.17,15.33,0.17,12.58,0.08,28.86,3.36],[0,0.14,0,0.23,0,0.71,0,0,0,0.61,0,0,0,0.04,0,0.91,0,0.86,0,0.87,0,0.9399999999999999,0,0]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>term<\/th>\n      <th>estimate<\/th>\n      <th>std.error<\/th>\n      <th>statistic<\/th>\n      <th>p.value<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[3,4,5,6]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


### Density plot for significant relationships at the species level

::: {.cell}

```{.r .cell-code}
# selection of species with a significant size-depth relationship (from linear relationships)
trawling_dataset_significant <- filter(trawling_dataset,
                                    species %in% c("Lampanyctus crocodilus","Melanostigma atlanticum",
                                                   "Xenodermichthys copei","Myctophum punctatum"))

trawling_dataset_significant$species <- factor(trawling_dataset_significant$species,
                                    levels = c("Lampanyctus crocodilus",
                                             "Melanostigma atlanticum",
                                             "Xenodermichthys copei",
                                             "Myctophum punctatum"))

# Median depth
median_size_species <- trawling_dataset_significant %>%
  group_by(species, depth_layer) %>%
  summarise(median_size_sp = median(size))

# plot
ggplot(trawling_dataset_significant, aes(x=size)) +
  geom_density(alpha=0.3, linewidth=0.5, adjust= 2, aes(fill= depth_layer, col= depth_layer))+
  scale_fill_manual(values = c("#93C3FF", "#6799D3","#3A72A8", "#002A58"))+
  scale_color_manual(values = c("#93C3FF", "#6799D3","#3A72A8", "#002A58"))+
  facet_grid(depth_layer~species, scale="free")+
  theme_minimal()+
  theme(strip.text.x = element_text(size=12,face="italic"),
        strip.text.y = element_text(size=11),
        axis.title = element_text(size=13),
        axis.text =  element_text(size=10, color= "grey50"),
        panel.background=element_rect(color="white"),
        plot.background = element_rect(color = "white"),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))+
  guides(fill="none",col="none")+
  xlab("Total length (cm)")+
  geom_vline(data =median_size_species, aes(xintercept = median_size_sp, color=depth_layer),
             linewidth=0.6, linetype="dashed")
```

::: {.cell-output-display}
![](index_files/figure-html/size_depth_species-1.png){width=1248}
:::

```{.r .cell-code}
# store the plot in the "figures" file in high resolution
ggsave("density_plot_species.png", path = "figures", dpi = 700, height = 7, width = 9)
```
:::


# 2. $\delta$<sup>15</sup>N-size relationships

__Latitude-depth influence on $\delta$<sup>15</sup>N values__
- No significant influence of latitude on $\delta$<sup>15</sup>N values

::: {.cell}

```{.r .cell-code}
isotopic_varpart <- isotopic_dataset%>%
  na.omit()

# Perform variation partitioning analysis
part <- vegan::varpart(isotopic_varpart$d15N,
                               ~ trawling_depth, ~ LatitudeEvent, data = isotopic_varpart)

# Create the specified plot for the current species
plot(
  part,
  Xnames = c("Depth", "Latitude"),
  # name the partitions
  bg = c("#072AC8", "#0B5345"),
  alpha = 100,
  # colour the circles
  digits = 2,
  # only show 2 digits
  cex = 1.4
)
```

::: {.cell-output-display}
![](index_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::

```{.r .cell-code}
rda1 <- vegan::rda(isotopic_varpart$d15N,
                   isotopic_varpart$trawling_depth,
                   isotopic_varpart$LatitudeEvent)

rda2 <- vegan::rda(isotopic_varpart$d15N,
                   isotopic_varpart$LatitudeEvent,
                   isotopic_varpart$trawling_depth)

vegan::anova.cca(rda1)
```

::: {.cell-output .cell-output-stdout}
```
Permutation test for rda under reduced model
Permutation: free
Number of permutations: 999

Model: rda(X = isotopic_varpart$d15N, Y = isotopic_varpart$trawling_depth, Z = isotopic_varpart$LatitudeEvent)
          Df Variance      F Pr(>F)
Model      1  0.01003 2.7091  0.122
Residual 172  0.63668              
```
:::

```{.r .cell-code}
vegan::anova.cca(rda2)
```

::: {.cell-output .cell-output-stdout}
```
Permutation test for rda under reduced model
Permutation: free
Number of permutations: 999

Model: rda(X = isotopic_varpart$d15N, Y = isotopic_varpart$LatitudeEvent, Z = isotopic_varpart$trawling_depth)
          Df Variance      F Pr(>F)
Model      1  0.00097 0.2614  0.619
Residual 172  0.63668              
```
:::
:::


## At community level

::: {.cell}

```{.r .cell-code}
# plot
ggplot(isotopic_dataset , aes(x=size, y=d15N))+
  geom_point (alpha=0.4, size=1) + 
  geom_smooth(method=lm, se=T, alpha=0.2, col= alpha("darkblue",0.7)) + 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        label.x.npc = "right",
                        label.y.npc = "bottom",
                        vstep = -0.0005)+ 
  scale_x_continuous(trans = 'log2') +
  xlab("Standard length (cm)")+
  ylab(expression({delta}^15*N~'\u2030'))+
  ylim(c(7, 14))+
  guides(fill="none")+
  theme_minimal()+
  theme(strip.text.x = element_text(size=12,face="italic"),
        axis.title = element_text(size=12),
        axis.text =  element_text(size=12),
        plot.background = element_rect(colour = "white"))
```

::: {.cell-output-display}
![](index_files/figure-html/d15n_size_community-1.png){width=672}
:::

```{.r .cell-code}
# store the plot in the "figures" file in high resolution 
ggsave("d15n_size_community.png", path = "figures", dpi = 700)
```
:::


## At species level
### Data summary

::: {.cell}

```{.r .cell-code}
data_sum <- isotopic_dataset %>%
  group_by(species) %>%
  summarise(
    range_size = max(size) - min(size),
    min_size = min(size),
    max_size = max(size),
    mean_size = mean(size),
    mean_d15n = mean(d15N),
    range_depth = max(trawling_depth) - min(trawling_depth)) %>%
  mutate(across(where(is.numeric), round, 2))

htmltools::tagList(DT::datatable(data_sum))
```

::: {.cell-output-display}

```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-f0f95d047eb0fe25e50b" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-f0f95d047eb0fe25e50b">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12"],["Aphanopus carbo","Arctozenus risso","Argyropelecus olfersii","Lampanyctus crocodilus","Lampanyctus macdonaldi","Melanostigma atlanticum","Myctophum punctatum","Notoscopelus kroyeri","Searsia koefoedi","Serrivomer beanii","Stomias boa","Xenodermichthys copei"],[37,9.5,6.7,8.300000000000001,3.3,4,4,8.1,6,45,21.8,13.9],[59,11,3.3,6.5,11.5,7,5,3.6,8.5,26.7,11.8,5.6],[96,20.5,10,14.8,14.8,11,9,11.7,14.5,71.7,33.6,19.5],[77.33,16.48,6.3,10.95,13.13,9.65,6.73,7.86,11.69,55.36,23.8,11.78],[12.36,10.53,10.18,10.46,11.54,11.21,9.99,11.18,11.8,9.49,11.61,9.83],[30,409,963,1229,666,278,1309,755,247,618,100,963]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>species<\/th>\n      <th>range_size<\/th>\n      <th>min_size<\/th>\n      <th>max_size<\/th>\n      <th>mean_size<\/th>\n      <th>mean_d15n<\/th>\n      <th>range_depth<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

:::
:::


### Linear relationships 
#### Plot linear relationships 

-  __A__: significant relationships 
-  __B__: non-significant relationships 


::: {.cell}

```{.r .cell-code}
# function to calculate coefficient of variation 
cv <- function(x) {
  (sd(x) / mean(x)) * 100
}

# Selection of species presenting significant d15N-size relationship
ontogeny_significant <- filter(
  isotopic_dataset,
  species %in% c(
    "Lampanyctus crocodilus",
    "Aphanopus carbo",
    "Melanostigma atlanticum",
    "Serrivomer beanii",
    "Stomias boa",
    "Myctophum punctatum",
    "Arctozenus risso"
  )
)
# Order species
ontogeny_significant$species <- factor(
  ontogeny_significant$species,
  levels = c(
    "Myctophum punctatum",
    "Melanostigma atlanticum",
    "Lampanyctus crocodilus",
    "Stomias boa",
    "Serrivomer beanii",
    "Aphanopus carbo",
    "Arctozenus risso"
  )
)

# plot of significant relationships
coeff_var_s <- aggregate(d15N ~ species,
                       data = ontogeny_significant,
                       FUN = cv)
coeff_var_s$d15N <- round(coeff_var_s$d15N, digits = 2)

# plot of significant relationships
plot_significant <- ggplot(ontogeny_significant, aes(x=size, y=d15N))+
  geom_point (alpha=0.4, col="black") + 
  geom_smooth(method=lm, se=T, alpha=0.2, col= alpha("darkblue",0.7)) +
  facet_wrap(~species, scale="free_x", ncol=3)+ 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=3.38,
                        label.x.npc = "right",
                        label.y.npc = "bottom",
                        vstep = -0.0005)+ 
  xlab("")+
  ylab(expression({delta}^15*N~'\u2030'))+
  ylim(c(7,13))+
  guides(color="none", fill="none")+
  theme_minimal()+
  theme(strip.text.x = element_text(size=15,face="italic"),
        plot.background = element_rect(colour = "white"),
        axis.title = element_text(size=14),
        axis.text =  element_text(size=13))+
  geom_text(coeff_var_s, mapping = aes(x = -Inf, y = -Inf, label = paste("CV = ", d15N, sep = "")),
            hjust = -0.2, vjust = -1, size = 3.5)

# Selection of species presenting non-significant d15N-size relationship
ontogeny_non_significant <- filter(isotopic_dataset,
                                   species %in% c("Argyropelecus olfersii", "Lampanyctus macdonaldi",
                                                  "Searsia koefoedi", "Notoscopelus kroyeri",
                                                  "Xenodermichthys copei"))

#order species 
ontogeny_non_significant$species <- factor(ontogeny_non_significant$species,
                                           levels = c("Xenodermichthys copei",
                                                      "Searsia koefoedi",
                                                      "Argyropelecus olfersii",
                                                      "Lampanyctus macdonaldi",
                                                      "Notoscopelus kroyeri"))

coeff_var <- aggregate(d15N ~ species,
                       data = ontogeny_non_significant,
                       FUN = cv)
coeff_var$d15N <- round(coeff_var$d15N, digits = 2)


# plot of non-significant relationships
plot_non_significant <- ggplot(ontogeny_non_significant, aes(x=size, y=d15N))+
  geom_point (alpha=0.4, col="black") + 
  facet_wrap(~species, scale="free_x", ncol=3, nrow = 3)+ 
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste( ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size=4,
                        label.x.npc = "right",
                        label.y.npc = "bottom",
                        vstep = -0.0005)+ 
  xlab("Standard Length (cm)")+
  ylab(expression({delta}^15*N~'\u2030'))+
  ylim(c(7, 14))+
  guides(color="none", fill="none")+
  theme_minimal()+
  theme(strip.text.x = element_text(size=15,face="italic"),
        axis.title = element_text(size=14),
        plot.background = element_rect(colour = "white"),
        axis.text =  element_text(size=13))+
  geom_text(coeff_var, mapping = aes(x = -Inf, y = -Inf, label = paste("CV = ", d15N, sep = "")),
            hjust = -0.2, vjust = -0.8, size = 4)

# Combine the two plot 
ggpubr::ggarrange(
  plot_significant,
  plot_non_significant,
  ncol = 1,
  labels = c("A", "B"),
  heights = c(2, 1.25)
)
```

::: {.cell-output-display}
![](index_files/figure-html/d15_size_species-1.png){width=960}
:::

```{.r .cell-code}
# store the plot in the "figures" file in high resolution 
ggsave("d15n_size_sp.png", path = "figures", dpi = 700, height = 13, width = 10)
```
:::

#### Assumptions validation

::: {.cell}

```{.r .cell-code}
# Create an empty list to store diagnostic metrics
diag_metrics_list <- list()

species_list <- unique(ontogeny_significant$species)

for (species_name in species_list){
  species_data <- ontogeny_significant %>%
    filter(species == species_name)
  
  # Fit the linear model
  model <- lm(d15N ~ size, data = species_data)
  
  # Create diagnostic plots
  par(mfrow = c(2, 2))
  plot(model, main = paste("Diagnostic Plots for", species_name))
  
  # Get diagnostic metrics and store them in the list
  model.diag.metrics <- broom::augment(model)
  diag_metrics_list[[species_name]] <- model.diag.metrics %>%
    top_n(3, wt = .cooksd)
}
```

::: {.cell-output-display}
![](index_files/figure-html/assumptions-1.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/assumptions-2.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/assumptions-3.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/assumptions-4.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/assumptions-5.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/assumptions-6.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/assumptions-7.png){width=768}
:::
:::


__Access diagnostic metrics__

::: {.cell}

```{.r .cell-code}
print(diag_metrics_list)
```

::: {.cell-output .cell-output-stdout}
```
$`Lampanyctus crocodilus`
# A tibble: 3 × 8
   d15N  size .fitted .resid   .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>      <dbl>
1  8.75   7.8    9.82 -1.07  0.0266  0.537  0.0547      -2.00
2 10.7    7.6    9.78  0.959 0.0291  0.538  0.0482       1.79
3 10.1   14.8   11.2  -1.14  0.0362  0.536  0.0865      -2.15

$`Arctozenus risso`
# A tibble: 3 × 8
   d15N  size .fitted .resid   .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>      <dbl>
1  9.22  17.5    10.5 -1.27  0.0147  0.312   0.104      -3.73
2 11.5   11.5    10.7  0.764 0.0582  0.333   0.163       2.30
3 11.6   11.5    10.7  0.884 0.0582  0.329   0.218       2.66

$`Myctophum punctatum`
# A tibble: 3 × 8
   d15N  size .fitted .resid   .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>      <dbl>
1  9.29     8   10.4  -1.13  0.0497  0.436  0.171       -2.56
2 11.4      8   10.4   1.03  0.0497  0.439  0.143        2.34
3 10.0      5    9.40  0.619 0.0818  0.450  0.0909       1.43

$`Aphanopus carbo`
# A tibble: 3 × 8
   d15N  size .fitted .resid  .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl> <dbl>  <dbl>   <dbl>      <dbl>
1  12.2    59    12.0  0.199 0.315  0.195   0.326       1.19
2  12.8    86    12.5  0.305 0.157  0.170   0.252       1.65
3  12.4    88    12.6 -0.213 0.180  0.196   0.149      -1.17

$`Serrivomer beanii`
# A tibble: 3 × 8
   d15N  size .fitted .resid   .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>      <dbl>
1  9.23  26.7    8.93  0.295 0.269   0.505  0.0877      0.690
2 10.6   54.5    9.48  1.11  0.0325  0.462  0.0858      2.26 
3 10.2   46.8    9.33  0.834 0.0534  0.483  0.0826      1.71 

$`Stomias boa`
# A tibble: 3 × 8
   d15N  size .fitted .resid  .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl> <dbl>  <dbl>   <dbl>      <dbl>
1 11.7   30.7    12.2 -0.504 0.105  0.373   0.112      -1.39
2  9.47  11.8    10.5 -1.07  0.215  0.266   1.35       -3.13
3 11.6   16.9    11.0  0.658 0.105  0.357   0.192       1.81

$`Melanostigma atlanticum`
# A tibble: 3 × 8
   d15N  size .fitted .resid  .hat .sigma .cooksd .std.resid
  <dbl> <dbl>   <dbl>  <dbl> <dbl>  <dbl>   <dbl>      <dbl>
1  9.85   7      10.6 -0.711 0.373  0.421   1.18       -1.99
2 11.3    8      10.8  0.474 0.169  0.448   0.135       1.15
3 12.2   10.9    11.5  0.666 0.112  0.437   0.155       1.57
```
:::
:::



# 3. Variation partitionning

-   At the species level, is it the sampling depth or the size of the individuals that most influences the values in $\delta$<sup>15</sup>N?
-  To test the significance of the influence of each variable (depth and size) on $\delta$<sup>15</sup>N values an ANOVA-type permutation test was performed for each model (anova.cca function)
-  Since the third fraction (depth and size) is not the result of an RDA, it cannot be tested for significance.


::: {.cell}

```{.r .cell-code}
# S. boa and A. carbo not considered in this analysis due to small depth range sampled
varpart_data <- isotopic_dataset %>%
  filter(!species %in% c("Stomias boa", "Aphanopus carbo")) %>%
  # outliers to remove 
  # filter(!c(species == "Melanostigma atlanticum" & d15N == 9.85)) %>%
  # filter(!c(species == "Stomias boa" & d15N == 9.47)) %>%
  # filter(!c(species == "Argyropelecus olfersii" & d15N == 8.15))
  arrange(species)

# List of species names
species_list <- unique(varpart_data$species)

# Initialize an empty list to store the results
result_list <- list()

# Initialize an empty data frame to store indfract values (variance of d15N values explained by depth and size)
indfract_df <-
  data.frame(species = character(0),
             indfract = numeric(0),
             variable = character(0))

# Loop through each species
for (species_name in species_list) {
  par(mfrow=c(1,1))
  
  # Filter the data for the current species
  species_data <- varpart_data %>%
    filter(species == species_name)
  
  # Perform variation partitioning analysis
  species_part <- vegan::varpart(species_data$d15N,
                                 ~ trawling_depth, ~ size, data = species_data)
  
  # Create the specified plot for the current species
  plot <- plot(
    species_part,
    Xnames = c("Depth", "size"),
    bg = c("#072AC8", "#0B5345"),
    alpha = 100,
    digits = 2,
    cex = 1.3
  )
  
  # Add a title to the plot
  title(main = paste("Variation Partitioning for", species_name))
  
  # Perform the significance testing
  rda1 <- vegan::rda(species_data$d15N,
                     species_data$trawling_depth,
                     species_data$size)
  rda2 <- vegan::rda(species_data$d15N,
                     species_data$size,
                     species_data$trawling_depth)
  anova1 <- vegan::anova.cca(rda1)
  anova2 <- vegan::anova.cca(rda2)
  
  # Check homogeneity of variances with residual plot
  model <- lm(d15N ~ trawling_depth + size, data = species_data)
  residuals <- residuals(model)
  
  # Store the results, and the significance testing in the list
  result_list[[species_name]] <-
    list(
      varpart_results = species_part,
      varpart_plot = plot,
      anova_depth_size = anova1,
      anova_size_depth = anova2,
      residuals = residuals
    )
  
  # Extract and store the indfract value (variance explained by each variable) in the data frame
  indfract_value <- species_part$part$indfract$Adj.R.squared
  indfract_df <-
    rbind(
      indfract_df,
      data.frame(
        species = species_name,
        indfract = indfract_value,
        variable = c("Depth only", "Size only", "Depth and size", "Residuals")
      )
    )
  
  # Print the significance testing results
  cat("ANOVA (Depth-Size):\n")
  print(broom::tidy(anova1))
  
  cat("ANOVA (Size-Depth):\n")
  print(broom::tidy(anova2))
  
  # Plot residual vs fitted 
  par(mfrow = c(1, 2))    
  cat("\n")
  plot(model)
}
```

::: {.cell-output-display}
![](index_files/figure-html/varpart-1.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00218      1.43   0.241
2 Residual    75  0.114       NA     NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00878      5.78   0.011
2 Residual    75  0.114       NA     NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-2.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-3.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-4.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0152      4.92   0.029
2 Residual    61   0.189      NA     NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0103      3.34    0.06
2 Residual    61   0.189      NA      NA   
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-5.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-6.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-7.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1 0.000170    0.0807   0.797
2 Residual   139 0.292      NA       NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1    0.115      54.5   0.001
2 Residual   139    0.292      NA    NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-8.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-9.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-10.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00254     0.578   0.478
2 Residual    20  0.0879     NA      NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00572      1.30   0.273
2 Residual    20  0.0879      NA     NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-11.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-12.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-13.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df    Variance  statistic p.value
  <chr>    <dbl>       <dbl>      <dbl>   <dbl>
1 Model        1 0.000000616  0.0000724   0.997
2 Residual    23 0.196       NA          NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0324      3.81   0.053
2 Residual    23   0.196      NA     NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-14.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-15.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-16.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00449      1.75   0.203
2 Residual    77  0.198       NA     NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0673      26.2   0.001
2 Residual    77   0.198       NA    NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-17.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-18.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-19.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1 0.000379     0.480    0.49
2 Residual    72 0.0568      NA       NA   
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1 0.000252     0.320   0.596
2 Residual    72 0.0568      NA      NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-20.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-21.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-22.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00619     0.350   0.547
2 Residual    17  0.300      NA      NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0155     0.880   0.371
2 Residual    17   0.300     NA      NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-23.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-24.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-25.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1  0.00349     0.409   0.517
2 Residual    28  0.239      NA      NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0450      5.28   0.026
2 Residual    28   0.239      NA     NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-26.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-27.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-28.png){width=768}
:::

::: {.cell-output .cell-output-stdout}
```
ANOVA (Depth-Size):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1   0.0263      7.48   0.005
2 Residual   111   0.391      NA     NA    
ANOVA (Size-Depth):
# A tibble: 2 × 5
  term        df Variance statistic p.value
  <chr>    <dbl>    <dbl>     <dbl>   <dbl>
1 Model        1 0.000203    0.0576   0.811
2 Residual   111 0.391      NA       NA    
```
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-29.png){width=768}
:::

::: {.cell-output-display}
![](index_files/figure-html/varpart-30.png){width=768}
:::
:::


## Variance partitionning summary 
- compile information on a single plot for all species 

::: {.cell}

```{.r .cell-code}
#order according to the variance explained by each species 
indfract_df$species <- factor(
  indfract_df$species,
  levels = c(
    "Notoscopelus kroyeri",
    "Searsia koefoedi",
    "Lampanyctus macdonaldi",
    "Xenodermichthys copei",
    "Arctozenus risso",
    "Argyropelecus olfersii",
    "Serrivomer beanii",
    "Melanostigma atlanticum",
    "Myctophum punctatum",
    "Lampanyctus crocodilus"
  )
)

# Percentage of variance for each variable + residuals
var_freq <- indfract_df %>%
  group_by(species) %>%
  mutate(freq = ((indfract * 100) / sum(indfract)))

# Order variable
var_freq$variable <- factor(var_freq$variable,
                            levels = c("Residuals", "Depth only", "Depth and size", "Size only"))


# plot
ggplot(var_freq , aes(fill=variable, y=freq, x=species)) + 
  geom_bar(position="stack", stat="identity", alpha=0.6, linewidth=0.5)+
  scale_fill_manual(values = c("#33505C","#861D31","#D8973C"))+
  theme_light()+
  coord_flip()+
  ylim(c(0,40))+
  ylab("Percentage variance (%)")+
  xlab("")+
  theme(axis.text.y = element_text(face ="italic", size=11),
        axis.title = element_text(size=11),
        legend.text = element_text(size=11),
        legend.title = element_text(size=11),
        axis.text.x = element_text(size=11))+
  guides(fill=guide_legend(title="Explanatory variables"),
         color="none")
```

::: {.cell-output-display}
![](index_files/figure-html/varpart_plot-1.png){width=864}
:::

```{.r .cell-code}
# store the plot in the "figures" file in high resolution 
ggsave("d15n_variability.png", path = "figures", dpi = 700, width = 8, height = 5)
```
:::

# 4. Appendices

## Representativeness of sample sizes between the two data sets 
- In order to compare the sizes of the species in the two data sets, a conversion from standard size to total size was carried out using conversion equations estimated in the laboratory by Tiphaine Chouvelon. 

::: {.cell}

```{.r .cell-code}
# conversion standard length in total length for the isotope dataset (equations by Tiphaine Chouvelon)
isotopic_dataset_TL <- isotopic_dataset %>%
  mutate(
    total_length = case_when(
      species == "Aphanopus carbo" & size < 85 ~ size + 3,
      species == "Aphanopus carbo" & size > 85 ~ size + 5,
      species == "Arctozenus risso" ~ size + 1,
      species == "Argyropelecus olfersii" ~ (size + 0.7913) / 0.9039,
      species == "Lampanyctus crocodilus" ~ (size + 0.4754) / 0.8284,
      species == "Lampanyctus macdonaldi" ~ (size + 0.4754) / 0.8284,
      species == "Melanostigma atlanticum" ~ size,
      species == "Myctophum punctatum" ~ (size - 1.0381) / 0.7373,
      species == "Notoscopelus kroyeri" ~ (size + 0.2406) / 0.8903,
      species == "Searsia koefoedi" ~ (size - 0.7343) / 0.8593,
      species == "Serrivomer beanii" ~ (size + 0.6654) / 1.0053,
      species == "Stomias boa" ~ (size + 0.2633) / 0.9679,
      species == "Xenodermichthys copei" ~ (size + 0.0631) / 0.9266)) %>%
  select(species, total_length) %>%
  mutate(data_set = "isotopic dataset") %>%
  rename("size" = "total_length")

# formatting the trawling data set
trawling_dataset_comparaison <- trawling_dataset %>%
  select(species, size) %>%
  mutate(data_set = "trawling dataset")

# merge the two data frame
size_comparaison <-
  rbind(trawling_dataset_comparaison, isotopic_dataset_TL)

# Calculate the size range for each species in the "isotopic dataset"
size_ranges <- subset(size_comparaison, data_set == "isotopic dataset") %>%
  group_by(species) %>%
  summarize(min_size = min(size), max_size = max(size))

# plot 
ggplot(subset(size_comparaison, data_set == "trawling dataset"), aes(x = size)) +
  geom_density(alpha = 0.3, linewidth = 0.3, fill = "#B0A8B9", col = "#B0A8B9") +
  facet_wrap(~species, scale = "free", ncol = 3) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 11, face = "italic"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour = "white")) +
  xlab("Standard length (cm)") +
  ylab("Density")+
  geom_segment(data = size_ranges,
               aes(x = min_size, xend = max_size, y = 0, yend = 0), color = "#3596B5", linewidth=1.2)
```

::: {.cell-output-display}
![](index_files/figure-html/data_set-1.png){width=960}
:::

```{.r .cell-code}
ggsave("Appendix_1.png", path = "figures", dpi = 200, width = 9, height = 8)
```
:::


## Test of temporal variability on $\delta$<sup>15</sup>N values

::: {.cell}

```{.r .cell-code}
# color for each sampling Years
col_Years <- c("#FFC75F", "#F3C5FF", "#845EC2", "#C34A36", "#3596B5", "grey")

# Years in discrete variable
isotopic_dataset$Years <- as.character(isotopic_dataset$Years)

# plot
ggplot(isotopic_dataset , aes(x=size, y=d15N))+
  geom_point (alpha=0.8, aes(col=Years), size=1.5) + 
  scale_color_manual(values = col_Years)+
  facet_wrap(~species, scale="free_x", ncol=3)+ 
  xlab("Standard Length (cm)")+
  ylab(expression({delta}^15*N~'\u2030'))+
  theme_light()+
  labs(col= "Sampling Years")+
  theme(strip.text.x = element_text(size = 11, face = "italic", color = "black"),
        strip.background=element_rect(fill="white"),
        axis.title = element_text(size=11),
        axis.text =  element_text(size=11),
        legend.title = element_text(size=11),
        legend.text = element_text(size=11),
        panel.background = element_rect(fill = "white",colour = "white"),
        plot.background = element_rect(fill = "white",colour = "white"))
```

::: {.cell-output-display}
![](index_files/figure-html/temporal_variability-1.png){width=960}
:::

```{.r .cell-code}
#store the plot in the "figures" file in high resolution 
ggsave("Appendix_2.png", path = "figures", dpi = 600, height = 8, width = 9)
```
:::


## Non-significant size-depth relationships at species level

::: {.cell}

```{.r .cell-code}
# Selection of non-significant size-depth relationships
trawling_dataset_ns <-
  filter(
    trawling_dataset,
    species %in% c(
      "Arctozenus risso",
      "Lampanyctus macdonaldi",
      "Notoscopelus kroyeri",
      "Searsia koefoedi",
      "Serrivomer beanii",
      "Argyropelecus olfersii",
      "Stomias boa",
      "Aphanopus carbo"))

# order species
trawling_dataset_ns$species <- factor(
  trawling_dataset_ns$species,
  levels = c(
    "Notoscopelus kroyeri",
    "Serrivomer beanii",
    "Stomias boa",
    "Argyropelecus olfersii",
    "Arctozenus risso",
    "Searsia koefoedi",
    "Aphanopus carbo",
    "Lampanyctus macdonaldi"),
  labels = c(
    "N. kroyeri",
    "S. beanii",
    "S. boa",
    "A. olfersii",
    "A. risso",
    "S. koefoedi",
    "A. carbo",
    "L.macdonaldi"))

# Median depth
median_size_sp_ns <- trawling_dataset_ns %>%
  group_by(depth_layer, species) %>%
  summarise(median_size = median(size))

# plot
ggplot(trawling_dataset_ns, aes(x=size)) +
  geom_density(alpha=0.3, linewidth=0.8, adjust= 2, aes(fill= depth_layer, col= depth_layer))+
  scale_fill_manual(values = c("#93C3FF", "#6799D3","#3A72A8", "#002A58"))+
  scale_color_manual(values = c("#93C3FF", "#6799D3","#3A72A8", "#002A58"))+
  facet_grid(depth_layer~species, scale="free")+
  theme_minimal()+
  theme(strip.text.x = element_text(size=11, face="italic"),
        strip.text.y = element_text(size=11),
        axis.title = element_text(size=12),
        axis.text =  element_text(size=11, color= "grey50"),
        panel.background=element_rect(color="white"),
        plot.background = element_rect(fill = "white",colour = "white"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))+
  guides(fill="none",col="none")+
  xlab("Total Length (cm)")+
  geom_vline(data =median_size_sp_ns, aes(xintercept = median_size, color=depth_layer),
             linewidth=0.6, linetype="dashed")
```

::: {.cell-output-display}
![](index_files/figure-html/size_depth_ns-1.png){width=1536}
:::

```{.r .cell-code}
# store the plot in the "figures" file in high resolution 
ggsave("Appendix_3.png", path = "figures", dpi = 600, width = 9, height = 8)
```
:::
