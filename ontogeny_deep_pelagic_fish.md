---
title: "Ontogeny of deep pelagic fish"
format: 
  html:
    self-contained: false
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



# 1. Size distribution as a function of depth

## Density plot by depth layer - community level

-   Bigger fish at depth ?


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/size_depth_community-1.png){width=672}
:::
:::

### Test of the significance of the size-depth relationship - community level

::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/size_depth_community_lm-1.png){width=672}
:::
:::



## Density plot by depth layer - species level

-  Only significant relationships (non-significant relationships in supplementary materials)

::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/size_depth_species-1.png){width=1248}
:::
:::

# Linear relationships

::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/size_depth_species_lm-1.png){width=960}
:::
:::



# 2. $\delta$<sup>15</sup>N-size relationships

-   Sampling of some individuals to obtain $\delta$<sup>15</sup>N values

## At community level

-   Can we observe an increase in $\delta$<sup>15</sup>N values (proxy of trophic level) with increasing size of individuals in the deep pelagic fish community of the Bay of Biscay?

-   X axis (size) in log<sub>2</sub>

-   Significant increase but very small slope


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/d15n_size_community-1.png){width=672}
:::
:::


## At species level

-   $\delta$<sup>15</sup>N axis standardized
-  __A__: significant relationships 
-  __B__: non-significant relationships 
-  __Coefficient of variation__: The dispersion of the $\delta$<sup>15</sup>N values is not the same between the species having shown non-significant $\delta$<sup>15</sup>N-size relationships: *X. copei* presents a strong dispersion of its values (CV = 6.57) contrary to the values of *N. kroyeri* which remain relatively stable with the size of its individuals (CV = 2.15)
-   Do these differences translate into differences in their feeding strategies?


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/d15_size_species-1.png){width=960}
:::
:::


# 4. Variation partitionning

-   At the species level, is it the sampling depth or the size of the individuals that most influences the values in $\delta$<sup>15</sup>N?
-  To test the significance of the influence of each variable (depth and size) on \(\delta\)$^{15}$N values an ANOVA-type permutation test was performed for each model (anova.cca function)
-  Since the third fraction is not the result of an RDA, it cannot be tested for significance.

## At community level


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-7-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Lampanyctus crocodilus*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-10-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Myctophum punctatum*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-13-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Melanostigma atlanticum*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-16-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Serrivomer beanii*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-19-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Argyropelecus olfersii*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-22-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Lampanyctus macdonaldi*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-25-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Searsia koefoedi*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-28-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Notoscopelus kroyeri*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-31-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Xenodermichthys copei*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-34-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## *Arctozenus risso*


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/unnamed-chunk-37-1.png){width=576}
:::
:::

::: {.cell}

:::

::: {.cell}

:::


## Variance partitionning summary


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/var_part-1.png){width=1248}
:::
:::


# 5. Summary table

**Testing significance:**

-   $\delta$<sup>15</sup>N - Size & $\delta$<sup>15</sup>N - depth : use of the anova.cca() function to test the significance of each part separately (depth & size) on the variability of $\delta$<sup>15</sup>N values
-   Depth - size : test with linear model lm (size\~depth)
-   *Stomias boa* & *Aphanopus carbo* \* depth range too small (\< 100m) so no use of variance partition models on these species, but linear models $\delta$<sup>15</sup>N - size significant

![Summary table](ontogeny_deep_pelagic_fish_files/figure-html/summary_table.jpg){width="70%"}

# 6. Limits

## Test of temporal variability on $\delta$<sup>15</sup>N values

-   Difficult to test because it is necessary to eliminate the biases due to size and depth.

-   However, over 90% of the sampling has been completed between 2019 and 2021 (and almost 3/4 in 2021)

-   If major environmental changes had occurred that significantly altered the baseline over time, this would have been observed in all species in the same way, which does not appear to be the case here

-   Test with *Lampnyctus crocodilus*: In the bathypelagic layer and individuals between 10 and 12cm : non signifiant differences

-   we can see that the data from the different years merge, there does not seem to be a strong effect of the year of sampling on the $\delta$<sup>15</sup>N values

 -   only night sampling


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/temporal_variability-1.png){width=1440}
:::
:::


## Representativeness of the sampling of individuals for $\delta$<sup>15</sup>N values

-   In supplementary material ?
-   no sampling of small *M. atlanticum* and big *L. macdonaldi*
-   but overall good sampling coverage over the size ranges of the species


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/data_set-1.png){width=1440}
:::
:::


## Non-significant size-depth relationships at species level


::: {.cell}
::: {.cell-output-display}
![](ontogeny_deep_pelagic_fish_files/figure-html/size_depth_ns-1.png){width=1536}
:::
:::
