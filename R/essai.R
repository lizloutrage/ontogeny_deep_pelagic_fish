essai <- large_dataset%>%
  group_by(species)%>%
  do(w = wilcox.test(size ~ depth_layer,  data = large_dataset))

large_dataset %>%
  group_by(dose) %>%
  wilcox_test(data =., len ~ supp) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")

kruskal.test(size ~ depth_layer, data = large_dataset)
pairwise.wilcox.test(large_dataset$size, large_dataset$depth_layer,
                     p.adjust.method = "BH")