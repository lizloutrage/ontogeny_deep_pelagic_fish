# relation x= taille et y= abondance par profondeur
large_dataset <- subset(size_distribution_complete, dataset == "Total")

large_dataset$depth_layer <- factor(large_dataset$depth_layer, 
                                    levels = c("epipelagic","upper_mesopelagic",
                                               "lower_mesopelagic","bathypelagic"),
                                    labels = c("Epipelagic","Upper mesopelagic",
                                               "Lower mesopelagic","Bathypelagic"))

abundance_size <- large_dataset %>%
  count(size, depth_layer)

ggplot(abundance_size, aes(x=size, y=n)) + 
  geom_smooth(method = "lm", se=F)+
  geom_point()+
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.. 
                                          , ..n.label..,sep = "*`,`~")),
                        parse = TRUE,
                        size= 3.4,
                        label.x.npc = "right",
                        label.y.npc = "bottom",
                        vstep = -0.0005)+
  facet_wrap(~depth_layer, ncol=1, scales = "free_y") +
  theme_minimal()

