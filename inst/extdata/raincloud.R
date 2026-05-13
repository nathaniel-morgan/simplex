print(
  ggplot(x, aes(x = class, y = value, fill = class, color = class))+
    geom_violin(alpha = 0.25, trim = FALSE, width = 0.85, color = NA)+
    geom_jitter(width = 0.12, alpha = 0.35, size = 1.2, show.legend = FALSE)+
    geom_boxplot(width = 0.16, outlier.shape = NA, fill = "white", alpha = 0.8, color = "black")+
    stat_summary(fun = median, geom = "point", size = 2.2, color = "black")+
    labs(x = NULL, y = NULL)+
    theme_minimal()+
    theme(legend.position = "none")
)
