# options(show.error.locations = TRUE)
# options(error = traceback)

`%notin%` <- Negate(`%in%`)

DEFAULT_ASPECT_RATIO <- 2 / (1 + sqrt(5))

create_theme_facet <- function(aspect_ratio = DEFAULT_ASPECT_RATIO) {
  theme(
    # aspect.ratio = aspect_ratio,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.box.spacing = unit(0.1, "cm"),
    legend.title.align = 0.5,
    legend.text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 14, hjust = 0.5, color = "black"),
    panel.grid.major = element_line(linetype = "11", linewidth = 0.5, color = "grey"),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 14),
    axis.line = element_line(linewidth = 0.2, color = "black"),
    axis.title.y = element_text(size = 12, vjust = 1.5, color = "black"),
    axis.title.x = element_text(size = 12, vjust = 1.5, color = "black"),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )
}

create_theme <- function(aspect_ratio = DEFAULT_ASPECT_RATIO) {
  theme(
    aspect.ratio = aspect_ratio,
    legend.background = element_blank(),
    legend.title = element_blank(),
    # legend.margin = margin(-5, 0, 0, 0),
    # legend.spacing.x = unit(0.01, "cm"),
    # legend.spacing.y = unit(0.01, "cm"),
    legend.box.spacing = unit(0.1, "cm"),
    legend.title.align = 0.5,
    legend.text = element_text(size = 8, color = "black"),
    plot.title = element_text(size = 10, hjust = 0.5, color = "black"),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.grid.major = element_line(linetype = "11", linewidth = 0.5, color = "grey"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.2, color = "black"),
    axis.title.y = element_text(size = 8, vjust = 1.5, color = "black"),
    axis.title.x = element_text(size = 8, vjust = 1.5, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8, color = "black"),
    axis.text.y = element_text(size = 8, color = "black")
  )
}
