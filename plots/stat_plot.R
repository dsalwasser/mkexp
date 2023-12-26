#!/usr/bin/env Rscript

create_compression_ratio_plot <- function(data, algorithm) {
  ggplot2::ggplot(data) +
    geom_col(aes(x = Graph, y = CompressionRatio)) +
    theme_bw() +
    create_theme_facet() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  cli::cli_alert_success("Created compression ratio plot for algorithm {col_magenta(algorithm)}")

  ggplot2::ggsave(
    path = "plots",
    filename = paste(paste0("compression_ratio_", algorithm), "pdf", sep = "."),
    width = 30
  )
  cli::cli_text()
}

create_compression_ratio_plots <- function(data) {
  data <- do.call(rbind.data.frame, data) %>%
    dplyr::filter(CompressionRatio > 0) %>%
    dplyr::group_by(Algorithm, Graph) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::group_by(Algorithm) %>%
    dplyr::group_walk(~ create_compression_ratio_plot(.x, .y$Algorithm))
}
