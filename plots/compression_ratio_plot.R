#!/usr/bin/env Rscript

source("box_plot_printer.R")

create_compression_ratio_plot <- function(data, file_name) {
  annotation <- create_annotation(data, "CompressionRatio", "CompressionRatio")
  print_boxplot(data, "CompressionRatio", "CompressionRatio", annotation, file_name)
}

create_detailed_compression_ratio_plot <- function(data, file_name) {
  for (df in data) {
    p <- ggplot2::ggplot(df) +
      ggplot2::geom_col(
        ggplot2::aes(x = Graph, y = CompressionRatio),
        position = "dodge"
      ) +
      ggplot2::labs(x = "Graph", y = "Compression Ratio") +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed") +
      ggplot2::theme_bw() +
      create_theme() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 5)
      )
    cli::cli_alert_success("Created detailed compression ratio plot {.file {file_name}}")

    ggplot2::ggsave(
      filename = paste(
        paste0(file_name, "-", df$Algorithm),
        "pdf",
        sep = "."
      ),
      plot = p,
      device = "pdf",
      path = "plots"
    )
    cli::cli_text()
  }
}

create_compression_ratio_plots <- function(data) {
  create_compression_ratio_plot(data, "compression_ratio_all")
  create_detailed_compression_ratio_plot(data, "compression_ratio_detailed")
}
