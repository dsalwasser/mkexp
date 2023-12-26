#!/usr/bin/env Rscript

source("performance_profile_plot.R")

print_cut_plot <- function(data, file_name) {
  ks <- unique(data[[1]]$K)
  num_unique_graphs <- length(unique(data[[1]]$Graph))

  all_pp <- do.call(create_performance_profile, data) +
    theme_bw() +
    create_theme() +
    ggtitle(paste0(
      num_unique_graphs,
      " unique graphs with k={",
      paste(ks, collapse = ", "),
      "}"
    ))
  cli::cli_alert_success("Created performance profile plot {.file {file_name}}")

  ggplot2::ggsave(paste(file_name, "pdf", sep = "."), all_pp, "pdf", "plots")
  cli::cli_text()
}

print_pairwise_cut_plot <- function(data, file_name) {
  pdf(paste0("plots/", file_name, ".pdf"))

  ks <- unique(data[[1]]$K)
  num_unique_graphs <- length(unique(data[[1]]$Graph))

  for (i in 1:(length(algorithms) - 1)) {
    for (j in (i + 1):length(algorithms)) {
      pp <- create_performance_profile(data[[i]], data[[j]]) +
        theme_bw() +
        create_theme() +
        ggtitle(paste0(
          num_unique_graphs,
          " unique graphs with k={",
          paste(ks, collapse = ", "),
          "}"
        ))
      print(pp)
    }
  }

  dev.off()
  cli::cli_alert_success("Created performance profile plot {.file {file_name}}")
  cli::cli_text()
}

create_cut_plots <- function(data, plot_pairwise_cut) {
  print_cut_plot(data, "cut_all")

  if (plot_pairwise_cut) {
    print_pairwise_cut_plot(data, "cut_pairwise")
  }
}
