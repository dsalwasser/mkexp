#!/usr/bin/env Rscript

source("box_plot_printer.R")

create_per_instance_plot <- function(
    data,
    plot_cut,
    plot_timing,
    plot_detailed_timing,
    plot_memory) {
  all_data <- data.frame()
  first <- data[[1]]
  first_memory <- do.call(rbind.data.frame, data) %>%
    dplyr::filter(AvgMemory > 0) %>%
    dplyr::group_by(Algorithm) %>%
    dplyr::filter(cur_group_id() == 1)
  for (df in data) {
    df$RelAvgCut <- df$AvgCut / first$AvgCut
    df$RelAvgTime <- df$AvgTime / first$AvgTime
    df$RelAvgClusteringTime <- df$AvgClusteringTime / first$AvgClusteringTime
    df$RelAvgContractionTime <- df$AvgContractionTime / first$AvgContractionTime
    df$RelAvgInitialPartitioningTime <- df$AvgInitialPartitioningTime / first$AvgInitialPartitioningTime
    df$RelAvgRefinementTime <- df$AvgRefinementTime / first$AvgRefinementTime
    df$RelAvgMemory <- df$AvgMemory / first_memory$AvgMemory
    all_data <- rbind(all_data, df)
  }
  all_data$K <- factor(all_data$K)
  graphs <- unique(all_data$Graph)

  side_len <- round(sqrt(length(unique(all_data$Graph))))
  normalized_data <- all_data |>
    dplyr::mutate(Title = paste0(Graph, ", D=", MaxDeg)) |>
    dplyr::mutate(MaxDegToM = MaxDeg / M.y)
  normalized_data$Title <- factor(normalized_data$Title, levels = unique(normalized_data$Title[order(normalized_data$MaxDeg, decreasing = TRUE)]))

  plot_per_instance <- function(title,
                                objective,
                                column,
                                per_instance_data = normalized_data) {
    p <- ggplot2::ggplot(
      per_instance_data,
      aes(x = K, y = !!sym(column), fill = Algorithm)
    ) +
      ggtitle(paste0(
        "Per Instance ",
        title,
        " (rel. to first, lower is better)"
      )) +
      geom_bar(
        stat = "identity",
        position = position_dodge()
      ) +
      ylab(paste0(objective, " rel. to Best")) +
      xlab("Number of Blocks") +
      theme_bw() +
      facet_wrap(~Title, ncol = side_len, scales = "free") +
      geom_hline(yintercept = 1) +
      create_theme_facet() +
      theme(legend.position = "right")
    print(p)
  }

  pdf("plots/per_instance.pdf", width = side_len * 4, height = side_len * 3)

  if (plot_cut) {
    plot_per_instance("Cuts", "Cut", "RelAvgCut")
  }

  if (plot_timing) {
    plot_per_instance("Running Times", "Time", "RelAvgTime")

    if (plot_detailed_timing) {
      plot_per_instance("Clustering Times", "Time", "RelAvgClusteringTime")
      plot_per_instance("Contraction Times", "Time", "RelAvgContractionTime")
      plot_per_instance("Initial Partitioning Times", "Time", "RelAvgInitialPartitioningTime")
      plot_per_instance("Refinement Times", "Time", "RelAvgRefinementTime")
    }
  }

  if (plot_memory) {
    normalized_memory_data <- normalized_data %>% dplyr::filter(AvgMemory > 0)
    plot_per_instance("Memory Usage", "Used Memory", "RelAvgMemory", normalized_memory_data)
  }

  dev.off()
}
