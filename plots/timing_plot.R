#!/usr/bin/env Rscript

source("box_plot_printer.R")

print_timing_plot <- function(data, column, file_name) {
  annotation <- create_annotation(data, "Time", column)
  print_boxplot(data, "Time", column, annotation, file_name)
}

print_per_k_timing_plot <- function(data, column, file_name) {
  annotation <- create_per_k_annotation(data, "Time", column)
  print_per_k_boxplot(data, "Time", column, annotation, file_name)
}

create_timing_plots <- function(
    data,
    plot_per_k,
    plot_detailed_timings) {
  print_timing_plot(data, "AvgTime", "time_all")

  if (plot_detailed_timings) {
    print_timing_plot(data, "AvgClusteringTime", "time_all_clustering")
    print_timing_plot(data, "AvgContractionTime", "time_all_contraction")
    print_timing_plot(data, "AvgInitialPartitioningTime", "time_all_initial_partitioning")
    print_timing_plot(data, "AvgRefinementTime", "time_all_refinement")
  }

  if (plot_per_k) {
    print_per_k_timing_plot(data, "AvgTime", "time_per_k")

    if (plot_detailed_timings) {
      print_per_k_timing_plot(data, "AvgClusteringTime", "time_per_k_clustering")
      print_per_k_timing_plot(data, "AvgContractionTime", "time_per_k_contraction")
      print_per_k_timing_plot(data, "AvgInitialPartitioningTime", "time_per_k_initial_partitioning")
      print_per_k_timing_plot(data, "AvgRefinementTime", "time_per_k_refinement")
    }
  }
}
