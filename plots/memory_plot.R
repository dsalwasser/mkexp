#!/usr/bin/env Rscript

source("box_plot_printer.R")

print_memory_plot <- function(data, column, file_name) {
  annotation <- create_annotation(data, "Memory", column)
  print_boxplot(data, "Memory", column, annotation, file_name)
}

print_per_k_memory_plot <- function(data, column, file_name) {
  annotation <- create_per_k_annotation(data, "Memory", column)
  print_per_k_boxplot(data, "Memory", column, annotation, file_name)
}

print_detailed_memory_plot <- function(data, file_name) {
  data <- do.call(rbind.data.frame, data) %>%
    tidyr::pivot_longer(
      cols = c(
        AvgInputGraphMemory,
        AvgFirstCoarseGraphMemory,
        AvgRemainingMemory
      ),
      names_to = "Fill",
      values_to = "AvgPivotedMemory"
    )

  annotation <- create_marg_annotation(
    list(data),
    "Memory",
    "AvgPivotedMemory"
  )

  print_marg_boxplot(list(data), "Memory", "AvgPivotedMemory", "Fill", annotation, file_name)
}

create_memory_plots <- function(data, plot_per_k, plot_graph_memory) {
  print_memory_plot(data, "AvgMemory", "memory_all")

  if (plot_per_k) {
    print_per_k_memory_plot(data, "AvgMemory", "memory_per_k")
  }

  if (plot_graph_memory) {
    print_detailed_memory_plot(data, "memory_detailed")
  }
}
