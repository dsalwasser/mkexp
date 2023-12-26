#!/usr/bin/env Rscript
options(error = traceback)

library(dplyr)

load_data <- function(name, file) {
  df <- read.csv(file)
  cat("Loaded", nrow(df), "rows from", file, ",", name, "\n")

  df$Algorithm <- name
  df$NumPEs <- df$NumNodes * df$NumMPIsPerNode * df$NumThreadsPerMPI

  return(df %>% dplyr::arrange(Graph, K))
}

algorithms <- commandArgs(trailingOnly = TRUE)
num_algorithms <- length(algorithms)

data <- list()
for (algorithm in algorithms) {
  df <- load_data(algorithm, paste0(getwd(), "/results/", algorithm, ".csv"))

  compression_stats <- df %>%
    dplyr::filter(CompressionRatio > 0, SizeReduction > 0) %>%
    dplyr::group_by(Graph) %>%
    dplyr::summarise(
      CompressionRatio = first(CompressionRatio),
      SizeReduction = first(SizeReduction)
    ) %>%
    dplyr::summarise(
      AvgCompressionRatio = mean(CompressionRatio),
      MedianCompressionRatio = median(CompressionRatio),
      AvgSizeReduction = mean(SizeReduction),
      MedianSizeReduction = median(SizeReduction)
    )

  print(compression_stats)
}
