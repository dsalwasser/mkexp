#!/usr/bin/env Rscript

`%notin%` <- Negate(`%in%`)

empty_min <- function(x) {
  if (length(x[!is.na(x)]) > 0) {
    return(min(x, na.rm = TRUE))
  } else {
    return(Inf)
  }
}

weakscaling_aggregator <- function(df) {
  data.frame(
    MinCut = empty_min(df$Cut),
    AvgCut = mean(df$Cut, na.rm = TRUE),
    MinTime = empty_min(df$Time),
    AvgTime = mean(df$Time, na.rm = TRUE),
    AvgClusteringTime = mean(df$ClusteringTime, na.rm = TRUE),
    AvgContractionTime = mean(df$ContractionTime, na.rm = TRUE),
    AvgInitialPartitioningTime = mean(df$InitialPartitioningTime, na.rm = TRUE),
    AvgRefinementTime = mean(df$RefinementTime, na.rm = TRUE),
    AvgMemory = mean(df$Memory, na.rm = TRUE),
    AvgInputGraphMemory = mean(df$InputGraphMemory, na.rm = TRUE),
    AvgFirstCoarseGraphMemory = mean(df$FirstCoarseGraphMemory, na.rm = TRUE),
    AvgRemainingMemory = mean(df$RemainingMemory, na.rm = TRUE),
    MinBalance = empty_min(df$Balance),
    M = max(df$M, na.rm = TRUE),
    N = max(df$N, na.rm = TRUE),
    Timeout = any(as.logical(df$Timeout)) & all(as.logical(df$Timeout) | as.logical(df$Failed)),
    Failed = all(as.logical(df$Failed))
  )
}

aggregate_data <- function(df, timelimit, aggregator) {
  df <- df %>%
    dplyr::mutate(Cut = ifelse(Timeout | Failed, NA, Cut)) %>%
    dplyr::mutate(Balance = ifelse(Timeout | Failed, NA, Balance)) %>%
    dplyr::mutate(Time = ifelse(Timeout, timelimit, Time)) %>%
    dplyr::mutate(Time = ifelse(Failed & !Timeout, NA, Time)) %>%
    dplyr::mutate(Cut = ifelse(Balance > 0.03 + .Machine$double.eps, NA, Cut))

  vars <- colnames(df)
  vars <- vars[!vars %in% c("Cut", "Balance", "Time", "ClusteringTime", "ContractionTime", "InitialPartitioningTime", "RefinementTime", "Memory", "InputGraphMemory", "FirstCoarseGraphMemory", "RemainingMemory", "Failed", "Timeout", "Seed")]
  df <- plyr::ddply(df, vars, aggregator)

  df <- df %>%
    dplyr::mutate(AvgCut = ifelse(is.na(AvgCut), Inf, AvgCut)) %>%
    dplyr::mutate(MinCut = ifelse(is.na(MinCut), Inf, MinCut)) %>%
    dplyr::mutate(AvgTime = ifelse(is.na(AvgTime), Inf, AvgTime)) %>%
    dplyr::mutate(AvgClusteringTime = ifelse(is.na(AvgClusteringTime), Inf, AvgClusteringTime)) %>%
    dplyr::mutate(AvgContractionTime = ifelse(is.na(AvgContractionTime), Inf, AvgContractionTime)) %>%
    dplyr::mutate(AvgInitialPartitioningTime = ifelse(is.na(AvgInitialPartitioningTime), Inf, AvgInitialPartitioningTime)) %>%
    dplyr::mutate(AvgRefinementTime = ifelse(is.na(AvgRefinementTime), Inf, AvgRefinementTime)) %>%
    dplyr::mutate(MinTime = ifelse(is.na(MinTime), Inf, MinTime)) %>%
    dplyr::mutate(AvgMemory = ifelse(is.na(AvgMemory), Inf, AvgMemory)) %>%
    dplyr::mutate(AvgInputGraphMemory = ifelse(is.na(AvgInputGraphMemory), Inf, AvgInputGraphMemory)) %>%
    dplyr::mutate(AvgFirstCoarseGraphMemory = ifelse(is.na(AvgFirstCoarseGraphMemory), Inf, AvgFirstCoarseGraphMemory)) %>%
    dplyr::mutate(AvgRemainingMemory = ifelse(is.na(AvgRemainingMemory), Inf, AvgRemainingMemory))

  df %>%
    dplyr::mutate(Infeasible = !Failed & !Timeout & MinBalance > 0.03 + .Machine$double.eps) %>%
    dplyr::mutate(Feasible = !Failed & !Timeout & !Infeasible) %>%
    dplyr::mutate(Invalid = Failed | Timeout | Infeasible)
}

load_data <- function(name, file) {
  cli::cli_text("Parsing result file for {col_magenta(name)} from {.file {file}}")

  df <- read.csv(file)

  if ("NumNodes" %notin% colnames(df)) {
    df$NumNodes <- 1
  }

  if ("NumMPIsPerNode" %notin% colnames(df)) {
    df$NumMPIsPerNode <- 1
  }

  if ("NumThreadsPerMPI" %notin% colnames(df)) {
    if ("Threads" %in% colnames(df)) {
      df$NumThreadsPerMPI <- df$Threads
    } else if ("NumThreads" %in% colnames(df)) {
      df$NumThreadsPerMPI <- df$NumThreads
    } else {
      df$NumThreadsPerMPI <- 1
    }
  }

  if ("Timeout" %notin% colnames(df)) {
    df$Timeout <- FALSE
  }

  if ("Failed" %notin% colnames(df)) {
    df$Failed <- FALSE
  }

  if ("Epsilon" %notin% colnames(df)) {
    cli::cli_alert_warning("No Epsilon column. Set value default to 3%")
    df$Epsilon <- 0.03
  }

  if ("Balance" %notin% colnames(df)) {
    if ("Imbalance" %in% colnames(df)) {
      df$Balance <- df$Imbalance
    } else {
      cli::cli_alert_warning("No balance column. Ignoring it")
      df$Balance <- df$Epsilon
    }
  }

  if ("Cut" %notin% colnames(df)) {
    df$Cut <- NA
  }

  if ("Seed" %notin% colnames(df)) {
    df$Seed <- 0
  }

  if ("M" %notin% colnames(df)) {
    df$M <- -1
  }

  if ("N" %notin% colnames(df)) {
    df$N <- -1
  }

  df$NumPEs <- df$NumNodes * df$NumMPIsPerNode * df$NumThreadsPerMPI
  df$Algorithm <- name

  cli::cli_text()

  aggregate_data(df, 3600, weakscaling_aggregator) %>% dplyr::arrange(Graph, K)
}

contains_invalid_column <- function(data, ...) {
  columns <- c(...)
  for (column in columns) {
    if (column %notin% colnames(data)) {
      return(TRUE)
    }

    invalid_count <- data %>%
      dplyr::filter(!!rlang::sym(column) == Inf) %>%
      dplyr::count()
    if (invalid_count > 0) {
      return(TRUE)
    }
  }

  return(FALSE)
}

filter_data <- function(data) {
  ks <- unique(data[[1]]$K)
  graphs <- unique(data[[1]]$Graph)

  for (i in seq_along(data)) {
    n_before <- nrow(data[[i]])
    data[[i]] <- data[[i]] %>%
      dplyr::filter(K %in% ks) %>%
      dplyr::filter(Graph %in% graphs) %>%
      dplyr::arrange(Graph, K)
    n_after <- nrow(data[[i]])

    n_diff <- n_before - n_after
    if (n_diff > 0) {
      cli::cli_alert_warning(
        "Removed {n_diff} rows from {col_magenta(data[[i]]$Algorithm[1])}"
      )
    }
  }

  data
}

validate_data <- function(data) {
  exp_nrows <- nrow(data[[1]])

  for (row in 1:exp_nrows) {
    first_graph <- data[[1]][row, "Graph"]
    first_k <- data[[1]][row, "K"]

    for (i in seq_along(data)) {
      cur_graph <- data[[i]][row, "Graph"]
      cur_k <- data[[i]][row, "K"]

      if (first_graph != cur_graph || first_k != cur_k) {
        cli::cli_alert_danger("Mismatch in row {row} for {col_magenta(algorithm[i])}")
        cli::cli_text("Expected graph={first_graph}, k={first_k}")
        cli::cli_text("Got graph={cur_graph}, k={cur_k}")
        quit()
      }
    }
  }
}
