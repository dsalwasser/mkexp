#!/usr/bin/env Rscript
source("common.R")

show_timeouts <- function(df, option) {
  option == "always" || (option == "auto" && any(df$Timeout))
}

show_infeasibles <- function(df, option) {
  option == "always" || (option == "auto" && any(df$Infeasible))
}

show_fails <- function(df, option) {
  option == "always" || (option == "auto" && any(df$Failed))
}

create_boxplot <- function(...,
                           statistic = "Time",
                           column_statistic = "AvgTime",
                           column_algorithm = "Algorithm",
                           column_timeout = "Timeout",
                           column_infeasible = "Infeasible",
                           column_failed = "Failed",
                           primary_key = c("Graph", "K"),
                           tick_timeout = "auto",
                           tick_infeasible = "auto",
                           tick_failed = "make_infeasible",
                           tick_errors_space_below = 0.8,
                           tick_errors_space_between = 0.8,
                           label_pdf_timeout = "timeout",
                           label_pdf_infeasible = "infeasible",
                           label_pdf_failed = "failed",
                           colors = c(),
                           levels = c(),
                           column_fill = "",
                           annotation = data.frame(),
                           position_y = "left",
                           tiny = FALSE) {
  all_datasets <- list(...)
  stopifnot(length(all_datasets) > 0)

  # Sort by primary key
  for (dataset in all_datasets) {
    dataset <- dataset %>% dplyr::arrange_at(primary_key)
  }

  # Check for consistent data
  first_dataset <- all_datasets[[1]]
  for (dataset in all_datasets) {
    stopifnot(column_statistic %in% colnames(dataset))
    stopifnot(column_algorithm %in% colnames(dataset))
    stopifnot(column_timeout %in% colnames(dataset))
    stopifnot(column_infeasible %in% colnames(dataset))
    stopifnot(NaN %notin% dataset[[column_statistic]])
    stopifnot(NA %notin% dataset[[column_statistic]])
    stopifnot(-Inf %notin% dataset[[column_statistic]])
    # stopifnot(0 %notin% dataset[[column_statistic]])
    stopifnot(nrow(dataset) == nrow(first_dataset))
    stopifnot(dataset[, primary_key] == first_dataset[, primary_key])
  }

  # Merge data to one data frame
  use_fill <- nchar(column_fill) > 0
  if (use_fill) {
    pp_data <- rbind(...) %>% dplyr::select(
      Algorithm = rlang::sym(column_algorithm),
      Statistic = rlang::sym(column_statistic),
      JitterStatistic = rlang::sym(column_statistic),
      Timeout = rlang::sym(column_timeout),
      Infeasible = rlang::sym(column_infeasible),
      Failed = rlang::sym(column_failed),
      Fill = rlang::sym(column_fill)
    )
  } else {
    pp_data <- rbind(...) %>% dplyr::select(
      Algorithm = rlang::sym(column_algorithm),
      Statistic = rlang::sym(column_statistic),
      JitterStatistic = rlang::sym(column_statistic),
      Timeout = rlang::sym(column_timeout),
      Infeasible = rlang::sym(column_infeasible),
      Failed = rlang::sym(column_failed)
    )
  }

  # Only keep algorithms that track the statistic
  pp_data <- pp_data %>% dplyr::filter(Statistic > 0)
  if (nrow(annotation) > 0) {
    annotation <- annotation %>% dplyr::filter(!!rlang::sym(statistic) > 0)
  }

  if (length(levels) > 0) {
    pp_data$Algorithm <- factor(
      pp_data$Algorithm,
      levels = levels,
      ordered = TRUE
    )
  }

  # Find max time
  min_max_time <- pp_data %>%
    dplyr::filter(!Timeout & !Infeasible & !Failed) %>%
    dplyr::summarize(Min = min(Statistic), Max = max(Statistic))
  min_time_log10 <- 0
  max_time_log10 <- ifelse(min_max_time$Max < 10,
    1,
    ceiling(log10(min_max_time$Max))
  )

  # Create ticks
  y_breaks <- 10^seq(min_time_log10, max_time_log10, by = 1)
  y_labels <- sapply(y_breaks, \(val) paste0("10e", log10(val)))
  y_breaks <- c(0, y_breaks)
  y_labels <- c("0", y_labels)

  # Remap infeasible solutions, timeouts and failed runs
  show_infeasible_tick <- show_infeasibles(pp_data, tick_infeasible)
  show_timeout_tick <- show_timeouts(pp_data, tick_timeout)
  show_failed_tick <- show_fails(pp_data, tick_failed)
  show_error_ticks <- show_infeasible_tick || show_timeout_tick || show_failed_tick

  if (tick_failed == "make_infeasible") {
    pp_data <- pp_data %>% dplyr::mutate(Infeasible = Infeasible | Failed)
  }

  offset <- tick_errors_space_below - tick_errors_space_below

  if (show_infeasibles(pp_data, tick_infeasible)) {
    offset <- offset + tick_errors_space_between
    y_breaks <- c(y_breaks, 10^(max_time_log10 + offset))
    y_labels <- c(y_labels, label_pdf_infeasible)
  }

  pp_data <- pp_data %>% dplyr::mutate(
    JitterStatistic = ifelse(Infeasible & !Timeout,
      10^(max_time_log10 + offset),
      JitterStatistic
    ),
    Statistic = ifelse(Infeasible & !Timeout, NA, Statistic)
  )

  if (show_timeouts(pp_data, tick_timeout)) {
    offset <- offset + tick_errors_space_between
    y_breaks <- c(y_breaks, 10^(max_time_log10 + offset))
    y_labels <- c(y_labels, label_pdf_timeout)
  }

  pp_data <- pp_data %>% dplyr::mutate(JitterStatistic = ifelse(Timeout,
    10^(max_time_log10 + offset),
    JitterStatistic
  ))

  if (show_fails(pp_data, tick_failed)) {
    offset <- offset + tick_errors_space_between
    y_breaks <- c(y_breaks, 10^(max_time_log10 + offset))
    y_labels <- c(y_labels, label_pdf_failed)
  }

  pp_data <- pp_data %>% dplyr::mutate(
    JitterStatistic = ifelse(Failed & !Timeout & !Infeasible,
      10^(max_time_log10 + offset),
      JitterStatistic
    ),
    Statistic = ifelse(Failed & !Timeout & !Infeasible, NA, Statistic)
  )

  stopifnot(nrow(pp_data %>% dplyr::filter(Statistic == Inf)) == 0)

  jitter_size <- if (tiny) 0.5 else 0.75


  p <- if (use_fill) {
    ggplot2::ggplot(pp_data, aes(x = Algorithm, y = Statistic, fill = Fill)) +
      ggplot2::geom_jitter(
        aes(y = JitterStatistic, color = Algorithm, fill = Fill),
        size = jitter_size,
        alpha = 0.33,
        pch = 21,
        width = 0.3
      )
  } else {
    ggplot2::ggplot(pp_data, aes(x = Algorithm, y = Statistic)) +
      ggplot2::geom_jitter(
        aes(y = JitterStatistic, color = Algorithm, fill = Algorithm),
        size = jitter_size,
        alpha = 0.33,
        pch = 21,
        width = 0.3
      )
  }

  p <- p +
    ggplot2::stat_boxplot(
      aes(color = Algorithm),
      geom = "errorbar",
      width = 0.6
    ) +
    ggplot2::geom_boxplot(
      aes(color = Algorithm),
      outlier.shape = NA,
      alpha = 0.5
    ) +
    ggplot2::scale_y_continuous(
      trans = "log10",
      breaks = y_breaks,
      labels = y_labels,
      position = position_y
    ) +
    ggplot2::labs(y = statistic) +
    ggplot2::theme_bw()

  if (show_error_ticks) {
    p <- p + ggplot2::geom_hline(
      yintercept = 10^(max_time_log10 + tick_errors_space_below / 2)
    )
  }

  if (nrow(annotation) > 0) {
    if (use_fill) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          x = Algorithm,
          y = 0,
          label = sprintf("%.1f", !!rlang::sym(statistic)),
          vjust = -0.5,
          group = Fill
        ),
        annotation,
        position = position_dodge(width = .9),
        size = 2.5
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(
          x = Algorithm,
          label = sprintf("%.1f", !!rlang::sym(statistic)),
          vjust = -0.5
        ),
        y = -Inf,
        annotation,
        size = 2.5
      )
    }
  }

  # Set colors
  if (length(colors) > 0) {
    p <- p +
      ggplot2::scale_color_manual(name = "Algorithm", values = colors) +
      ggplot2::scale_fill_manual(name = "Algorithm", values = colors)
  }

  return(p)
}
