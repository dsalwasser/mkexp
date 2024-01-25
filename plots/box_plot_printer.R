#!/usr/bin/env Rscript

source("box_plot.R")

create_annotation <- function(data, statistic, column) {
  annotation <- dplyr::tibble(
    Algorithm = factor(),
    !!rlang::sym(statistic) := numeric()
  )

  for (i in seq_along(data)) {
    annotation <- annotation |> rbind(
      data[[i]] %>%
        dplyr::group_by(Algorithm) %>%
        dplyr::summarise(!!rlang::sym(statistic) := exp(mean(log(!!rlang::sym(column)))))
    )
  }

  annotation
}

print_boxplot <- function(data, statistic, column, annotation, file_name) {
  args <- data
  args[["statistic"]] <- statistic
  args[["column_statistic"]] <- column
  args[["annotation"]] <- annotation

  all_bp <- do.call(create_boxplot, args) +
    ggplot2::theme_bw() +
    create_theme()
  cli::cli_alert_success("Created box plot {.file {file_name}} for statistic {col_magenta(column)}")

  ggplot2::ggsave(paste(file_name, "pdf", sep = "."), all_bp, "pdf", "plots")
  cli::cli_text()
}

create_per_k_annotation <- function(data, statistic, column) {
  annotation <- dplyr::tibble(
    Algorithm = factor(),
    K = numeric(),
    !!rlang::sym(statistic) := numeric()
  )

  for (i in seq_along(data)) {
    annotation <- annotation |> rbind(
      data[[i]] %>%
        dplyr::group_by(Algorithm, K) %>%
        dplyr::summarise(
          !!rlang::sym(statistic) := exp(mean(log(!!rlang::sym(column)))),
          .groups = "drop"
        )
    )
  }

  annotation
}

print_per_k_boxplot <- function(data, statistic, column, annotation, file_name) {
  pdf(paste0("plots/", file_name, ".pdf"))

  ks <- unique(data[[1]]$K)
  for (k in ks) {
    fargs <- lapply(data, function(df) {
      df %>% dplyr::filter(K == k)
    })

    fargs[["statistic"]] <- statistic
    fargs[["column_statistic"]] <- column

    fannotation <- annotation %>%
      dplyr::filter(K == k) %>%
      dplyr::select(!K)
    fargs[["annotation"]] <- fannotation

    all_bp <- do.call(create_boxplot, fargs) +
      ggplot2::theme_bw() +
      create_theme() +
      ggplot2::ggtitle(paste0("k=", k))
    print(all_bp)
  }

  dev.off()
  cli::cli_alert_success("Created per-k box plot {.file {file_name}} for statistic {col_magenta(column)}")
  cli::cli_text()
}

create_marg_annotation <- function(data, statistic, column) {
  annotation <- dplyr::tibble(
    Algorithm = factor(),
    Fill = factor(),
    !!rlang::sym(statistic) := numeric()
  )

  for (i in seq_along(data)) {
    annotation <- annotation |> rbind(
      data[[i]] %>%
        dplyr::group_by(Algorithm, Fill) %>%
        dplyr::summarise(
          !!rlang::sym(statistic) := exp(mean(log(!!rlang::sym(column)))),
          .groups = "drop"
        )
    )
  }

  annotation
}

print_marg_boxplot <- function(data, statistic, column, column_fill, annotation, file_name) {
  args <- data
  args[["statistic"]] <- statistic
  args[["column_statistic"]] <- column
  args[["column_fill"]] <- column_fill
  args[["annotation"]] <- annotation

  all_bp <- do.call(create_boxplot, args) +
    ggplot2::theme_bw() +
    create_theme()
  cli::cli_alert_success("Created marg box plot {.file {file_name}} for statistic {col_magenta(column)}")

  ggplot2::ggsave(paste(file_name, "pdf", sep = "."), all_bp, "pdf", "plots")
  cli::cli_text()
}
