#!/usr/bin/env Rscript

source("box_plot_printer.R")

create_per_instance_plot2 <- function(data, title, objective, column, file_name) {
  all_data <- data.frame()
  first <- data[[1]]
  first_memory <- do.call(rbind.data.frame, data) %>%
    dplyr::filter(AvgMemory > 0) %>%
    dplyr::group_by(Algorithm) %>%
    dplyr::filter(cur_group_id() == 1)

  for (df in data) {
    # rel_avg_objective <- dplyr::select(df, all_of(column)) / dplyr::select(first, all_of(column))
    df <- df %>% dplyr::mutate(RelAvgObjective = !!rlang::sym(column) / dplyr::select(first, all_of(column)))
    # print(rel_avg_objective)
    print("###########")
    print(df)
    all_data <- rbind(all_data, df)
  }
  all_data$K <- factor(all_data$K)
  graphs <- unique(all_data$Graph)

  side_len <- round(sqrt(length(unique(all_data$Graph))))
  normalized_data <- all_data |>
    dplyr::mutate(Title = paste0(Graph, ", D=", MaxDeg)) |>
    dplyr::mutate(MaxDegToM = MaxDeg / M.y)
  normalized_data$Title <- factor(
    normalized_data$Title,
    levels = unique(
      normalized_data$Title[
        order(normalized_data$MaxDeg, decreasing = TRUE)
      ]
    )
  )

  pdf(paste0("plots/", file_name, ".pdf"), width = side_len * 4, height = side_len * 3)

  print(normalized_data)
  p <- ggplot2::ggplot(
    normalized_data,
    aes(x = K, y = RelAvgObjective, fill = Algorithm)
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
  dev.off()

  cli::cli_alert_success("Created per instance plot {.file {file_name}} for statistic {col_magenta(column)}")
  cli::cli_text()
}
