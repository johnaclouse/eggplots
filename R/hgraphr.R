#' hgraphr
#'
#' A plot for showing high dimensional attributes with indication of percentile
#' ranges.
#'
#' @param df data frame containing values to be plotted. Data frame must
#'   contain: dimension, measure, individual, and neighborhood. Dimension is a
#'   factor, measure is numeric, and individual and neighborhood are logical.
#' @param index numeric value representing the individual's relative risk index based on the population.
#' @param color_palette vector of RGB background colors to be used for
#'   percentile ranges {(0, 50), (50, 80), (80, 90), (90, 95), (95,
#'   100)}
#'
#' @return NULL
#' @export
#'
#' @examples
#' df = data.frame(
#' dimension = factor(state.name[1:22]),
#' measure = runif(22) * 100,
#' individual = sample(c(TRUE, FALSE, FALSE), 22, replace = TRUE),
#' neighborhood = sample(c(TRUE, TRUE, FALSE), 22, replace = TRUE)
#' ) %>%
#'   dplyr::mutate(measure = ifelse(individual == FALSE &
#'                                    neighborhood == FALSE, NA, measure))
#' hgraphr(df, index = 67)


hgraphr <- function(df,
                    index,
                    color_palette = c("#feedde",
                                      "#fdd0a2",
                                      "#fdae6b",
                                      "#fd8d3c",
                                      "#f16913")) {
  dimension <-
    measure <-
    vulnerability <- individual_color <- neighborhood_color <- NULL

  df <- df %>%
    dplyr::mutate(
      vulnerability = dplyr::case_when(measure > 50 ~ 100,
                                       measure <= 50 ~ 20,
                                       TRUE ~ 15),
      # individual_color = ifelse(individual == TRUE, "#009CFF", NA),
      individual_color = dplyr::case_when(
        individual == TRUE & measure > 50 ~ "#008CE6",
        individual == TRUE &
          measure <= 50 ~ "#99D1F4",
        TRUE ~ as.character(NA)
      ),
      # neighborhood_color = ifelse(neighborhood == TRUE, "#009CFF", "white")
      neighborhood_color = dplyr::case_when(
        neighborhood == TRUE & measure > 50 ~ "#008CE6",
        neighborhood == TRUE &
          measure <= 50 ~ "#99D1F4",
        TRUE ~ as.character(NA)
      )
    )

  level_count <- length(droplevels(df$dimension))

  # prevent labels from overlapping at the top and bottom of the plot
  label_offsets <- rep(0, level_count)
  if((level_count %% 2) == 0) {
    label_offsets[c(1, (level_count / 2 + 1))] <- 1
  } else {
    label_offsets[c(1, ceiling(level_count / 2))] <- 1
  }
  label_offsets <- label_offsets * 5

  # radial ----
  radial_plot <-
    ggplot2::ggplot(data = df,
                    ggplot2::aes(x = dimension, y = measure)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0,
        ymax = 50
      ),
      fill = color_palette[1],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 50,
        ymax = 80
      ),
      fill = color_palette[2],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 80,
        ymax = 90
      ),
      fill = color_palette[3],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 90,
        ymax = 95
      ),
      fill = color_palette[4],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 95,
        ymax = 100
      ),
      fill = color_palette[5],
      alpha = 0.1
    ) +
    ggplot2::geom_hline(yintercept = 50,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 80,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 90,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 95,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_point(
      ggplot2::aes(color = individual_color,
                   alpha = vulnerability),
      size = 4,
      shape = 19
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = neighborhood_color),
      size = 4.25,
      stroke = 1.1,
      shape = 4
    ) +
    ggplot2::geom_text(ggplot2::aes(
      y = 125 + label_offsets,
      label = dimension,
      alpha = vulnerability
    )) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    # remove extra space between the plot and the axis (edge of the panel)
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0, add = -20),
                                limits = c(0,135)) +
    # align the first category at the top of the plot
    # helps with label alignment for both odd and even number of categories
    ggplot2::coord_polar(
      start = -0.5 / level_count * 2 * pi,
      clip = "off"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Dimensions",
      caption = "Individual data = circles   neighborhood data = X"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # plot.background=ggplot2::element_rect(fill='#e3fbff'),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0, "pt"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
      # plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0, unit = "cm")
    )

  # vertical ----
  vertical_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0,
        ymax = 50
      ),
      fill = color_palette[1],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 50,
        ymax = 80
      ),
      fill = color_palette[2],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 80,
        ymax = 90
      ),
      fill = color_palette[3],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 90,
        ymax = 95
      ),
      fill = color_palette[4],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 95,
        ymax = 100
      ),
      fill = color_palette[5],
      alpha = 0.8
    ) +
    ggplot2::geom_hline(yintercept = 50,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 80,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 90,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 95,
                        color = "white",
                        size = 0.5) +
    ggplot2::geom_bar(ggplot2::aes(x = 1, y = index),
                      width = 0.5,
                      fill = "#008CE6",
                      stat = "identity") +
    ggplot2::scale_x_continuous(limits = c(0.5, 1.5),
                                breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 105),
                                breaks = c(0, 50, 80, 90, 95, 100),
                                expand = c(0, 0)) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = paste("Overall index:", round(index))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # panel.grid.major.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(color = NA,
                                         size = 0),
      axis.text.x = ggplot2::element_blank(),
      # axis.text.y = ggplot2::element_blank()
      panel.border = ggplot2::element_rect(
        colour = "#008CE6",
        fill = NA,
        size = 0.1
      ),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  cowplot::plot_grid(
    NULL,
    vertical_plot,
    radial_plot,
    align = "v",
    ncol = 3,
    rel_widths = c(1, 3, 15)
  )
}
