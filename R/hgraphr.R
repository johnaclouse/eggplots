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
#'   percentile ranges {(0, 0.5), (0.5, 0.8), (0.8, 0.9), (0.9, 0.95), (0.95,
#'   1.0)}
#'
#' @return NULL
#' @export
#'
#' @examples
#' df = data.frame(
#' dimension = factor(state.name[1:18]),
#' measure = runif(18),
#' individual = sample(c(TRUE, FALSE, FALSE), 18, replace = TRUE),
#' neighborhood = sample(c(TRUE, TRUE, FALSE), 18, replace = TRUE)
#' ) %>%
#'   dplyr::mutate(measure = ifelse(individual == FALSE &
#'                                    neighborhood == FALSE, NA, measure))
#' hgraphr(df, index = 0.67)


hgraphr <- function(df,
                    index,
                    color_palette = c("#ffffff",
                                      "#ffd7b5",
                                      "#ffb38a",
                                      "#ff9248",
                                      "#ff6700")) {
  dimension <-
    measure <-
    vulnerability <- individual_color <- neighborhood_color <- NULL

  df <- df %>%
    dplyr::mutate(
      vulnerability = dplyr::case_when(measure > 0.5 ~ 1,
                                       measure <= 0.5 ~ 0.2,
                                       TRUE ~ 0.15),
      # individual_color = ifelse(individual == TRUE, "#009CFF", NA),
      individual_color = dplyr::case_when(
        individual == TRUE & measure > 0.5 ~ "#008CE6",
        individual == TRUE &
          measure <= 0.5 ~ "#99D1F4",
        TRUE ~ as.character(NA)
      ),
      # neighborhood_color = ifelse(neighborhood == TRUE, "#009CFF", "white")
      neighborhood_color = dplyr::case_when(
        neighborhood == TRUE & measure > 0.5 ~ "#008CE6",
        neighborhood == TRUE &
          measure <= 0.5 ~ "#99D1F4",
        TRUE ~ as.character(NA)
      )
    )

  level_count <- length(droplevels(df$dimension))

  radial_plot <-
    ggplot2::ggplot(data = df,
                    ggplot2::aes(x = dimension, y = measure)) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.5,
        ymax = 0.8
      ),
      fill = color_palette[2],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.8,
        ymax = 0.9
      ),
      fill = color_palette[3],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.9,
        ymax = 0.95
      ),
      fill = color_palette[4],
      alpha = 0.1
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.95,
        ymax = 1
      ),
      fill = color_palette[5],
      alpha = 0.1
    ) +
    ggplot2::geom_hline(yintercept = 0.8,
                        color = color_palette[1],
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 0.9,
                        color = color_palette[2],
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 0.95,
                        color = color_palette[3],
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
      y = 1.6 +
        c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.25) * 0.2,
      label = dimension,
      alpha = vulnerability
    )) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_y_continuous(limits = c(0, 1.9)) +
    ggplot2::coord_polar(clip = "off") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Component Measures",
      caption = "circles indicate measure containts individual data
                 crossed lines indicate measure containts neighborhood data"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )


  vertical_plot <-
    ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.5,
        ymax = 0.8
      ),
      fill = color_palette[2],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.8,
        ymax = 0.9
      ),
      fill = color_palette[3],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.9,
        ymax = 0.95
      ),
      fill = color_palette[4],
      alpha = 0.8
    ) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 0.95,
        ymax = 1
      ),
      fill = color_palette[5],
      alpha = 0.8
    ) +
    ggplot2::geom_hline(yintercept = 0.8,
                        color = color_palette[1],
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 0.9,
                        color = color_palette[2],
                        size = 0.5) +
    ggplot2::geom_hline(yintercept = 0.95,
                        color = color_palette[3],
                        size = 0.5) +
    ggplot2::geom_bar(ggplot2::aes(x = 1, y = index),
                      width = 1,
                      fill = "#008CE6",
                      stat = "identity") +
    ggplot2::scale_x_continuous(limits = c(0, 1.75),
                                breaks = NULL) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                breaks = c(0, 0.5, 0.8, 0.9, 0.95),
                                expand = c(0, 0)) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = paste("Overall index:", round(index, 2))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # panel.grid.major.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(color = NA,
                                         size = 0),
      axis.text.x = ggplot2::element_blank(),
      # axis.text.y = ggplot2::element_blank()
      panel.border = ggplot2::element_rect(
        colour = color_palette[3],
        fill = NA,
        size = 0.1
      ),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
  cowplot::plot_grid(NULL,
                     vertical_plot,
                     radial_plot,
                     align = "hv",
                     ncol = 3,
                     rel_widths = c(1, 1, 10))
}
