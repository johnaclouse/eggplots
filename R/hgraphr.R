#' hgraphr
#'
#' A plot for showing high dimensional attributes with indication of percentil ranges.
#'
#' @param df data frame containing values to be plotted
#'
#' @return NULL
#' @export
#'
#' @examples
#' df = data.frame(
#' x = factor(state.name[1:18]),
#' y = runif(18)
#' )
#' hgraphr(df)
hgraphr <- function(df) {
  x <- y <- NULL
  ggplot2::ggplot(data = df,
                  ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 0.8),
              fill = "#ffd7b5",
              alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 0.9),
              fill = "#ffb38a",
              alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0.9, ymax = 0.95),
              fill = "#ff9248",
              alpha = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0.95, ymax = 1),
              fill = "#ff6700",
              alpha = 0.1) +
    ggplot2::geom_point(size = 3, shape = 21) +
    ggplot2::geom_text(ggplot2::aes(y = 1.2, label = x)) +
    ggplot2::scale_y_continuous(limits = c(0,1.2)) +
    ggplot2::coord_polar(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank())
}

# shape outline for geographic data, shape fill for individual data
# color labels gray < 0.5
