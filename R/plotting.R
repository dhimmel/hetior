`%+replace%` <- ggplot2::`%+replace%`

#' The official ggplot2 theme of dhimmel.
#'
#' The theme is a modified version of ggplot2::theme_bw to improve space
#' conservation and improve color aesthetics.
#' @export
theme_dhimmel <- function(base_size = 11.5, base_family = "") {
  # Starts with theme_bw and then modify some parts
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # background of facet labels (element_rect; inherits from rect)
      strip.background  = ggplot2::element_rect(fill = '#fef2e2', colour = 'grey50', size = 0.2),
      # margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
      plot.margin       = grid::unit(c(2, 6, 2, 2), 'points'),
      # extra space added around legend (unit)
      legend.margin     = grid::unit(0, 'points')
    )
}
