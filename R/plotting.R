`%+replace%` <- ggplot2::`%+replace%`

#' The official ggplot2 theme of dhimmel.
#'
#' The theme is a modified version of ggplot2::theme_bw to improve space
#' conservation and improve color aesthetics.
#' @export
theme_dhimmel <- function(base_size = 11.5, base_family = "") {
  # Starts with theme_bw and then modify some parts
  # Theme options are documentated at http://docs.ggplot2.org/current/theme.html
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # background of facet labels (element_rect; inherits from rect)
      strip.background  = ggplot2::element_rect(fill = '#fef2e2', colour = 'grey50', size = 0.2),
      # margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
      plot.margin       = ggplot2::margin(t=2, r=2, b=2, l=2, unit='pt'),
      # extra space added around legend (unit)
      legend.spacing     = grid::unit(0.1, 'cm'),
      # background underneath legend keys (element_rect; inherits from rect)
      legend.key        = ggplot2::element_blank()
    )
}

