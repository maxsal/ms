#' Maxwell Salvatore's custom ggplot2 theme
#'
#' @param show_grid_lines Should grid lines be shown (TRUE/FALSE)
#' @param show_axis_titles Should axis titles be shown (TRUE/FALSE)
#' @param show_legend_title Should axis title by shown (TRUE/FALSE)
#' @param font_family Font to use in plot (Google Font, default = 'Public Sans')
#'
#' @return A complete ggplot2 theme
#' @export
#'

theme_ms <- function(
    show_grid_lines   = TRUE,
    show_axis_titles  = TRUE,
    show_legend_title = TRUE,
    font_family       = "Public Sans"
    ) {

  showtext::showtext_auto()

  if (!(font_family %in% sysfonts::font_families())) {
    cli::cli_alert_info(paste0("installing ", font_family, "..."))
    sysfonts::font_add_google(font_family)
  }

  custom_theme <- ggplot2::theme_classic() +
    ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "grey90"),
          panel.grid.minor = ggplot2::element_blank(),

          axis.ticks = ggplot2::element_blank(),

          axis.title   = ggplot2::element_text(color = "#1b1b1b"),
          axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),

          axis.text = ggplot2::element_text(color = "#1b1b1b"),

          plot.title.position = "plot",
          plot.title = ggplot2::element_text(face = "bold",
                                    margin = ggplot2::margin(b = 8),
                                    color = "#1b1b1b"),
          plot.subtitle = ggplot2::element_text(color = "#1b1b1b"),
          plot.caption  = ggplot2::element_text(hjust = 0),

          text = ggplot2::element_text(family = font_family),

          legend.position = "top")

  if (show_grid_lines == FALSE) {

    custom_theme <- custom_theme +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank())

  }

  if (show_axis_titles == FALSE) {

    custom_theme <- custom_theme +
      ggplot2::theme(axis.title   = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())

  }

  if (show_legend_title == FALSE) {
    custom_theme <- custom_theme +
      ggplot2::theme(
        legend.title = ggplot2::element_blank()
      )
  }

  custom_theme

}

#' Maxwell Salvatore's custom ggplot2 color palette
#'
#' @param black_first Should black be the first color in the palette (TRUE/FALSE)
#' @param ... Additional arguments
#'
#' @return A discrete ggplot2 color scale
#' @export
#'

scale_color_ms <- function(black_first = FALSE, ...){
  if (black_first) {
    ggplot2::discrete_scale("color", "ms", scales::manual_pal(values = c("#1b1b1b", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#005ea2", "#D55E00", "#CC79A7")), ...)
  } else {
    ggplot2::discrete_scale("color", "ms", scales::manual_pal(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#005ea2", "#D55E00", "#CC79A7", "#1b1b1b")), ...)

  }
}

#' Maxwell Salvatore's custom ggplot2 fill palette
#'
#' @param black_first Should black be the first color in the palette (TRUE/FALSE)
#' @param ... Additional arguments
#'
#' @return A discrete ggplot2 fill palette
#' @export
#'

scale_fill_ms <- function(black_first = FALSE, ...){
  if (black_first) {
    ggplot2::discrete_scale("fill", "ms", scales::manual_pal(values = c("#1b1b1b", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#005ea2", "#D55E00", "#CC79A7")), ...)
  } else {
    ggplot2::discrete_scale("fill", "ms", scales::manual_pal(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#005ea2", "#D55E00", "#CC79A7", "#1b1b1b")), ...)

  }
}
