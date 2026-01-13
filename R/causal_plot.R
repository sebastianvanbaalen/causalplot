#' Create a causal plot
#'
#' Draw a simple causal diagram using ggplot2, with rounded boxes and arrows.
#'
#' Supported templates:
#' - "1111": IV -> mech1 -> mech2 -> DV (4 boxes)
#' - "111" : IV -> mech -> DV (3 boxes)
#' - "1121": IV -> one box -> two boxes -> DV (5 boxes)
#' - "1211": IV (center) -> two boxes -> one box -> DV (5 boxes)
#' - "1221": IV (center) -> two parallel paths -> DV (center) (6 boxes)
#' - "bathtub": like "1221" but only bottom path + dashed direct IV->DV (4 boxes)
#'
#' Labels are wrapped to a maximum of 3 lines. If a label would wrap to more than
#' 3 lines (given `wrap_width`), an error is thrown.
#'
#' @param labels Character vector of labels. If NULL, defaults are used.
#' @param type Character. Layout template.
#' @param fill_variables Fill color for the first and last box (variables).
#' @param fill_mechanisms Fill color for the in-between boxes (mechanisms).
#' @param corner_radius Rounded corner radius. Either a grid::unit() or a numeric (interpreted as cm).
#' @param font Font family for labels (e.g., "sans", "serif"). Default is "sans".
#' @param text_size Numeric label size.
#' @param text_color Label color.
#' @param wrap_width Integer. Approximate wrap width for labels (in characters).
#' @param arrow_length grid::unit for arrowhead size.
#' @param arrow_linewidth Numeric line width for arrows.
#' @param box_ratio Numeric. Passed to coord_fixed(ratio = box_ratio). Default 0.8.
#' @param xlim Numeric vector length 2.
#' @param ylim Numeric vector length 2.
#'
#' @return A ggplot object.
#' @export
causal_plot <- function(
    labels = NULL,
    type = "1111",
    fill_variables = "grey80",
    fill_mechanisms = "grey90",
    corner_radius = 0.12,
    font = "sans",
    text_size = 4,
    text_color = "black",
    wrap_width = 24,
    arrow_length = grid::unit(3, "mm"),
    arrow_linewidth = 0.6,
    box_ratio = 0.8,
    xlim = c(0, 13),
    ylim = c(0, 6)
) {
  if (!is.character(type) || length(type) != 1) {
    stop("`type` must be a single string (e.g., '1111').", call. = FALSE)
  }

  supported <- c("111", "1111", "1121", "1211", "1221", "bathtub")
  if (!type %in% supported) {
    stop(
      sprintf(
        "Unknown `type`: %s. Currently supported types: %s",
        sQuote(type),
        paste(sQuote(supported), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(labels) && !is.character(labels)) {
    stop("`labels` must be a character vector or NULL.", call. = FALSE)
  }

  if (is.null(labels)) {
    labels <- switch(
      type,
      "1111" = c(
        "Independent variable",
        "Causal mechanism step 1",
        "Causal mechanism step 2",
        "Dependent variable"
      ),
      "111" = c(
        "Independent variable",
        "Causal mechanism",
        "Dependent variable"
      ),
      "1121" = c(
        "Independent variable",
        "Shared mechanism step 1",
        "Path 1: mechanism step 2",
        "Path 2: mechanism step 2",
        "Dependent variable"
      ),
      "1211" = c(
        "Independent variable",
        "Path 1: mechanism step 1",
        "Path 2: mechanism step 1",
        "Shared mechanism step 2",
        "Dependent variable"
      ),
      "1221" = c(
        "Independent variable",
        "Path 1: mechanism step 1",
        "Path 2: mechanism step 1",
        "Path 1: mechanism step 2",
        "Path 2: mechanism step 2",
        "Dependent variable"
      ),
      "bathtub" = c(
        "Independent variable",
        "Bottom path: step 1",
        "Bottom path: step 2",
        "Dependent variable"
      )
    )
  }

  required_n <- switch(
    type,
    "111" = 3L,
    "1111" = 4L,
    "1121" = 5L,
    "1211" = 5L,
    "1221" = 6L,
    "bathtub" = 4L
  )

  if (length(labels) != required_n) {
    stop(
      sprintf("For type = %s, `labels` must have length %d.", sQuote(type), required_n),
      call. = FALSE
    )
  }

  # Dispatch
  if (type == "111") {
    return(.causal_template_111(
      labels = labels,
      fill_variables = fill_variables,
      fill_mechanisms = fill_mechanisms,
      corner_radius = corner_radius,
      font = font,
      text_size = text_size,
      text_color = text_color,
      wrap_width = wrap_width,
      arrow_length = arrow_length,
      arrow_linewidth = arrow_linewidth,
      box_ratio = box_ratio,
      xlim = xlim,
      ylim = ylim
    ))
  }

  if (type == "1111") {
    return(.causal_template_1111(
      labels = labels,
      fill_variables = fill_variables,
      fill_mechanisms = fill_mechanisms,
      corner_radius = corner_radius,
      font = font,
      text_size = text_size,
      text_color = text_color,
      wrap_width = wrap_width,
      arrow_length = arrow_length,
      arrow_linewidth = arrow_linewidth,
      box_ratio = box_ratio,
      xlim = xlim,
      ylim = ylim
    ))
  }

  if (type == "1121") {
    return(.causal_template_1121(
      labels = labels,
      fill_variables = fill_variables,
      fill_mechanisms = fill_mechanisms,
      corner_radius = corner_radius,
      font = font,
      text_size = text_size,
      text_color = text_color,
      wrap_width = wrap_width,
      arrow_length = arrow_length,
      arrow_linewidth = arrow_linewidth,
      box_ratio = box_ratio,
      xlim = xlim,
      ylim = ylim
    ))
  }

  if (type == "1211") {
    return(.causal_template_1211(
      labels = labels,
      fill_variables = fill_variables,
      fill_mechanisms = fill_mechanisms,
      corner_radius = corner_radius,
      font = font,
      text_size = text_size,
      text_color = text_color,
      wrap_width = wrap_width,
      arrow_length = arrow_length,
      arrow_linewidth = arrow_linewidth,
      box_ratio = box_ratio,
      xlim = xlim,
      ylim = ylim
    ))
  }

  if (type == "1221") {
    return(.causal_template_1221(
      labels = labels,
      fill_variables = fill_variables,
      fill_mechanisms = fill_mechanisms,
      corner_radius = corner_radius,
      font = font,
      text_size = text_size,
      text_color = text_color,
      wrap_width = wrap_width,
      arrow_length = arrow_length,
      arrow_linewidth = arrow_linewidth,
      box_ratio = box_ratio,
      xlim = xlim,
      ylim = ylim
    ))
  }

  if (type == "bathtub") {
    return(.causal_template_bathtub(
      labels = labels,
      fill_variables = fill_variables,
      fill_mechanisms = fill_mechanisms,
      corner_radius = corner_radius,
      font = font,
      text_size = text_size,
      text_color = text_color,
      wrap_width = wrap_width,
      arrow_length = arrow_length,
      arrow_linewidth = arrow_linewidth,
      box_ratio = box_ratio,
      xlim = xlim,
      ylim = ylim
    ))
  }

  stop("Internal error: unsupported type dispatch.", call. = FALSE)
}
