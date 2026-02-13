#' Create a 2x2 typology diagram
#'
#' Draw a 2x2 typology diagram with four labeled quadrants, double-headed
#' axis arrows, and axis labels using ggplot2. The diagram consists of a single
#' rounded outer box divided into four quadrants by dashed midlines, with
#' double-headed arrows indicating the two dimensions.
#'
#' The `type_labels` vector maps to quadrants as follows:
#' - `type_labels[1]`: top-left (high y-axis, low x-axis)
#' - `type_labels[2]`: top-right (high y-axis, high x-axis)
#' - `type_labels[3]`: bottom-left (low y-axis, low x-axis)
#' - `type_labels[4]`: bottom-right (low y-axis, high x-axis)
#'
#' @param type_labels Character vector of length 4. Labels for each quadrant,
#'   in order: top-left, top-right, bottom-left, bottom-right.
#'   Default: `c("Type 1", "Type 2", "Type 3", "Type 4")`.
#' @param x_axis_label Character. Label for the horizontal axis (top).
#'   Default: `"Dimension 2"`.
#' @param y_axis_label Character. Label for the vertical axis (left side).
#'   Default: `"Dimension 1"`.
#' @param x_axis_values Character vector of length 2. Labels for the left (low)
#'   and right (high) ends of the x-axis. Default: `c("Low", "High")`.
#' @param y_axis_values Character vector of length 2. Labels for the top
#'   and bottom ends of the y-axis. Default: `c("Low", "High")`.
#' @param fill Fill color for the outer box. Default: `"grey90"`.
#' @param border_color Color of the outer box border. Default: `NA` (no border).
#' @param line_color Color of the dashed dividing lines. Default: `"grey50"`.
#' @param line_linewidth Numeric. Line width for the dashed dividing lines. Default: `0.5`.
#' @param corner_radius Rounded corner radius. Either a grid::unit() or a numeric
#'   (interpreted as cm). Default: `0.12`.
#' @param font Font family for labels (e.g., "sans", "serif"). Default: `"sans"`.
#' @param text_size Numeric. Text size for type labels. Default: `4`.
#' @param text_color Color for all text labels. Default: `"black"`.
#' @param axis_text_size Numeric. Text size for axis labels and endpoint values. Default: `3.5`.
#' @param wrap_width Integer. Approximate wrap width for type labels (in characters). Default: `20`.
#' @param arrow_length grid::unit for arrowhead size. Default: `grid::unit(2, "mm")`.
#' @param arrow_linewidth Numeric line width for axis arrows. Default: `0.5`.
#' @param box_ratio Numeric. Passed to coord_fixed(ratio = box_ratio). Default: `0.8`.
#' @param xlim Numeric vector of length 2, or NULL for auto-computed limits.
#' @param ylim Numeric vector of length 2, or NULL for auto-computed limits.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' # Basic typology with defaults
#' typology()
#'
#' # Custom labels
#' typology(
#'   type_labels = c("Foxes", "Eagles", "Mice", "Snakes"),
#'   x_axis_label = "Speed",
#'   y_axis_label = "Size",
#'   x_axis_values = c("Slow", "Fast"),
#'   y_axis_values = c("Small", "Large")
#' )
typology <- function(
    type_labels = c("Type 1", "Type 2", "Type 3", "Type 4"),
    x_axis_label = "Dimension 2",
    y_axis_label = "Dimension 1",
    x_axis_values = c("Low", "High"),
    y_axis_values = c("Low", "High"),
    fill = "grey90",
    border_color = NA,
    line_color = "grey50",
    line_linewidth = 0.5,
    corner_radius = 0.12,
    font = "sans",
    text_size = 4,
    text_color = "black",
    axis_text_size = 3.5,
    wrap_width = 20,
    arrow_length = grid::unit(2, "mm"),
    arrow_linewidth = 0.5,
    box_ratio = 0.8,
    xlim = NULL,
    ylim = NULL
) {
  # --- Input validation ---
  if (!is.character(type_labels) || length(type_labels) != 4) {
    stop("`type_labels` must be a character vector of length 4.", call. = FALSE)
  }
  if (!is.character(x_axis_label) || length(x_axis_label) != 1) {
    stop("`x_axis_label` must be a single string.", call. = FALSE)
  }
  if (!is.character(y_axis_label) || length(y_axis_label) != 1) {
    stop("`y_axis_label` must be a single string.", call. = FALSE)
  }
  if (!is.character(x_axis_values) || length(x_axis_values) != 2) {
    stop("`x_axis_values` must be a character vector of length 2.", call. = FALSE)
  }
  if (!is.character(y_axis_values) || length(y_axis_values) != 2) {
    stop("`y_axis_values` must be a character vector of length 2.", call. = FALSE)
  }

  # --- Corner radius (matching existing pattern) ---
  if (inherits(corner_radius, "unit")) {
    radius_unit <- corner_radius
  } else if (is.numeric(corner_radius) && length(corner_radius) == 1) {
    radius_unit <- grid::unit(corner_radius, "cm")
  } else {
    stop("`corner_radius` must be a grid::unit() or a single numeric (cm).", call. = FALSE)
  }

  # --- Outer box geometry ---
  box_xmin <- 3.7
  box_xmax <- 11.7
  box_ymin <- 1
  box_ymax <- 7
  x_mid <- (box_xmin + box_xmax) / 2 # = 7
  y_mid <- (box_ymin + box_ymax) / 2  # = 4

  # --- Outer box polygon (reuse .rect_to_poly) ---
  outer_poly <- .rect_to_poly(
    xmin = box_xmin, xmax = box_xmax,
    ymin = box_ymin, ymax = box_ymax,
    id = 1
  )
  outer_poly$fill <- fill

  # --- Quadrant centers ---
  # [1] top-left, [2] top-right, [3] bottom-left, [4] bottom-right
  quad_x <- c(
    (box_xmin + x_mid) / 2, # top-left x = 5
    (x_mid + box_xmax) / 2, # top-right x = 9
    (box_xmin + x_mid) / 2, # bottom-left x = 5
    (x_mid + box_xmax) / 2  # bottom-right x = 9
  )
  quad_y <- c(
    (y_mid + box_ymax) / 2, # top-left y = 5.5
    (y_mid + box_ymax) / 2, # top-right y = 5.5
    (box_ymin + y_mid) / 2, # bottom-left y = 2.5
    (box_ymin + y_mid) / 2  # bottom-right y = 2.5
  )

  # --- Wrap type labels ---
  wrapped_labels <- vapply(
    seq_along(type_labels),
    FUN.VALUE = character(1),
    FUN = function(i) {
      lines <- strwrap(type_labels[i], width = wrap_width)
      if (length(lines) == 0) lines <- ""
      if (length(lines) > 3) {
        stop(
          sprintf(
            "Type label %d is too long: wrapping at wrap_width=%d produces %d lines (max is 3). Shorten the label or increase `wrap_width`. Label: %s",
            i, wrap_width, length(lines), sQuote(type_labels[i])
          ),
          call. = FALSE
        )
      }
      paste(lines, collapse = "\n")
    }
  )

  # --- Dashed dividing lines ---
  dividers <- data.frame(
    x    = c(box_xmin, x_mid),
    y    = c(y_mid,    box_ymin),
    xend = c(box_xmax, x_mid),
    yend = c(y_mid,    box_ymax),
    stringsAsFactors = FALSE
  )

  # --- Axis positions ---
  # Y-axis: label (x=0.8) -> arrow (x=2.2) -> box (x=3)
  # Endpoint values sit above/below the arrow on the same x
  y_arrow_x   <- 2.2
  y_label_x   <- 0.8

  # X-axis: box top (y=7) -> arrow (y=8.5) -> label (y=9.1)
  # Endpoint values sit at arrow ends on the same y
  x_arrow_y   <- 8.5
  x_label_y   <- 9.1

  # --- Auto-compute limits ---
  if (is.null(xlim)) {
    xlim <- c(-0.2, 13.0)
  }
  if (is.null(ylim)) {
    ylim <- c(-0.2, 9.8)
  }

  # --- Build ggplot ---
  ggplot2::ggplot() +

    # Layer 1: Outer rounded box
    ggforce::geom_shape(
      data = outer_poly,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
      radius = radius_unit,
      colour = border_color
    ) +
    ggplot2::scale_fill_identity() +

    # Layer 2: Dashed dividing lines
    ggplot2::geom_segment(
      data = dividers,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      linetype = "dashed",
      colour = line_color,
      linewidth = line_linewidth
    ) +

    # Layer 3: Type labels in each quadrant
    ggplot2::annotate(
      "text",
      x = quad_x,
      y = quad_y,
      label = wrapped_labels,
      size = text_size,
      family = font,
      colour = text_color,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 1
    ) +

    # Layer 4: X-axis arrow (horizontal, double-headed, above box)
    ggplot2::annotate(
      "segment",
      x = box_xmin, xend = box_xmax,
      y = x_arrow_y, yend = x_arrow_y,
      arrow = grid::arrow(
        type = "closed", ends = "both", length = arrow_length
      ),
      linewidth = arrow_linewidth
    ) +

    # Layer 5: Y-axis arrow (vertical, double-headed, left of box)
    ggplot2::annotate(
      "segment",
      x = y_arrow_x, xend = y_arrow_x,
      y = box_ymin, yend = box_ymax,
      arrow = grid::arrow(
        type = "closed", ends = "both", length = arrow_length
      ),
      linewidth = arrow_linewidth
    ) +

    # Layer 6: X-axis label (centered above the arrow, bold)
    ggplot2::annotate(
      "text",
      x = x_mid, y = x_label_y,
      label = x_axis_label,
      size = axis_text_size,
      family = font,
      colour = text_color,
      hjust = 0.5,
      fontface = "bold"
    ) +

    # Layer 7: Y-axis label (rotated 90, reads bottom-to-top, base faces right/inward)
    ggplot2::annotate(
      "text",
      x = y_label_x, y = y_mid,
      label = y_axis_label,
      size = axis_text_size,
      family = font,
      colour = text_color,
      hjust = 0.5,
      angle = 90,
      fontface = "bold"
    ) +

    # Layer 8: X-axis endpoint values (inline with arrow, flanking each end)
    ggplot2::annotate(
      "text",
      x = c(box_xmin - 0.3, box_xmax + 0.3),
      y = c(x_arrow_y, x_arrow_y),
      label = x_axis_values,
      size = axis_text_size * 0.85,
      family = font,
      colour = text_color,
      hjust = c(1, 0),
      vjust = 0.5
    ) +

    # Layer 9: Y-axis endpoint values (inline with arrow, flanking each end)
    ggplot2::annotate(
      "text",
      x = c(y_arrow_x, y_arrow_x),
      y = c(box_ymax + 0.4, box_ymin - 0.4),
      label = y_axis_values,
      size = axis_text_size * 0.85,
      family = font,
      colour = text_color,
      hjust = 0.5,
      vjust = c(0, 1)
    ) +

    # Standard theming
    ggplot2::scale_x_continuous(
      limits = xlim, expand = ggplot2::expansion(0)
    ) +
    ggplot2::scale_y_continuous(
      limits = ylim, expand = ggplot2::expansion(0)
    ) +
    ggplot2::coord_fixed(ratio = box_ratio) +
    ggplot2::theme_void()
}
