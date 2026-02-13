# Internal: template implementation for "2221"
# Layout: [IV1_top, IV2_bottom] -> [M1_top, M1_bottom] -> [M2_top, M2_bottom] -> DV(center)
.causal_template_2221 <- function(
    labels,
    fill_variables,
    fill_mechanisms,
    corner_radius,
    font,
    text_size,
    text_color,
    wrap_width,
    arrow_length,
    arrow_linewidth,
    box_ratio,
    xlim,
    ylim
) {
  if (inherits(corner_radius, "unit")) {
    radius_unit <- corner_radius
  } else if (is.numeric(corner_radius) && length(corner_radius) == 1) {
    radius_unit <- grid::unit(corner_radius, "cm")
  } else {
    stop("`corner_radius` must be a grid::unit() or a single numeric (cm).", call. = FALSE)
  }

  wrap3_or_error <- function(x, width, max_lines = 3, idx = NA_integer_) {
    lines <- strwrap(x, width = width)
    if (length(lines) == 0) lines <- ""
    if (length(lines) > max_lines) {
      stop(
        sprintf(
          "Text in box %s is too long: wrapping at wrap_width=%d produces %d lines (max is %d). Label: %s",
          idx, width, length(lines), max_lines, sQuote(x)
        ),
        call. = FALSE
      )
    }
    paste(lines, collapse = "<br>")
  }

  # Index map:
  # 1=IV1_top, 2=IV2_bottom, 3=M1_top, 4=M1_bottom,
  # 5=M2_top, 6=M2_bottom, 7=DV_center
  boxes <- data.frame(
    xmin = c(1,  1,  4,  4,  7,  7, 10),
    xmax = c(3,  3,  6,  6,  9,  9, 12),
    ymin = c(3.8, 1.0, 3.8, 1.0, 3.8, 1.0, 2.4),
    ymax = c(5.0, 2.2, 5.0, 2.2, 5.0, 2.2, 3.6),
    label = labels,
    stringsAsFactors = FALSE
  )
  boxes <- .add_box_centers(boxes)

  # Auto-compute limits if not supplied
  if (is.null(xlim) || is.null(ylim)) {
    auto <- .compute_default_limits(boxes)
    if (is.null(xlim)) xlim <- auto$xlim
    if (is.null(ylim)) ylim <- auto$ylim
  }

  boxes$label_display <- vapply(
    seq_len(nrow(boxes)),
    FUN.VALUE = character(1),
    FUN = function(i) wrap3_or_error(boxes$label[i], width = wrap_width, idx = i)
  )

  # Arrows:
  # All 2->2 transitions are straight parallel (center-to-center)
  # Final 2->1 uses converge convention (right-center to DV corners)
  arrows <- rbind(
    data.frame( # IV1_top -> M1_top (straight)
      x = boxes$xmax[1], y = boxes$y[1],
      xend = boxes$xmin[3], yend = boxes$y[3]
    ),
    data.frame( # IV2_bottom -> M1_bottom (straight)
      x = boxes$xmax[2], y = boxes$y[2],
      xend = boxes$xmin[4], yend = boxes$y[4]
    ),
    data.frame( # M1_top -> M2_top (straight)
      x = boxes$xmax[3], y = boxes$y[3],
      xend = boxes$xmin[5], yend = boxes$y[5]
    ),
    data.frame( # M1_bottom -> M2_bottom (straight)
      x = boxes$xmax[4], y = boxes$y[4],
      xend = boxes$xmin[6], yend = boxes$y[6]
    ),
    data.frame( # M2_top right-center -> DV top-left corner
      x = boxes$xmax[5], y = boxes$y[5],
      xend = boxes$xmin[7], yend = boxes$ymax[7]
    ),
    data.frame( # M2_bottom right-center -> DV bottom-left corner
      x = boxes$xmax[6], y = boxes$y[6],
      xend = boxes$xmin[7], yend = boxes$ymin[7]
    )
  )

  # Fills: IV1, IV2, DV = variables; all mechanisms = mechanisms
  fills <- rep(fill_mechanisms, nrow(boxes))
  fills[c(1, 2, 7)] <- fill_variables

  poly_df <- .boxes_to_poly_df(boxes = boxes, fills = fills)

  ggplot2::ggplot() +
    ggforce::geom_shape(
      data = poly_df,
      ggplot2::aes(x = .data$x, y = .data$y, group = .data$id, fill = .data$fill),
      radius = radius_unit,
      colour = NA
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      arrow = grid::arrow(type = "closed", ends = "last", length = arrow_length),
      linewidth = arrow_linewidth,
      lineend = "round"
    ) +
    ggtext::geom_richtext(
      data = boxes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label_display),
      size = text_size,
      family = font,
      colour = text_color,
      vjust = 0.5,
      lineheight = 1,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    ggplot2::scale_x_continuous(limits = xlim, expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_continuous(limits = ylim, expand = ggplot2::expansion(0)) +
    ggplot2::coord_fixed(ratio = box_ratio) +
    ggplot2::theme_void()
}
