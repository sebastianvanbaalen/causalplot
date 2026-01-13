# Internal: template implementation for "1121"
# Layout: IV -> M1(center) -> [M2_top, M2_bottom] -> DV(center)
.causal_template_1121 <- function(
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

  boxes <- data.frame(
    xmin  = c(1,  4,  7,  7, 10),
    xmax  = c(3,  6,  9,  9, 12),
    ymin  = c(2.4, 2.4, 3.8, 1.0, 2.4),
    ymax  = c(3.6, 3.6, 5.0, 2.2, 3.6),
    label = labels,
    stringsAsFactors = FALSE
  )
  boxes <- .add_box_centers(boxes)

  boxes$label_display <- vapply(
    seq_len(nrow(boxes)),
    FUN.VALUE = character(1),
    FUN = function(i) wrap3_or_error(boxes$label[i], width = wrap_width, idx = i)
  )

  # Index map:
  # 1 = IV, 2 = M1, 3 = M2_top, 4 = M2_bottom, 5 = DV
  # Rules (as corrected earlier):
  # - M1 -> M2_* ends in middle of target box (left-center)
  # - M2_* -> DV starts from middle of source box (right-center), ends at DV corners
  arrows <- rbind(
    data.frame( # IV -> M1 (straight)
      x = boxes$xmax[1], y = boxes$y[1],
      xend = boxes$xmin[2], yend = boxes$y[2]
    ),
    data.frame( # M1 top-right corner -> M2_top left-center
      x = boxes$xmax[2], y = boxes$ymax[2],
      xend = boxes$xmin[3], yend = boxes$y[3]
    ),
    data.frame( # M1 bottom-right corner -> M2_bottom left-center
      x = boxes$xmax[2], y = boxes$ymin[2],
      xend = boxes$xmin[4], yend = boxes$y[4]
    ),
    data.frame( # M2_top right-center -> DV top-left corner
      x = boxes$xmax[3], y = boxes$y[3],
      xend = boxes$xmin[5], yend = boxes$ymax[5]
    ),
    data.frame( # M2_bottom right-center -> DV bottom-left corner
      x = boxes$xmax[4], y = boxes$y[4],
      xend = boxes$xmin[5], yend = boxes$ymin[5]
    )
  )

  fills <- rep(fill_mechanisms, nrow(boxes))
  fills[c(1, nrow(boxes))] <- fill_variables

  poly_df <- .boxes_to_poly_df(boxes = boxes, fills = fills)

  ggplot2::ggplot() +
    ggforce::geom_shape(
      data = poly_df,
      ggplot2::aes(x = x, y = y, group = id, fill = fill),
      radius = radius_unit,
      colour = NA
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      arrow = grid::arrow(type = "closed", ends = "last", length = arrow_length),
      linewidth = arrow_linewidth,
      lineend = "round"
    ) +
    ggtext::geom_richtext(
      data = boxes,
      ggplot2::aes(x = x, y = y, label = label_display),
      size = text_size,
      family = font,
      colour = text_color,
      vjust = 0.5,
      lineheight = 1,
      fill = NA,
      label.color = NA,
      label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = ylim) +
    ggplot2::coord_fixed(ratio = box_ratio) +
    ggplot2::theme_void()
}
