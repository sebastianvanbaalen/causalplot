# Internal: template implementation for "111_moderator"
# Based on "111" but adds a moderator box under the middle mechanism box,
# with an upward arrow into the middle box.
#
# Layout:
#   IV  ->  Mechanism  ->  DV
#            ^
#        Moderator
.causal_template_111_moderator <- function(
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
  # Corner radius handling
  if (inherits(corner_radius, "unit")) {
    radius_unit <- corner_radius
  } else if (is.numeric(corner_radius) && length(corner_radius) == 1) {
    radius_unit <- grid::unit(corner_radius, "cm")
  } else {
    stop("`corner_radius` must be a grid::unit() or a single numeric (cm).", call. = FALSE)
  }

  # Wrap labels to <= 3 lines; use <br> for ggtext
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

  # Boxes: first three are standard 111; 4th is moderator under the middle box
  # Index map:
  # 1 = IV, 2 = Mechanism, 3 = DV, 4 = Moderator
  boxes <- data.frame(
    xmin  = c(1, 5, 9, 5),
    xmax  = c(3, 7, 11, 7),
    ymin  = c(2.4, 2.4, 2.4, 0.4),
    ymax  = c(3.6, 3.6, 3.6, 1.6),
    label = labels,
    stringsAsFactors = FALSE
  )

  boxes <- .add_box_centers(boxes)

  boxes$label_display <- vapply(
    seq_len(nrow(boxes)),
    FUN.VALUE = character(1),
    FUN = function(i) wrap3_or_error(boxes$label[i], width = wrap_width, idx = i)
  )

  # Arrows:
  # IV -> Mechanism, Mechanism -> DV (straight, touch edges)
  # Moderator -> Mechanism (upward, touch edges at bottom-center/top-center)
  arrows <- rbind(
    data.frame(
      x = boxes$xmax[1], y = boxes$y[1],
      xend = boxes$xmin[2], yend = boxes$y[2]
    ),
    data.frame(
      x = boxes$xmax[2], y = boxes$y[2],
      xend = boxes$xmin[3], yend = boxes$y[3]
    ),
    data.frame(
      x = boxes$x[4], y = boxes$ymax[4],     # top-center of moderator
      xend = boxes$x[2], yend = boxes$ymin[2] # bottom-center of mechanism
    )
  )

  # Fills:
  # Variables: IV, DV, Moderator (1, 3, 4)
  # Mechanisms: middle box (2)
  fills <- rep(fill_mechanisms, nrow(boxes))
  fills[c(1, 3, 4)] <- fill_variables

  poly_df <- .boxes_to_poly_df(boxes = boxes, fills = fills)

  ggplot2::ggplot() +
    ggforce::geom_shape(
      data = poly_df,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        group = .data$id,
        fill = .data$fill
      ),
      radius = radius_unit,
      colour = NA
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$xend,
        yend = .data$yend
      ),
      arrow = grid::arrow(type = "closed", ends = "last", length = arrow_length),
      linewidth = arrow_linewidth,
      lineend = "round"
    ) +
    ggtext::geom_richtext(
      data = boxes,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$label_display
      ),
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
