# Internal: template implementation for "11111"
# Layout: IV -> M1 -> M2 -> M3 -> DV (5 boxes in a row)
.causal_template_11111 <- function(
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
    stop("`corner_radius` must be a grid::unit() or a single numeric (cm).",
         call. = FALSE)
  }

  # Shared wrapping helper (HTML line breaks for ggtext)
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

  # Box geometry (five evenly spaced boxes)
  boxes <- data.frame(
    xmin  = c(1,  3.5, 6,  8.5, 11),
    xmax  = c(3,  5.5, 8, 10.5, 13),
    ymin  = rep(2.4, 5),
    ymax  = rep(3.6, 5),
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
    FUN = function(i) wrap3_or_error(
      boxes$label[i],
      width = wrap_width,
      idx = i
    )
  )

  # Straight arrows between adjacent boxes
  arrows <- .linear_arrows_from_boxes(boxes)

  # Fill colors: first + last = variables, middle = mechanisms
  fills <- rep(fill_mechanisms, nrow(boxes))
  fills[c(1, nrow(boxes))] <- fill_variables

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
      arrow = grid::arrow(type = "closed", ends = "last",
                          length = arrow_length),
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
      family = font,
      size = text_size,
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
