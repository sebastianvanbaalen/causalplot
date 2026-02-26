# Internal: compute box centers
.add_box_centers <- function(boxes) {
  boxes$x <- (boxes$xmin + boxes$xmax) / 2
  boxes$y <- (boxes$ymin + boxes$ymax) / 2
  boxes
}

# Internal: wrap a single label to <= max_lines or error.
# Also returns a label_display that preserves your centering behavior:
# - if the label is originally 1 line, we pad it to 2 lines (second line is " ")
#   so it behaves like your original approach and can be nudged.
.wrap_label_max_lines_or_error <- function(x, width, max_lines = 5, box_idx = NA_integer_) {
  lines <- strwrap(x, width = width)
  if (length(lines) == 0) lines <- ""

  if (length(lines) > max_lines) {
    stop(
      sprintf(
        "Text in box %s is too long: wrapping at wrap_width=%d produces %d lines (max is %d). Shorten the label or increase `wrap_width`. Label: %s",
        box_idx, width, length(lines), max_lines, sQuote(x)
      ),
      call. = FALSE
    )
  }

  n_lines <- length(lines)

  # Match the original centering strategy: pad ONLY 1-line labels
  label_display <- if (n_lines == 1) {
    paste(c(lines[1], " "), collapse = "\n")
  } else {
    paste(lines, collapse = "\n")
  }

  list(
    label_display = label_display,
    n_lines = n_lines
  )
}

# Internal: add wrapped labels and y_text to a boxes df
.add_wrapped_labels_and_y <- function(
    boxes,
    wrap_width,
    max_lines = 5,
    one_line_vshift = -0.10,
    two_line_vshift = 0,
    three_line_vshift = 0
) {
  wrapped <- Map(
    f = function(lbl, idx) .wrap_label_max_lines_or_error(
      x = lbl,
      width = wrap_width,
      max_lines = max_lines,
      box_idx = idx
    ),
    lbl = boxes$label,
    idx = seq_len(nrow(boxes))
  )

  boxes$label_display <- vapply(wrapped, `[[`, character(1), "label_display")
  boxes$n_lines <- vapply(wrapped, `[[`, integer(1), "n_lines")

  boxes$y_text <- boxes$y +
    ifelse(
      boxes$n_lines == 1, one_line_vshift,
      ifelse(boxes$n_lines == 2, two_line_vshift, three_line_vshift)
    )

  boxes
}
