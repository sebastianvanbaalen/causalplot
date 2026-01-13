# Internal: rectangle to 4-point polygon for ggforce::geom_shape
.rect_to_poly <- function(xmin, xmax, ymin, ymax, id) {
  data.frame(
    x  = c(xmin, xmax, xmax, xmin),
    y  = c(ymin, ymin, ymax, ymax),
    id = id,
    stringsAsFactors = FALSE
  )
}

# Internal: convert boxes to polygon df with fill per box
.boxes_to_poly_df <- function(boxes, fills) {
  n <- nrow(boxes)
  if (n == 0) stop("`boxes` must have at least one row.", call. = FALSE)

  poly_list <- lapply(seq_len(n), function(i) {
    .rect_to_poly(
      xmin = boxes$xmin[i],
      xmax = boxes$xmax[i],
      ymin = boxes$ymin[i],
      ymax = boxes$ymax[i],
      id   = i
    )
  })

  poly_df <- do.call(rbind, poly_list)
  fills <- rep_len(fills, n)
  poly_df$fill <- fills[poly_df$id]
  poly_df
}

# Internal: sequential arrows from box i to box i+1 (touch edges)
.linear_arrows_from_boxes <- function(boxes) {
  required <- c("xmin", "xmax", "y")
  if (!all(required %in% names(boxes))) {
    stop("`boxes` must contain: xmin, xmax, y.", call. = FALSE)
  }
  n <- nrow(boxes)
  if (n < 2) stop("Need at least 2 boxes to draw arrows.", call. = FALSE)

  data.frame(
    x    = boxes$xmax[1:(n - 1)],
    y    = boxes$y[1:(n - 1)],
    xend = boxes$xmin[2:n],
    yend = boxes$y[2:n],
    stringsAsFactors = FALSE
  )
}
