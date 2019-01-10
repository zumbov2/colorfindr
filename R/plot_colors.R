#' Create treemaps of image color compositions
#'
#' \code{plot_colors} creates a treemap of colors extracted from Windows BMP, JPEG, PNG, TIFF, and SVG format images with
#'    the \code{get_colors} function.
#'
#' @importFrom treemap treemap
#'
#' @param data a \code{data.frame} from a \code{get_colors} call consisting of the columns \code{col_hex}, \code{col_freq},
#'    \code{col_share}.
#' @param sort specifies the sorting of the treemap rectangles. By default (\code{"color"}), the rectangles are sorted by
#'    hex color codes, starting in the upper left corner. With (\code{"size"}) the largest rectangle is placed top left.
#' @param labels by default, rectangles that are sufficiently large are provided with a label. If \code{FALSE}, then no labels are
#'    displayed.
#'
#' @export
#'
#' @examples
#' # Extract all colors
#' pic1 <- system.file("extdata", "pic1.png", package = "colorfindr")
#' col <- get_colors(pic1)
#'
#' # Plot image composition
#' plot_colors(col)
#'
plot_colors <- function(data, sort = "color", labels = TRUE) {

  # Calculate share
  data$col_share2 <- data$col_share / sum(data$col_share, na.rm = T)

  # Process arguments
  if (labels) fontsize <- 11
  if (!labels) fontsize <- 0
  if (sort == "size") sort <- "-size"

  # Plot
  suppressWarnings(
    treemap::treemap(
      data,
      index = c("col_hex"),
      vSize = "col_share2",
      vColor = "col_hex",
      type = "color",
      title = "",
      sortID = sort,
      border.lwds = 1,
      fontsize.labels = fontsize
      )
    )
  }
