#' Create interactive 3D scatterplots of image color compositions
#'
#' \code{plot_colors_3d} calls \code{\link{plotly}} and creates an interactive 3D scatterplot of
#'    colors extracted from Windows BMP, JPEG, PNG, TIFF, and SVG format images with the
#'    \code{get_colors} function in the RGB color space.
#'
#' @importFrom purrr map2
#' @importFrom tibble tibble as.tibble
#' @importFrom dplyr bind_cols
#' @importFrom grDevices col2rgb rgb2hsv
#'
#' @param data a \code{data.frame} from a \code{get_colors} call consisting of the columns \code{col_hex}, \code{col_freq},
#'    \code{col_share}.
#' @param sample_size the number of pixels to randomly select.
#' @param marker_size size of marker.
#' @param color_space specifies color space. By default, the colors are displayed in the \code{"RGB"} color space (x-axis: red,
#'    y-axis: blue, z-axis: green). Alternatively, the color spaces \code{"HSL"} (hue, saturation, lightness) and \code{"HSV"}
#'    (hue, saturation, value) can be used.
#'
#' @export
#'
#' @examples
#' # Extract all colors
#' pic1 <- system.file("extdata", "pic5.png", package = "colorfindr")
#' col <- get_colors(pic1)
#'
#' # Plot image composition
#' plot_colors_3d(col)
#'
plot_colors_3d <- function(data, sample_size = 5000, marker_size = 2.5, color_space = "RGB") {

  # Recover all pixels
  all <- unlist(purrr::map2(data[["col_hex"]], data[["col_freq"]], rep))

  # Check if enough pixels for sample draw
  if (length(all) < sample_size) {

    sample_size <- length(all)
    message("sample_size exceeds number of pixels: all pixels are plotted.")

  }

  # Draw sample
  all <- tibble::tibble(
    hex = all[sample(c(1:length(all)), sample_size)]
  )

  # Convert back to RGB
  all <- dplyr::bind_cols(
    all,
    tibble::as.tibble(t(grDevices::col2rgb(all[["hex"]])))
  )

  # Check for correct color space
  if (!color_space %in% c("RGB", "HSV", "HSL")) stop("color_space must be 'RGB', 'HSV' or 'HSL'.")

  # Plot variants
  if (color_space == "RGB") {
    plot <- plot3Drgb(all, marker_size = marker_size)
  }

  if (color_space == "HSV") {
    plot <- plot3Dhsv(all, marker_size = marker_size)
  }

  if (color_space == "HSL") {
    plot <- plot3Dhsl(all, marker_size = marker_size)
  }

  plot
  }
