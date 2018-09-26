#' Extract colors from images.
#'
#' \code{get_colors} extract colors from Windows BMP, JPEG, PNG, TIFF, and SVG format images.
#'
#' @importFrom purrr map2
#' @importFrom tibble tibble
#'
#' @param img path or url to image.
#' @param exclude_col vector of colors to be excluded from the analysis. The built-in colors (see \code{colors()}) and/or
#'    hex color codes can be used.
#' @param exclude_rad numeric vector with blurring of the colors to be excluded. Corresponds to a maximum spherical distance
#'    in the RGB color space (all dimensions range from 0 to 255). If \code{is.null}, only the exact colors are excluded.
#'    If input is of length 1, the same blurring is applied to all elements of \code{exclude_col}.
#' @param top_n display the most frequent colors.
#' @param min_share display the colors with a minimum share of all pixels (0-1).
#' @param get_stats if \code{TRUE}, absolute and relative frequency of the colors are also included in the response.
#'
#' @return If \code{get_stats} is set to \code{FALSE} a \code{character vector} containing the hex color codes is returned. Otherwise,
#'    a \code{data.frame} (\code{tibble::tibble}) is returned with the following columns:
#' \itemize{
#' \item \code{col_hex} hex color code.
#' \item \code{col_freq} absolute frequency of the color.
#' \item \code{col_share} relative frequency of the color.
#' }
#'
#' @export
#'
#' @examples
#' # Extract all colors
#' pic1 <- system.file("extdata", "pic1.png", package = "colorfindr")
#' get_colors(pic1)
#'
#' # Extract three most frequent colors
#' pic2 <- system.file("extdata", "pic2.tif", package = "colorfindr")
#' get_colors(pic2, top_n = 3)
#'
#' # Extract colors that fill over 20% of the area
#' pic3 <- system.file("extdata", "pic3.jpg", package = "colorfindr")
#' get_colors(pic3, min_share = 0.2)
#'
#' # Extract all colors except white
#' pic4 <- system.file("extdata", "pic4.bmp", package = "colorfindr")
#' get_colors(pic4, exclude_col = "white")
#'
get_colors <- function(img, exclude_col = NULL, exclude_rad = NULL, top_n = NULL,
                       min_share = NULL, get_stats = TRUE) {

  # Check format
  check_format(img)

  # Get color ranking
  ranking <- pic_to_ranking(img)

  # Exclude colors
  if (!is.null(exclude_col)) {

    if (is.null(exclude_rad)) exclude_rad <- 0
    if (length(exclude_rad) == 1 & length(exclude_col) > 1) exclude_rad <- rep(exclude_rad, length(exclude_col))
    exclude_colors <- unique(unlist(purrr::map2(exclude_col, exclude_rad, get_blurred_colors)))
    ranking <- ranking[!ranking[["col_hex"]] %in% exclude_colors,]

  }

  # Reduce colors by ranking place
  if (!is.null(top_n)) ranking <- ranking[c(1:top_n),]

  # Reduce colors by share
  if (!is.null(min_share)) ranking <- ranking[ranking[["col_share"]] >= min_share,]

  # Prepare output
  if (!get_stats) output <- ranking[["col_hex"]]
  if (get_stats) output <- ranking

  # Return colors
  return(output)

  }
