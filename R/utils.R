#' @importFrom stringr str_detect
#'
#' @noRd
check_format <- function(file) {

  if (!stringr::str_detect(tolower(file), "\\.jpg|\\.png|\\.bmp|\\.tif|\\.svg"))
    stop("file does not appear to be a BMP, JPEG, PNG, TIFF, or SVG")

}

#'
#' @noRd
read_from_path <- function(file) {

  if (is.character(file)) {

    if (grepl("^https?://", file)) {
      con <- url(file, "rb")
      on.exit(close(con))
      pic <- raw()

      while (length(buf <- readBin(con, raw(), 1e+06))) {
        pic <- c(pic, buf)
      }

    }
    else if (file.exists(file)) {
      pic <- readBin(file, raw(), file.info(file)$size)
    }
    else {
      stop("Argument 'file' must be a file path, or url.")
    }
  }
  stopifnot(is.raw(pic))
  return(pic)
}

#' @importFrom stringr str_detect
#' @importFrom rsvg rsvg
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom tiff readTIFF
#' @importFrom bmp read.bmp
#' @importFrom pixmap pixmapRGB
#' @importFrom grDevices rgb
#' @importFrom tibble tibble
#'
#' @noRd
pic_to_ranking <- function(file) {

  # Read to hex
  if (stringr::str_detect(tolower(file), "\\.svg")) pic <- suppressWarnings(pixmap::pixmapRGB(rsvg::rsvg(file)))
  if (stringr::str_detect(tolower(file), "\\.png")) pic <- suppressWarnings(pixmap::pixmapRGB(png::readPNG(read_from_path(file))))
  if (stringr::str_detect(tolower(file), "\\.jpg")) pic <- suppressWarnings(pixmap::pixmapRGB(jpeg::readJPEG(read_from_path(file))))
  if (stringr::str_detect(tolower(file), "\\.tif")) pic <- suppressWarnings(pixmap::pixmapRGB(tiff::readTIFF(read_from_path(file))))
  if (stringr::str_detect(tolower(file), "\\.bmp")) pic <- suppressWarnings(pixmap::pixmapRGB(bmp::read.bmp(file)))
  if (stringr::str_detect(tolower(file), "\\.bmp")) pic <- suppressWarnings(pixmap::pixmapRGB(bmp::read.bmp(file)))

  val <- grDevices::rgb(pic@red, pic@green, pic@blue)

  # Ranking
  result <- tibble::tibble(
    col_hex = names(sort(table(val), decreasing = TRUE)),
    col_freq = as.vector(sort(table(val), decreasing = TRUE))
  )

  result$col_share = result$col_freq / sum(result$col_freq)

  return(result)

}

#' @noRd
get_blurred_colors <- function(color, radius) {

  # Get rgb
  val <- grDevices::col2rgb(color)
  red <- val[1,1]
  green <- val[2,1]
  blue <- val[3,1]

  # rgb blur
  red_blur <- seq(red - radius, red + radius, by = 1)
  green_blur <- seq(green - radius, green + radius, by = 1)
  blue_blur <- seq(blue - radius, blue + radius, by = 1)

  # rgb blur corrected
  red_blur <- red_blur[red_blur >= 0 & red_blur <= 255]
  green_blur <- green_blur[green_blur >= 0 & green_blur <= 255]
  blue_blur <- blue_blur[blue_blur >= 0 & blue_blur <= 255]

  # list
  col_blur <- list(red = red_blur, green = green_blur, blue = blue_blur)

  # combinations
  col_grid <- expand.grid(col_blur)

  # euclidean distance
  col_grid$dist <- sqrt((col_grid$red - red) ^ 2 + (col_grid$green - green) ^ 2 + (col_grid$blue - blue) ^ 2)

  #subset
  col_grid <- col_grid[col_grid$dist <= radius,]
  blurred_colors <- grDevices::rgb(col_grid$red / 255, col_grid$green / 255, col_grid$blue / 255)
  return(as.vector(blurred_colors))

}
