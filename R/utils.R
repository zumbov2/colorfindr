#' @importFrom stringr str_detect
#'
#' @noRd
check_format <- function(file) {

  if (!stringr::str_detect(tolower(file), "\\.jpg|\\.jpeg|\\.png|\\.bmp|\\.tif|\\.svg"))
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

#' @noRd
alpha_deleter <- function(arr) {

  if (dim(arr)[3] == 4) {

    # Helper matrix
    hm <- matrix(
      ncol = ncol(arr),
      nrow = nrow(arr),
      byrow = F,
      arr[,,4] > 0
    )

    # Subset array
    arr2 <- array(arr[hm], dim = c(1, length(arr[,,1][hm]), 4))

    return(arr2)

  } else {

    return(arr)

  }

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
  if (stringr::str_detect(tolower(file), "\\.svg")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(rsvg::rsvg(file))))
  if (stringr::str_detect(tolower(file), "\\.png")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(png::readPNG(read_from_path(file)))))
  if (stringr::str_detect(tolower(file), "\\.jpg|\\.jpeg")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(jpeg::readJPEG(read_from_path(file)))))
  if (stringr::str_detect(tolower(file), "\\.tif")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(tiff::readTIFF(read_from_path(file)))))
  if (stringr::str_detect(tolower(file), "\\.bmp")) pic <- suppressWarnings(pixmap::pixmapRGB(alpha_deleter(bmp::read.bmp(file))))

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

#' @importFrom magrittr "%>%"
#' @importFrom plotly plot_ly add_trace layout
#'
#' @noRd
plot3Drgb <- function(dt, marker_size = 2) {

  plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
    plotly::add_trace(
      x = dt[["red"]],
      y = dt[["green"]],
      z = dt[["blue"]],
      marker = list(
        color = dt[["hex"]],
        size = marker_size
      ),
      showlegend = F
      ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "R", range = c(0, 255)),
        yaxis = list(title = "G", range = c(0, 255)),
        zaxis = list(title = "B", range = c(0, 255)),
        camera = list(
          up = list(x = 0, y = 0, z = 1),
          center = list(x = 0, y = 0, z = -0.1),
          eye = list(x = 1.1, y = -1.98, z = 0.55)
          )
        )
      )
}

#' @importFrom magrittr "%>%"
#' @importFrom plotly plot_ly add_trace layout
#'
#' @noRd
plot3Dhsv <- function(dt, marker_size = 2) {

  # RGB to HSV
  dt <- dplyr::bind_cols(
    dt,
    tibble::as.tibble(
      t(grDevices::rgb2hsv(dt[["red"]], dt[["green"]], dt[["blue"]]))
    )
  )

  # H to radians
  dt[["h"]] <- 2 * pi * dt[["h"]]


  # Plot
  plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
    plotly::add_trace(
      x = dt[["s"]] * cos(dt[["h"]]),
      y = dt[["s"]] * sin(dt[["h"]]),
      z = dt[["v"]],
      marker = list(
        color = dt[["hex"]],
        size = marker_size
      ),
      showlegend = F
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "S * cos(H)", range = c(-1, 1)),
        yaxis = list(title = "S * sin(H)", range = c(-1, 1)),
        zaxis = list(title = "V", range = c(0, 1)),
        camera = list(
          up = list(x = 0, y = 0, z = 1),
          center = list(x = 0, y = 0, z = -0.1),
          eye = list(x = 1.1, y = -1.98, z = 0.55)
        )
      )
    )
}

#' @importFrom plotwidgets rgb2hsl
#' @importFrom magrittr "%>%"
#' @importFrom plotly plot_ly add_trace layout
#'
#' @noRd
plot3Dhsl <- function(dt, marker_size = 2) {

  # Col to HSL
  dt <- dplyr::bind_cols(
    dt,
    tibble::as.tibble(
      t(plotwidgets::col2hsl(dt[["hex"]]))
    )
  )

  # H to radians
  dt[["H"]] <- 2 * pi * (dt[["H"]]/360)

  # Plot
  plotly::plot_ly(type = "scatter3d", mode = "markers") %>%
    plotly::add_trace(
      x = dt[["S"]] * cos(dt[["H"]]),
      y = dt[["S"]] * sin(dt[["H"]]),
      z = dt[["L"]],
      marker = list(
        color = dt[["hex"]],
        size = marker_size
      ),
      showlegend = F
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(title = "S * cos(H)", range = c(-1, 1)),
        yaxis = list(title = "S * sin(H)", range = c(-1, 1)),
        zaxis = list(title = "L", range = c(0, 1)),
        camera = list(
          up = list(x = 0, y = 0, z = 1),
          center = list(x = 0, y = 0, z = -0.1),
          eye = list(x = 1.1, y = -1.98, z = 0.55)
        )
      )
    )
}
