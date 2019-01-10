#' Create a color palette from an image.
#'
#' \code{make_palette} creates a color palette from colors extracted from Windows BMP, JPEG, PNG, TIFF, and SVG format images with
#'     the \code{get_colors} function.
#'
#' @importFrom purrr map2
#' @importFrom grDevices col2rgb
#' @importFrom stats kmeans
#'
#' @param data a \code{data.frame} from a \code{get_colors} call consisting of the columns \code{col_hex}, \code{col_freq},
#'    \code{col_share}.
#' @param n the number of discrete colors to be extracted from the data.
#' @param clust_method specifies the method used to cluster the pixels. By default, the colors are clustered by the
#'     \code{\link[stats:kmeans]{k-means}} method. Alternatively, a \href{https://en.wikipedia.org/wiki/Median_cut}{median cut}
#'     approach \code{"median_cut"} can be used.
#' @param extract_method specifies the process for extracting the colors from the clusters obtained. By default \code{"hex_freq"},
#'     the most common hex colors per cluster are returned. Alternatively, the cluster-specific\code{"mean"}, \code{"median"} or
#'     \code{"mode"} of the RGB values can be used to define the desired number of hex colors.
#' @param show by default \code{"TRUE"}, the generated color palette is displayed.
#'
#' @return A character vector with hex color codes, sorted by the weight of the associated clusters.
#'
#' @export
#'
#' @examples
#' # Create palette from image
#' img <- system.file("extdata", "pic6.png", package = "colorfindr")
#' colors <- get_colors(img)
#' make_palette(colors)
#'
make_palette <- function(data, n = 10, clust_method = "kmeans", extract_method = "hex_freq", show = TRUE) {

  # Check n > 0 clust und extract
  if (n <= 0) stop()

  # Recover all pixels
  all <- unlist(purrr::map2(data[["col_hex"]], data[["col_freq"]], rep))

  # Back to RGB
  all_rgb <- t(grDevices::col2rgb(all))

  # Perform clustering
  if (clust_method == "median_cut") clusters <- median_cut(all_rgb, n = n)
  if (clust_method == "kmeans") clusters <- stats::kmeans(all_rgb, centers = n, iter.max = 20)[["cluster"]]

  # Get colors
  if (extract_method == "hex_freq") output <- most_frequent_hex_per_cluster(all, clusters)
  if (!extract_method == "hex_freq") output <- rgb_per_cluster(all_rgb, clusters, method = extract_method)

  # Show colors
  if (show) show_palette(output)

  # Return output
  return(output)

}

