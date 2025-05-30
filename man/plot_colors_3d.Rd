% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_colors_3D.R
\name{plot_colors_3d}
\alias{plot_colors_3d}
\title{Create interactive 3D scatterplots of image color compositions}
\usage{
plot_colors_3d(
  data,
  sample_size = 5000,
  marker_size = 2.5,
  color_space = "RGB"
)
}
\arguments{
\item{data}{a \code{data.frame} from a \code{get_colors} call consisting of the columns \code{col_hex}, \code{col_freq},
\code{col_share}.}

\item{sample_size}{the number of pixels to randomly select.}

\item{marker_size}{size of marker.}

\item{color_space}{specifies color space. By default, the colors are displayed in the \code{"RGB"} color space (x-axis: red,
y-axis: blue, z-axis: green). Alternatively, the color spaces \code{"HSL"} (hue, saturation, lightness) and \code{"HSV"}
(hue, saturation, value) can be used.}
}
\description{
\code{plot_colors_3d} calls \code{\link[plotly]{plot_ly}} and creates an interactive 3D scatterplot of
   colors extracted from Windows BMP, JPEG, PNG, TIFF, and SVG format images with the
   \code{get_colors} function in the RGB color space.
}
\examples{
# Extract all colors
pic1 <- system.file("extdata", "pic5.png", package = "colorfindr")
col <- get_colors(pic1)

# Plot image composition
plot_colors_3d(col)

}
