[![Build Status](https://travis-ci.org/zumbov2/colorfindr.svg?branch=master)](https://travis-ci.org/zumbov2/colorfindr)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# colorfindr
This R package allows you to **extract colors** from various image types (currently JPEG, PNG, TIFF, SVG, BMP). Either a tailored **report** (directly with the main function `get_colors`) or a **treemap** with the image color composition can be returned (with the function `plot_colors`).

## Installation
For regularly updated version (latest: 0.1.0), install from GitHub:
```
install.packages("devtools")
devtools::install_github("zumbov2/colorfindr")
```

## Colour composition of the South African flag
<img src="https://raw.githubusercontent.com/zumbov2/colorfindr/master/img/rsa1.png" width="800">

### Code
```
# Load packages
pacman::p_load(colorfindr, dplyr)

# Plot
get_colors(
  img = "https://upload.wikimedia.org/wikipedia/commons/a/af/Flag_of_South_Africa.svg",
  min_share = 0.05
) %>%
  plot_colors(sort = "size")
```

## Flags of Swiss Cantons
<img src="https://raw.githubusercontent.com/zumbov2/colorfindr/master/img/kt1.png" width="800">

### Code
```
# Load packages
pacman::p_load(colorfindr, dplyr)

# Images
img <- c(
  "https://upload.wikimedia.org/wikipedia/commons/b/b5/Wappen_Aargau_matt.svg",
  "https://upload.wikimedia.org/wikipedia/commons/0/0e/Wappen_Glarus_matt.svg",
  "https://upload.wikimedia.org/wikipedia/commons/4/47/Wappen_Bern_matt.svg",
  "https://upload.wikimedia.org/wikipedia/commons/d/d1/Wappen_Neuenburg_matt.svg"
)

# Plot
for (i in 1:length(img)) get_colors(img[i], top_n = 5) %>% plot_colors(sort = "size")
```
