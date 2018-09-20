[![Build Status](https://travis-ci.org/zumbov2/colorfindr.svg?branch=master)](https://travis-ci.org/zumbov2/colorfindr)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

# colorfindr
This R package allows you to extract colors from various image types, returns customized reports and plots treemaps of image compositions.

## Installation
For regularly updated version (latest: 0.1.0), install from GitHub:
```
install.packages("devtools")
devtools::install_github("zumbov2/colorfindr")
```

# Examples
Are you having trouble finding the right color for your plots? `colorfindr` provides a remedy.

## Swiss Cantons
### Do you recognize them?
<img src="https://raw.githubusercontent.com/zumbov2/colorfindr/master/img/kt.png" width="800">

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
