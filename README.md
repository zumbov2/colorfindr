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

## Coat of arms of the Swiss cantons
### Canton of Zürich
```
ZH <- "https://upload.wikimedia.org/wikipedia/commons/5/5a/Wappen_Z%C3%BCrich_matt.svg"

# Show all colors except black and its neighboring values
colorfindr::get_colors(ZH, exclude_col = "black", exclude_rad = 100) %>% 
  colorfindr::plot_colors()
```
<img src="https://upload.wikimedia.org/wikipedia/commons/5/5a/Wappen_Z%C3%BCrich_matt.svg" height="50">


### Canton of Bern
```
BE <- "https://upload.wikimedia.org/wikipedia/commons/4/47/Wappen_Bern_matt.svg"

# Show all colors that make up at least 10% of the image
colorfindr::get_colors(BE, min_share = 0.1) %>% 
  colorfindr::plot_colors()
```
<img src="https://upload.wikimedia.org/wikipedia/commons/4/47/Wappen_Bern_matt.svg" height="50">


### Canton of Vaud
```
VD <- "https://upload.wikimedia.org/wikipedia/commons/1/1d/Wappen_Waadt_matt.svg"

# Show thee five most frequent colors
colorfindr::get_colors(VD, top_n = 5) %>%
  colorfindr::plot_colors(sort = "size")
```
<img src="https://upload.wikimedia.org/wikipedia/commons/1/1d/Wappen_Waadt_matt.svg" height="50">
