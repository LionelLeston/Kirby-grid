library(tidyverse)
library(gridExtra)
library(grid)
library(png)
library(downloader)
library(grDevices)

comb2pngs <- function(imgs, bottom_text = NULL){
  img1 <-  grid::rasterGrob(as.raster(readPNG(imgs[1])),
                            interpolate = FALSE)
  img2 <-  grid::rasterGrob(as.raster(readPNG(imgs[2])),
                            interpolate = FALSE)
  grid.arrange(img1, img2, ncol = 2, bottom = bottom_text)
}

img.1 <- "3_outputs/figures/ALFLtop50m_AVImodel.png"
img.2 <- "3_outputs/figures/ALFLtop150m_AVImodel.png"

comb2pngs(c(img.1, img.2))

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("EBImage")
library(EBImage)
readImage("3_outputs/figures/ALFLtop50m_AVImodel.png")