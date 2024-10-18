#Shiny_Leaflet_0.1

library(shiny)
library(tidyverse)
library(DT)
library(formattable)
library(bslib)
library(leaflet)
library(terra)
library(tidyterra)
library(ncdf4)

## data for tables
Water_ESI_matrix <- read_csv("data/Water_ESI_coefficients.csv") %>%
  tibble::column_to_rownames(var = "...1") %>%
  as.matrix() 

Land_ESI_matrix <- read_csv("data/Land_ESI_coefficients(not_rounded).csv") %>%
  tibble::column_to_rownames(var = "...1") %>%
  as.matrix()

initial_data <- read_csv("data/esi_tool_sample(10^6).csv")

## data for map
df_esi_inner <- read_csv("data/df_esi_inner(withc_esi).csv")
r <- rast(df_esi_inner, crs = "EPSG:4326")

empty_data <- data.frame(
  id = character(),
  value = numeric(),
  ESI = numeric()
)

heatmap_colors2 <- c("#BFE499", "#F9DF8B", "#f7b267","#f79d65","#f4845f","#f27059","#f25c54", "#F05C42", "#ED3C1D")
custom_green_colors2 <- c("#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b")
custom_blue_colors2 <- c("#86C5DA", "#5DAFD3", "#349ACD", "#1E80B0", "#17679A", "#115085", "#0B3A6F", "#062659", "#021443")
reds <- c("#FFFFFF",  "#F05C42")
