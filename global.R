#Shiny_Leaflet_0.1

library(shiny)
library(tidyverse)
library(DT)
library(formattable)
library(bslib)
library(leaflet)
library(terra)
library(tidyterra)

## data for tables
Water_ESI_coefficients <- as.tibble(read_csv("data/Water_ESI_coefficients.csv"))
Land_ESI_coefficients <- as.tibble(read_csv("data/Land_ESI_coefficients.csv"))
Land_ESI_matrix <- Land_ESI_coefficients %>%
  tibble::column_to_rownames(var = "...1") %>%
  as.matrix() 
Water_ESI_matrix <- Water_ESI_coefficients %>%
  tibble::column_to_rownames(var = "...1") %>%
  as.matrix() 

initial_data <- as.tibble(read_csv("data/esi_tool_sample.csv")) %>%
  mutate((across(7:10, ~ .x * 10^6)))

## data for map
df_esi_inner <- as_tibble(read_csv("data/df_esi_inner.csv"))
r <- rast(df_esi_inner, crs = "EPSG:4326")


empty_data <- data.frame(
  id = character(),
  value = numeric(),
  ESI = numeric()
)