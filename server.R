#Shiny_Leaflet_0.1

server <- function(input, output, session) {
  Water_ESI_coefficients <- as.tibble(read_csv("data/Water_ESI_coefficients.csv"))
  Land_ESI_coefficients <- as.tibble(read_csv("data/Land_ESI_coefficients.csv"))
  Land_ESI_matrix <- Land_ESI_coefficients %>%
    tibble::column_to_rownames(var = "...1") %>%
    as.matrix() 
  Water_ESI_matrix <- Water_ESI_coefficients %>%
    tibble::column_to_rownames(var = "...1") %>%
    as.matrix() 
  
  assets <- reactiveValues(data = data.frame())  # Store user inputs in a reactive data frame
  
  initial_data <- as.tibble(read_csv("data/esi_tool_sample.csv"))
  
  assets$data <- initial_data
  
  observeEvent(input$clear_data, {
    assets$data <- data.frame()  # Clear the data frame
    showNotification("Data has been cleared. Disregard the error message above and insert your asset data to view the results", type = "default", duration = 10)
  })
  
  observeEvent(input$calculate_esi, {
    
    # Check if any of the required fields are empty
    if (input$asset_name == "" || input$co2_emissions == "" || input$land_use == "" || input$water_use == "" || is.na(input$co2_emissions) || is.na(input$land_use) || is.na(input$water_use)) {
      # Display an error message using showNotification
      showNotification("Please fill in all required fields.", type = "error")
    } else {
      
      updateNumericInput(session, "co2_emissions", value = "")  # Reset CO2 Emissions input
      updateNumericInput(session, "land_use", value = "")       # Reset Land Use input
      updateNumericInput(session, "water_use", value = "")      # Reset Water Use input
      updateTextInput(session, "asset_name", value = "")         # Reset Asset Name input
      new_asset <- data.frame(
        AssetName = input$asset_name,
        Region = input$region,
        VegType = input$veg_type,
        CO2Emissions = input$co2_emissions,
        LandUse = input$land_use,
        WaterUse = input$water_use
      )
      
      new_asset$Carbon_ESI <- new_asset$CO2Emissions * 2.80E-12
      new_asset$Land_ESI <- new_asset$LandUse * Land_ESI_matrix[new_asset$Region, new_asset$VegType]
      new_asset$Water_ESI <- new_asset$WaterUse * Water_ESI_matrix[new_asset$Region, new_asset$VegType]
      new_asset$Total_ESI <- new_asset$Carbon_ESI + new_asset$Land_ESI + new_asset$Water_ESI
      
      assets$data <- rbind(assets$data, new_asset)
    }})
  
  esi_formatted <- reactive({
    formatted <- assets$data %>%
      mutate(across(c("Carbon_ESI", "Land_ESI", "Water_ESI", "Total_ESI"), ~ {
        ifelse(. < 0.01, formatC(., format = "e", digits = 2), formatC(., format = "f", digits = 2, big.mark = ","))
      })) %>%
      mutate(across(c("CO2Emissions", "LandUse", "WaterUse"), ~ {
        formatC(., format = "f", digits = 0, big.mark = ",")
      }))
    formatted
  })
  
  output$esi_output <- renderDT({
    datatable(
      esi_formatted(),
      rownames = FALSE,
      options = list(
        dom = 't',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10)
    )
  })
  
  
  
  output$esi_breakdown_plot <- renderPlot({
    if (nrow(assets$data) > 0) {
      full_long <- assets$data %>%
        pivot_longer(cols = c("Carbon_ESI", "Land_ESI", "Water_ESI"),
                     names_to = 'impact_type',
                     values_to = 'ESI_value')
      
      full_long <- full_long %>%
        arrange(desc(Total_ESI))  # Sort the data frame by Total_ESI in descending order
      
      scale_plot <- (max(full_long$ESI_value))/(max(full_long$CO2Emissions))
      
      gg <- ggplot(full_long) + 
        geom_bar(aes(x = reorder(AssetName, -CO2Emissions), y = ESI_value, fill = impact_type), position = "stack", stat = "identity") +
        scale_fill_manual(values = c(Carbon_ESI = "#D62246", Land_ESI = "#238352", Water_ESI = "#5A91ED"))
      
      if (input$show_carbon_emissions) {
        gg <- gg +
          geom_line(aes(x = reorder(AssetName, -CO2Emissions), y = CO2Emissions*scale_plot, group=1), linewidth = 0.8, color = "black") +
          scale_y_continuous(sec.axis = sec_axis(~ . /scale_plot, name = "CO2 Emissions",  labels = comma))
      }
      
      gg <- gg +
        labs(x = "Assets",
             y = "Earth System Impact") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      print(gg)
    }
  })
  
  #map
  ## data for map
  df_esi_inner <- as_tibble(read_csv("data/df_esi_inner.csv"))
  r <- rast(df_esi_inner, crs = "EPSG:4326")
  
  
  empty_data <- data.frame(
    id = character(),
    value = numeric(),
    ESI = numeric()
  )
  
  ## event data
  # Reactive expression to generate heatmap data
  heatmap_data_reactive <- reactive({
    req(input$co2_emissions_map, input$land_use_map, input$water_use_map)  # Ensure inputs are available
    
    r %>%
      mutate(
        ESI = 10^6 * ((input$land_use_map * l_esi) +
                        (input$co2_emissions_map * 2.80E-12) +
                        (input$water_use_map * w_esi))
      )
  })
  
  # Create the heatmap plot
  output$world_map_plot <- renderLeaflet({
    # Ensure that the heatmap_data is available
    heatmap_data <- heatmap_data_reactive()
    
    heatmap_colors <- colorNumeric(c("#BFE499", "#F9DF8B", "#f7b267","#f79d65","#f4845f","#f27059","#f25c54", "#F05C42", "#ED3C1D"), values(heatmap_data$ESI),  na.color = "transparent")
    heatmap_colors2 <- c("#BFE499", "#F9DF8B", "#f7b267","#f79d65","#f4845f","#f27059","#f25c54", "#F05C42", "#ED3C1D")
    
    # Check for user inputs
    if (input$co2_emissions_map == 0 && input$land_use_map == 0 && input$water_use_map == 0) 
      # Blank map when no inputs are provided
      
      {
      leaflet() %>% 
        addTiles() %>%
        setMaxBounds(-180, 83.5, 190.2, -85) %>%
        setView(0,0, zoom = 2)
      } 
    else 
      {
      # Map with data when inputs are provided
      leaflet(heatmap_data) %>% addTiles() %>%
        addRasterImage(heatmap_data$ESI, colors = heatmap_colors2, opacity = 0.8) %>%
        addLegend(pal = heatmap_colors, values = values(heatmap_data$ESI), title = "ESI", position = "bottomright") %>%
          setView(0,0, zoom = 2)
    }
    
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("heatmap_data", Sys.Date(), ".asc", sep = "")
    },
    content = function(file) {
      heatmap_data <- heatmap_data_reactive()
      esi_raster <- heatmap_data[["ESI"]]
      terra::writeRaster(esi_raster, file, overwrite = TRUE)
    }
  )
  
  # Download TIFF
  output$download_tiff <- downloadHandler(
    filename = function() {
      paste("heatmap_data", Sys.Date(), ".tif", sep = "")
    },
    content = function(file) {
      heatmap_data <- heatmap_data_reactive()
      esi_raster <- heatmap_data[["ESI"]]
      terra::writeRaster(esi_raster, file, overwrite = TRUE)
    }
  )
  # Download  NETCDF
  output$download_netcdf <- downloadHandler(
    filename = function() {
      paste("heatmap_data", Sys.Date(), ".nc", sep = "")
    },
    content = function(file) {
      # Get the modified heatmap data (raster) being displayed on the map
      heatmap_data <- heatmap_data_reactive()
      esi_cdf <- heatmap_data
      # Write the selected ESI raster to a NetCDF file
      terra::writeCDF(esi_cdf, file, overwrite = TRUE)
    }
  )
  
}

