#Shiny_Leaflet_0.1

server <- function(input, output, session) {
  assets <- reactiveValues(data = data.frame())  # Store user inputs in a reactive data frame
  
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
      
      new_asset$Carbon_ESI <- new_asset$CO2Emissions * 2.80E-12 * 10^6 
      new_asset$Land_ESI <- new_asset$LandUse * 10^6 * Land_ESI_matrix[new_asset$Region, new_asset$VegType]
      new_asset$Water_ESI <- new_asset$WaterUse * 10^6 * Water_ESI_matrix[new_asset$Region, new_asset$VegType]
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

  
  ## event data
  # Reactive expression to generate heatmap data
  heatmap_data_reactive <- reactive({
    req(input$co2_emissions_map, input$land_use_map, input$water_use_map)  # Ensure inputs are available
    
    r_3857 %>%
      mutate(
        ESI = 10^6 * ((input$land_use_map * l_esi) + (input$co2_emissions_map * c_esi) + (input$water_use_map * w_esi)),
        L_ESI = 10^6 * ((input$land_use_map * l_esi)),
        W_ESI = 10^6 * ((input$water_use_map * w_esi)),
        C_ESI = 10^6 * ((input$co2_emissions_map * c_esi))
      )
  })
  
  # Create the heatmap plot
  output$world_map_plot <- renderLeaflet({
    # Ensure that the heatmap_data is available
    heatmap_data <- heatmap_data_reactive()
    
    heatmap_colors <- colorNumeric(c("#BFE499", "#F9DF8B", "#f7b267","#f79d65","#f4845f","#f27059","#f25c54", "#F05C42", "#ED3C1D"), values(heatmap_data$ESI),  na.color = "transparent")
    custom_green_colors <- colorNumeric(c("#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"), domain = values(heatmap_data$L_ESI),  na.color = "transparent")
    custom_blue_colors <- colorNumeric(c("#86C5DA", "#5DAFD3", "#349ACD", "#1E80B0", "#17679A", "#115085", "#0B3A6F", "#062659", "#021443"), domain = values(heatmap_data$W_ESI),  na.color = "transparent")
    veg_colors <- colorFactor(c("#023436","#513C2C","#2a9d8f","#f4a261", "#e9c46a"), domain = 2:6,  na.color = "transparent")
    
    # Check for user inputs
    if (input$co2_emissions_map == 0 && input$land_use_map == 0 && input$water_use_map == 0) 
      # Blank map when no inputs are provided
      {
      leaflet() %>% 
        #addTiles() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%  # background
        addProviderTiles("Stadia.StamenTonerLines",    # emphasize boundaries/lines
                         options = providerTileOptions(opacity = 0.4)) %>%
        addSearchOSM(searchOptions(zoom = 6, textPlaceholder  = "Search...", 
                                   marker = list(icon = NULL, animate = TRUE, circle = list(radius = 10, weight = 0, color = "#e03", stroke = FALSE, fill = FALSE)))) %>%
        setMaxBounds(-180, 83.5, 190.2, -85) %>%
        setView(0,0, zoom = 2) %>%
        addRasterImage(heatmap_data$vegetation, colors = veg_colors, opacity = 0.6, project = FALSE, group = "Vegetation") %>%
        addLegend(pal = veg_colors, values = 2:6, title = "Vegetation", position = "bottomright", group = "Vegetation",
                  labFormat = function(type, cuts, p) {
                    # Map the numeric breaks to your vegetation names
                    veg_lookup[as.character(cuts)]
                  })
      } 
    else 
      {
      # Map with data when inputs are provided
      leaflet(heatmap_data) %>%
          addProviderTiles("OpenStreetMap.Mapnik") %>%
          #addProviderTiles("Stadia.StamenTonerLite") %>%  # background
          addProviderTiles("Stadia.StamenTonerLines",    # emphasize boundaries/lines
                           options = providerTileOptions(opacity = 0.4)) %>%
          addSearchOSM(searchOptions(zoom = 6)) %>%
        addRasterImage(heatmap_data$ESI, colors = heatmap_colors2, opacity = 0.8, project = FALSE, group = "ESI") %>%
        addRasterImage(heatmap_data$L_ESI, colors = custom_green_colors2, opacity = 0.8, project = FALSE, group = "Land ESI") %>%
        addRasterImage(heatmap_data$W_ESI, colors = custom_blue_colors2, opacity = 0.8, project = FALSE, group = "Water ESI") %>%
        addRasterImage(heatmap_data$C_ESI, colors = reds, opacity = 0.8, project = FALSE, group = "Carbon ESI") %>%
        addRasterImage(heatmap_data$vegetation, colors = veg_colors, opacity = 0.8, project = FALSE, group = "Vegetation") %>%
        addLegend(pal = heatmap_colors, values = values(heatmap_data$ESI), title = "ESI", position = "bottomright", group = "ESI") %>%
        #addLegend(pal = custom_green_colors, values = (values(heatmap_data$L_ESI)), title = "Land ESI", position = "bottomright", group = "Land ESI") %>%
        #addLegend(colors = c("#FFFFFF",  "#F05C42"), labels = c(0, max(values(heatmap_data$C_ESI), na.rm = TRUE)), title = "CO2 ESI", position = "bottomright", group = "Carbon ESI") %>%
        #addLegend(pal = custom_blue_colors, values = (values(heatmap_data$W_ESI)), title = "Water ESI", position = "bottomright", group = "Water ESI") %>%
        addLayersControl(baseGroups = c("ESI", "Land ESI", "Water ESI", "Carbon ESI", "Vegetation"),
                         position = "bottomleft", 
            options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup("Land ESI") %>%
          hideGroup("Water ESI") %>%
          hideGroup("Carbon ESI") %>%
          hideGroup("Vegetation") %>%
          setView(0,0, zoom = 2)
        
      }
  })
  
  observeEvent(input$world_map_plot_groups, {
    heatmap_data <- heatmap_data_reactive()
    
    heatmap_colors <- colorNumeric(c("#BFE499", "#F9DF8B", "#f7b267","#f79d65","#f4845f","#f27059","#f25c54", "#F05C42", "#ED3C1D"), values(heatmap_data$ESI),  na.color = "transparent")
    custom_green_colors <- colorNumeric(c("#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c", "#00441b"), domain = values(heatmap_data$L_ESI),  na.color = "transparent")
    custom_blue_colors <- colorNumeric(c("#86C5DA", "#5DAFD3", "#349ACD", "#1E80B0", "#17679A", "#115085", "#0B3A6F", "#062659", "#021443"), domain = values(heatmap_data$W_ESI),  na.color = "transparent")
    veg_colors <- colorFactor(c("#023436","#513C2C","#2a9d8f","#f4a261", "#e9c46a"), domain = 2:6,  na.color = "transparent")
    my_map <- leafletProxy("world_map_plot") %>% clearControls()
    
    if (input$world_map_plot_groups == 'ESI'){
      my_map <- my_map %>%
        addLegend(pal = heatmap_colors, values = values(heatmap_data$ESI), title = "ESI", position = "bottomright")
    }
    if(input$world_map_plot_groups == 'Water ESI'){
      my_map <- my_map %>%
        addLegend(pal = custom_blue_colors, values = values(heatmap_data$W_ESI), title = "Water ESI", position = "bottomright", group = "Water ESI")
    }
    if(input$world_map_plot_groups == 'Land ESI'){
      my_map <- my_map %>%
        addLegend(pal = custom_green_colors, values = values(heatmap_data$L_ESI), title = "Land ESI", position = "bottomright", group = "Land ESI")
    }
    if(input$world_map_plot_groups == 'Carbon ESI'){
      my_map <- my_map %>%
        addLegend(colors = c("#FFFFFF",  "#F05C42"), labels = c(0, round(max(values(heatmap_data$C_ESI), na.rm = TRUE), digits = 4)), title = "CO2 ESI", position = "bottomright", group = "Carbon ESI")
    }
    if(input$world_map_plot_groups == "Vegetation"){
      my_map <- my_map %>%
        addLegend(pal = veg_colors, values = 2:6, title = "Vegetation", position = "bottomright", group = "Vegetation",
                  labFormat = function(type, cuts, p) {
                    # Map the numeric breaks to your vegetation names
                    veg_lookup[as.character(cuts)]
                  })
    }
  })

  observeEvent(input$world_map_plot_click, {
    click <- input$world_map_plot_click
    
    
    # Ensure coordinates are numeric
    lng <- as.numeric(click$lng)
    lat <- as.numeric(click$lat)
    
    # Only proceed if coordinates are finite
    req(is.finite(lng), is.finite(lat))
    
    # Ensure necessary data is available
    req(heatmap_data_reactive(), click) 
    heatmap_data <- heatmap_data_reactive()
    
    # 1. Extract values from the ENTIRE SpatRaster stack
    # This assumes 'heatmap_data' contains all 4 layers: ESI, C_ESI, L_ESI, W_ESI
    coords <- matrix(c(click$lng, click$lat), ncol = 2)
    click_df <- data.frame(x = click$lng, y = click$lat)
    
    # Wrap click into a SpatVector
    click_vect <- terra::vect(click_df, geom = c("x", "y"), crs = "EPSG:4326")
    
    # Reproject click to match raster CRS
    click_3857 <- terra::project(click_vect, crs(heatmap_data))
    
    extracted_value_df <- terra::extract(
      heatmap_data, # <-- Extracting from the full stack
      click_3857,
      method = 'simple' 
    )
    
    # Add vegetation label
    extracted_value_df <- extracted_value_df %>%
      mutate(
        vegetation_label = veg_lookup[as.character(vegetation)]
      )
    
    print("--- Extracted Value Data Frame ---")
    print(extracted_value_df)
    print(paste("Columns:", ncol(extracted_value_df)))
    
    # 2. Safely check and format the values
    
    # Get the column names (which should be the layer names, e.g., "ESI")
    col_names <- names(extracted_value_df)
    
    # Check if the extraction returned data (columns should be > 1 if an ID column is added, 
    # but since you found only 1 column when extracting a single layer, we'll check for 
    # the presence of your data layer names.)
    # We will assume that if the number of layers in the stack is N, the df will have N columns 
    # (or N+1 if an ID is present). Since your previous prints showed only 1 column for 1 layer, 
    # we'll assume the columns correspond directly to the layer order.
    
    # Check if the primary ESI value is present (assuming it's the first column)
    if (ncol(extracted_value_df) >= 1 && !is.na(extracted_value_df[1, 5])) {
      
      # Use the layer names to safely access the values
      # Note: We use HTML <br> for line breaks inside the popup
      value_text <- paste0(
        "Total ESI: ", format(extracted_value_df$ESI[1], big.mark = ",", digits = 4), "<br>",
        "Land ESI: ", format(extracted_value_df$L_ESI[1], big.mark = ",", digits = 4), "<br>",
        "Water ESI: ", format(extracted_value_df$W_ESI[1], big.mark = ",", digits = 4), "<br>",
        "Carbon ESI: ", format(extracted_value_df$C_ESI[1], big.mark = ",", digits = 4), "<br>",
        "Vegetation: ", extracted_value_df$vegetation_label[1]
      )
      
    } else {
      # Fallback for clicks outside the raster extent
      value_text <- "No ESI data available at this location."
    }
    
    # 3. Show the popup on the map
    leafletProxy("world_map_plot") %>%
     # clearPopups() %>% 
      addPopups(
        lng = click$lng,
        lat = click$lat,
        popup = value_text, # Leaflet accepts HTML in the popup content
        options = popupOptions(
          closeOnClick = FALSE,  # don't close when clicking on map
          autoClose    = FALSE)
        #layerId = paste0("popup_", click$lng, "_", click$lat)
      )
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
      esi_cdf <- heatmap_data[[c("ESI", "W_ESI", "L_ESI", "C_ESI")]]
      # Write the selected ESI raster to a NetCDF file
      terra::writeCDF(esi_cdf, file, split = TRUE, overwrite = TRUE)
    }
  )
  
}

