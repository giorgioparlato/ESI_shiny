## WORKING SCRIPT BASIC

library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(formattable)
library(shinyWidgets)
library(bslib)
library(plotly)



ui <- page_navbar(title = "ESI prototype showcase", theme = bs_theme(bootswatch = "united"),
                  tags$head(tags$script(
                    HTML(
                        "<!-- Google tag (gtag.js) -->
                        <script async src='https://www.googletagmanager.com/gtag/js?id=G-20QL733ETT'></script>
                        <script>
                        window.dataLayer = window.dataLayer || [];
                        function gtag(){dataLayer.push(arguments);}
                        gtag('js', new Date());
                        gtag('config', 'G-20QL733ETT');
                        </script>"
                    )
                  ),
                  tags$style(HTML("
                    .shiny-notification {position:fixed;
                    top: calc(70%);
                    left: calc(6%);
                    padding: 0 50px 0 50px;
                    border: 1px solid red;}
                    
                    .navbar-default {
                    background-color: #E78059 !important;
                    border-color: #E78059;}
                    
                    .nav-link, .nav-tabs>li>a, .nav-pills>li>a, ul.nav.navbar-nav>li>a {
                    font-size: 1.25rem}
                    
                    .navbar {
                    --bs-navbar-brand-font-size: 1.4rem;}
                    
                    .navbar.navbar-inverse {
                    background-color: #E78059 !important;}
                    
                    .btn-primary {
                    --bs-btn-color: #fff;
                    --bs-btn-bg: #E78059;
                    --bs-btn-border-color: #E78059;
                    --bs-btn-hover-color: #fff;
                    --bs-btn-hover-bg: #c6471b;
                    --bs-btn-hover-border-color: #ba431a;
                    --bs-btn-focus-shadow-rgb: 236,110,65;
                    --bs-btn-active-color: #fff;
                    --bs-btn-active-bg: #ba431a;
                    --bs-btn-active-border-color: #af3f18;
                    --bs-btn-active-shadow: inset 0 3px 5px rgba(0,0,0,0.125);
                    --bs-btn-disabled-color: #fff;
                    --bs-btn-disabled-bg: #E78059;
                    --bs-btn-disabled-border-color: #E78059;}
    
                    @media (max-width: 767px) { 
                    .sidebar {
                    /* Set the mobile sidebar width to a fixed value */
                    width: 100% !important; 
                    min-width: 80% !important;
                    min-height: 50vh !important;
                    collapse = FALSE;
                    position: relative;}
                    
                    img[src='ESi_interactions_5.png'] {
                    width: 100% !important;}
                                    }

"

                                      ))),
## MAIN NAVBAR
                  bslib::nav_panel("About",
                                   h3("The Earth System Impact tool – some key facts and disclaimers"),
                                   h5("This web page showcases the prototype Earth System Impact (ESI) score."),
                                   p("Stability of the Earth’s climate system depends on both reducing GHG emissions while simultaneously bolstering the resilience of key regions (biomes) of the planet. Mitigating severe systemic risks related to climate change and concurrent nature degradation therefore hinges on our ability to rapidly reduce the harm incurred through economic activities."),
                                   p("By assessing the planetary scale impact of assets and providing improved information for decision-making, ESI is a tool that can improve…", br("…how companies assess the impact of their operations, or"), "…how investors assess the impact of their portfolios."),
                                   p("The ESI tool is:",
                                     tags$div(tags$ul(
                                       tags$li("Systemic: it accounts for Earth system components other than climate (CO2), such as water and landuse, and most importantly, the interactions between these three"),
                                       tags$li("Context sensitive: it distinguishes impacts by region and vegetation type"),
                                       tags$li("Science based: it accounts for the current state of each Earth system component relative to scientifically estimated guardrails – i.e. it accounts for total availability")))),
                                   p("For more details on how the prototype was developed see", a("Lade et al. 2021", href= "https://iopscience.iop.org/article/10.1088/1748-9326/ac2db1")),
                                    p("For a short introduction on ESI see", a("this brief", href = "ESI - A tool to better capture corporate and investment impacts on the Earth system_v1.1.pdf", target = "_blank"), ", and for more information on how it can be applied by corporate actors, banks or other financial institutions see", a("Crona et al. 2023 (preprint)", href = "https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4495331"), "which also includes a case study applying the ESI on a sample of mining companies."),
                                   br(),
                                   img(src='ESi_interactions_5.png', align = "center", width = "50%"),
                                   p(strong("Disclaimer", style="font-size: 20px;"), br(strong("This tool is currently a prototype. We advise caution when interpreting its results and it should not be used to replace regulatory requirements. Given its focus on planetary-scale impacts it also does not replace assessments of local environmental impacts, such as pollution or biodiversity impacts.")))),
                  bslib::nav_panel("Tool",
## TOOL PAGE
                                   page_sidebar(
                                     sidebar = sidebar(width = "30%",
                                                       collapse_sidebar = FALSE,
                                                       open = "always",
##Inner Tabs
                                                       navset_card_tab(
                                                       nav_panel("Input Data",
                                                       p("Clear the example data and fill in the information below to calculate the ESI for an asset. You can add more assets and compare their impacts on the Earth System. The bars represent the ESI broken down by each component. You can click on the checkbox above the plot to show/hide the black line representing CO2e emissions (secondary axis) of assets. The assets in the plot are automatically sorted in descending order of CO2e emissions"),
                                        selectInput(inputId = "region", label = "Region", choices = c(
                                          "Africa", "Asia", "Australia", "Europe", "Oceania", "North America", "South America"
                                        )),
                                        selectInput(inputId = "veg_type", label = "Vegetation Type", choices = c(
                                          "warm climate grassland", "boreal forest", "cool climate grassland", "temperate forest", "tropical forest"
                                        )),
                                        numericInput(inputId = "co2_emissions", label = "CO2e Emissions (tons)", value = ""),
                                        numericInput(inputId = "land_use", label = "Land Use (km2)", value = ""),
                                        numericInput(inputId = "water_use", label = "Water Use (thousand m3)", value = ""),
                                        textInput(inputId = "asset_name", label = "Asset Name", value = "Example Asset 1"),
                                        actionButton(inputId = "calculate_esi", label = "Calculate ESI for this asset", class="btn btn-primary"),
                                        actionButton(inputId = "clear_data", label = "Clear Data", class="btn btn-secondary")),
                                        
                                        nav_panel("Instructions",
                                        p("In the input data tab you can insert the data for the assets you want to analyze. The 'clear data' button allows to delete all the input values and start from scratch."),
                                        h5("Interpreting ESI scores"),
                                        p("ESI scores are scaled to planetary boundaries. Since any single company will contribute a small fraction of total regional or global impact relative to these boundaries, ESI scores are usually much smaller than
1. This small number, however, does not represent negligible impact."),
                                        h5("Errors and clearing data"),
                                        p("Note that there are some combinations of region and vegetation type for which there is no value. If you select one of those combinations you will receive an error message, and will need to 'clear data' to start again. The missing combinations are the following:"),
                                        tags$div(tags$ul(
                                          tags$li("boreal forest AND one of the following"),
                                          tags$ul(tags$li("Australia, Oceania, South America, Africa")),
                                          tags$li("Oceania AND warm OR cool climate grasslands"),
                                          tags$li("Europe AND cool climate grasslands OR tropical forest"))),
                                        p("If you encounter any other issues feel free to reach out to giorgio.parlato(at)kva.se")))),
                                     h5("Asset List"),
                                    column(width = 12, DT::dataTableOutput("esi_output"),style = "height:30%; overflow-y: scroll"),
                            #DTOutput("esi_output", style = "height:300px; overflow-y: scroll"),
                            card(card_header("ESI Breakdown"),
                                 checkboxInput(inputId = "show_carbon_emissions", label = "Show/Hide Carbon Emissions Line", value = FALSE),
                                 plotOutput("esi_breakdown_plot", height = "50%"),
                                 strong("Disclaimer: This tool is currently a prototype. We advise caution when interpreting its results and it should not be used to replace regulatory requirements. Given its focus on planetary-scale impacts it also does not replace assessments of local environmental impacts, such as pollution or biodiversity impacts."))  # Output the plot using plotOutput
                                          )))

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
  
}

shinyApp(ui = ui, server = server)


