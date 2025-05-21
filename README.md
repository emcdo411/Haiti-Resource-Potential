library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(shinyjs)

# Sample data for resources
regions <- data.frame(
  region = c("Beloc", "Bainet", "Haiti South-East", "Haiti West", "Haiti Central"),
  lat = c(18.0, 18.5, 18.7, 19.0, 19.2),
  lng = c(-73.0, -73.5, -73.7, -73.8, -73.9),
  iridium_reserve = c(50000, 75000, 80000, 60000, 70000),
  oil_reserve = c(20000000, 22000000, 142000000, 941000000, 35000000),
  gas_reserve = c(159000000000, 159000000000, 1200000000000, 1200000000000, 1100000000000)
)

# Wealth projection
years <- 2025:2045
wealth_projection <- data.frame(
  year = years,
  iridium_revenue = 100000000 * (1 + seq(0, 0.2, length.out = 21)),
  oil_revenue = 150000000 * (1 + seq(0, 0.15, length.out = 21)),
  gold_revenue = 200000000 * (1 + seq(0, 0.25, length.out = 21)),
  natural_gas_revenue = 120000000 * (1 + seq(0, 0.18, length.out = 21))
)

# Job and tax projection
job_tax_years <- 2025:2035
sectors <- c("Mining (Iridium)", "Offshore Drilling", "Transportation & Logistics", "Construction", 
             "Manufacturing", "Energy Sector", "Engineering & Technology", "Environmental Services", 
             "Hospitality & Services", "Education & Training")
job_tax_projection <- expand.grid(year = job_tax_years, sector = sectors) %>%
  mutate(
    haiti_jobs = case_when(
      sector == "Mining (Iridium)" ~ round(500 + 4500 * (year - 2025) / 10, 0),
      sector == "Offshore Drilling" ~ round(1000 + 6000 * (year - 2025) / 10, 0),
      sector == "Transportation & Logistics" ~ round(800 + 4000 * (year - 2025) / 10, 0),
      sector == "Construction" ~ round(600 + 3500 * (year - 2025) / 10, 0),
      sector == "Manufacturing" ~ round(400 + 3000 * (year - 2025) / 10, 0),
      sector == "Energy Sector" ~ round(300 + 2500 * (year - 2025) / 10, 0),
      sector == "Engineering & Technology" ~ round(200 + 2000 * (year - 2025) / 10, 0),
      sector == "Environmental Services" ~ round(150 + 1500 * (year - 2025) / 10, 0),
      sector == "Hospitality & Services" ~ round(500 + 4000 * (year - 2025) / 10, 0),
      sector == "Education & Training" ~ round(100 + 1000 * (year - 2025) / 10, 0)
    ),
    us_jobs = round(haiti_jobs * 0.4, 0),
    haiti_tax = case_when(
      year <= 2030 ~ 0.05 * 500000000 * (1.1)^(year - 2025) / 10,
      TRUE ~ 0.10 * 500000000 * (1.1)^(year - 2025) / 10
    ) + 0.2 * 0.3 * 500000000 * (1.1)^(year - 2025) / 10,
    us_tax = 0.21 * 0.3 * 250000000 * (1.1)^(year - 2025) / 10
  )

# UN & Global Oversight risk data
risk_years <- 2025:2035
risk_scenarios <- c("Geopolitical Tension (China/Russia)", "Proxy Conflict (N. Korea/Cuba)", 
                    "Environmental Disaster", "Humanitarian Crisis", "Maritime Security")
risk_projection <- expand.grid(year = risk_years, scenario = risk_scenarios) %>%
  mutate(
    risk_level = case_when(
      scenario == "Geopolitical Tension (China/Russia)" ~ round(3 + 0.5 * (year - 2025), 1),
      scenario == "Proxy Conflict (N. Korea/Cuba)" ~ round(2 + 0.4 * (year - 2025), 1),
      scenario == "Environmental Disaster" ~ round(4 + 0.3 * (year - 2025), 1),
      scenario == "Humanitarian Crisis" ~ round(3 + 0.6 * (year - 2025), 1),
      scenario == "Maritime Security" ~ round(2 + 0.5 * (year - 2025), 1)
    )
  )

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Dark Theme */
      .dark-theme {
        background-color: #1C2526;
        color: #CAD2C5;
      }
      .dark-theme .title-panel {
        background: linear-gradient(to right, #C8102E, #8B0000);
        color: #CAD2C5;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        padding: 15px;
        text-align: center;
      }
      .dark-theme .sidebar {
        background-color: #2C2C2C;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        padding: 15px;
      }
      .dark-theme .nav-tabs > li > a {
        background-color: #CAD2C5;
        color: #1C2526;
        border: none;
        border-radius: 6px;
        margin-right: 5px;
        font-weight: 500;
      }
      .dark-theme .nav-tabs > li > a:hover {
        background-color: #C8102E;
        color: #CAD2C5;
      }
      .dark-theme .nav-tabs > li.active > a {
        background-color: #8B0000;
        color: #CAD2C5;
      }
      .dark-theme .tab-content {
        background-color: #2C2C2C;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
      }
      .dark-theme .btn {
        background-color: #C8102E;
        color: #CAD2C5;
        border: none;
        border-radius: 6px;
        padding: 8px 16px;
      }
      .dark-theme .btn:hover {
        background-color: #8B0000;
        color: #CAD2C5;
      }
      .dark-theme .dataTables_wrapper {
        color: #CAD2C5;
      }
      .dark-theme .table-bordered {
        border: 1px solid #CAD2C5;
      }
      .dark-theme .table-bordered th, .table-bordered td {
        border: 1px solid #CAD2C5;
      }
      .dark-theme .leaflet-container {
        background: #2C2C2C;
        border-radius: 8px;
        box-shadow:media {
        box-shadow: 0 4px 8px rgba(0,0,0,0.3);
      }

      /* Light Theme */
      .light-theme {
        background-color: #FFFFFF;
        color: #333333;
      }
      .light-theme .title-panel {
        background: linear-gradient(to right, #C8102E, #8B0000);
        color: #FFFFFF;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        padding: 15px;
        text-align: center;
      }
      .light-theme .sidebar {
        background-color: #F5F7FA;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        padding: 15px;
      }
      .light-theme .nav-tabs > li > a {
        background-color: #E6ECF0;
        color: #333333;
        border: none;
        border-radius: 6px;
        margin-right: 5px;
        font-weight: 500;
      }
      .light-theme .nav-tabs > li > a:hover {
        background-color: #C8102E;
        color: #FFFFFF;
      }
      .light-theme .nav-tabs > li.active > a {
        background-color: #8B0000;
        color: #FFFFFF;
      }
      .light-theme .tab-content {
        background-color: #F5F7FA;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      .light-theme .btn {
        background-color: #C8102E;
        color: #FFFFFF;
        border: none;
        border-radius: 6px;
        padding: 8px 16px;
      }
      .light-theme .btn:hover {
        background-color: #8B0000;
        color: #FFFFFF;
      }
      .light-theme .dataTables_wrapper {
        color: #333333;
      }
      .light-theme .table-bordered {
        border: 1px solid #E6ECF0;
      }
      .light-theme .table-bordered th, .table-bordered td {
        border: 1px solid #E6ECF0;
      }
      .light-theme .leaflet-container {
        background: #F5F7FA;
        border-radius: 8px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }

      /* General Styles */
      body {
        font-family: 'Segoe UI', Arial, sans-serif;
      }
      h1 {
        font-size: 28px;
        font-weight: 600;
        margin: 0;
      }
      .theme-toggle {
        position: fixed;
        top: 20px;
        right: 20px;
        z-index: 1000;
      }
    "))
  ),
  titlePanel("Haiti's Potential: Resource-Based Economic Impact"),
  div(class = "theme-toggle",
      actionButton("toggleTheme", "Toggle Theme", icon = icon("adjust"))
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      h3("Wealth Projection"),
      sliderInput("years", "Select Year Range",
                  min = 2025, max = 2045, 
                  value = c(2025, 2035), 
                  step = 1, 
                  sep = ""),
      checkboxGroupInput("resources", "Select Resources",
                         choices = c("Iridium", "Oil", "Gold", "Natural Gas"),
                         selected = c("Iridium", "Oil", "Gold", "Natural Gas")),
      h5("Projected Wealth for Haiti"),
      DTOutput("revenue_table")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Economic Projection", plotlyOutput("wealth_plot", height = "500px")),
        tabPanel("Resource Map", leafletOutput("map", height = "500px")),
        tabPanel("Job & Tax Impact", 
                 plotlyOutput("job_plot", height = "500px"),
                 plotlyOutput("tax_plot", height = "500px"),
                 DTOutput("job_tax_table")),
        tabPanel("UN & Global Oversight",
                 h3("Global Roles & Risks"),
                 p("UN: Oversees peacekeeping, regulates extraction."),
                 p("WHO: Monitors health impacts from mining/drilling."),
                 p("Red Cross: Provides aid for conflicts/disasters."),
                 p("US Coast Guard: Secures maritime routes."),
                 plotlyOutput("risk_plot", height = "500px"),
                 DTOutput("risk_table"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Theme toggle logic
  theme <- reactiveVal("dark-theme")
  observeEvent(input$toggleTheme, {
    current_theme <- theme()
    new_theme <- if (current_theme == "dark-theme") "light-theme" else "dark-theme"
    theme(new_theme)
    runjs(sprintf("document.body.className = '%s';", new_theme))
  })
  
  # Reactive filtered wealth data
  filtered_data <- reactive({
    wealth_projection %>%
      filter(year >= input$years[1],
             year <= input$years[2]) %>%
      select(year,
             if("Iridium" %in% input$resources) "iridium_revenue",
             if("Oil" %in% input$resources) "oil_revenue",
             if("Gold" %in% input$resources) "gold_revenue",
             if("Natural Gas" %in% input$resources) "natural_gas_revenue")
  })
  
  # Reactive filtered job and tax data
  filtered_job_tax_data <- reactive({
    job_tax_projection %>%
      filter(year >= input$years[1],
             year <= input$years[2])
  })
  
  # Reactive filtered risk data
  filtered_risk_data <- reactive({
    risk_projection %>%
      filter(year >= input$years[1],
             year <= input$years[2])
  })
  
  # Render revenue table
  output$revenue_table <- renderDT({
    data <- filtered_data() %>%
      summarise(across(ends_with("_revenue"), sum, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "Resource", values_to = "Revenue") %>%
      mutate(Resource = gsub("_revenue", "", Resource),
             Revenue = paste0("$", formatC(Revenue, format = "f", big.mark = ",", digits = 0)))
    
    datatable(data,
              options = list(dom = 't', ordering = FALSE),
              rownames = FALSE,
              colnames = c("Resource", "Total Projected Revenue"))
  })
  
  # Render wealth plot
  output$wealth_plot <- renderPlotly({
    data_long <- filtered_data() %>%
      pivot_longer(-year, names_to = "resource", values_to = "revenue") %>%
      mutate(resource = gsub("_revenue", "", resource))
    
    p <- ggplot(data_long, aes(x = year, y = revenue, color = resource)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = "Projected Wealth from Resources",
           x = "Year",
           y = "Revenue (USD)",
           color = "Resource Type") +
      theme_minimal() +
      scale_color_manual(values = c("iridium" = "#C8102E",
                                    "oil" = "#1C2526",
                                    "gold" = "#8B0000",
                                    "natural_gas" = "#CAD2C5"))
    
    ggplotly(p)
  })
  
  # Render job plot
  output$job_plot <- renderPlotly({
    data_long <- filtered_job_tax_data() %>%
      pivot_longer(c(haiti_jobs, us_jobs), names_to = "country", values_to = "jobs") %>%
      mutate(country = recode(country, "haiti_jobs" = "Haiti", "us_jobs" = "US"))
    
    p <- ggplot(data_long, aes(x = year, y = jobs, color = sector, linetype = country)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Projected Job Creation by Sector",
           x = "Year",
           y = "Number of Jobs",
           color = "Sector",
           linetype = "Country") +
      theme_minimal() +
      scale_color_manual(values = c("Mining (Iridium)" = "#C8102E",
                                    "Offshore Drilling" = "#1C2526",
                                    "Transportation & Logistics" = "#8B0000",
                                    "Construction" = "#CAD2C5",
                                    "Manufacturing" = "#C8102E",
                                    "Energy Sector" = "#1C2526",
                                    "Engineering & Technology" = "#8B0000",
                                    "Environmental Services" = "#CAD2C5",
                                    "Hospitality & Services" = "#C8102E",
                                    "Education & Training" = "#1C2526"))
    
    ggplotly(p)
  })
  
  # Render tax plot
  output$tax_plot <- renderPlotly({
    data_long <- filtered_job_tax_data() %>%
      pivot_longer(c(haiti_tax, us_tax), names_to = "country", values_to = "tax") %>%
      mutate(country = recode(country, "haiti_tax" = "Haiti", "us_tax" = "US"))
    
    p <- ggplot(data_long, aes(x = year, y = tax, color = sector, linetype = country)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(title = "Projected Tax Revenue by Sector",
           x = "Year",
           y = "Tax Revenue (USD)",
           color = "Sector",
           linetype = "Country") +
      theme_minimal() +
      scale_color_manual(values = c("Mining (Iridium)" = "#C8102E",
                                    "Offshore Drilling" = "#1C2526",
                                    "Transportation & Logistics" = "#8B0000",
                                    "Construction" = "#CAD2C5",
                                    "Manufacturing" = "#C8102E",
                                    "Energy Sector" = "#1C2526",
                                    "Engineering & Technology" = "#8B0000",
                                    "Environmental Services" = "#CAD2C5",
                                    "Hospitality & Services" = "#C8102E",
                                    "Education & Training" = "#1C2526"))
    
    ggplotly(p)
  })
  
  # Render job and tax table
  output$job_tax_table <- renderDT({
    data <- filtered_job_tax_data() %>%
      mutate(
        haiti_tax = paste0("$", formatC(haiti_tax, format = "f", big.mark = ",", digits = 0)),
        us_tax = paste0("$", formatC(us_tax, format = "f", big.mark = ",", digits = 0))
      )
    
    datatable(data,
              options = list(pageLength = 10, autoWidth = TRUE),
              rownames = FALSE,
              colnames = c("Year", "Sector", "Haiti Jobs", "US Jobs", "Haiti Tax", "US Tax"))
  })
  
  # Render risk plot
  output$risk_plot <- renderPlotly({
    p <- ggplot(filtered_risk_data(), aes(x = year, y = risk_level, color = scenario)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      labs(title = "Projected Risk Levels (1-10)",
           x = "Year",
           y = "Risk Level",
           color = "Scenario") +
      theme_minimal() +
      scale_color_manual(values = c("Geopolitical Tension (China/Russia)" = "#C8102E",
                                    "Proxy Conflict (N. Korea/Cuba)" = "#1C2526",
                                    "Environmental Disaster" = "#8B0000",
                                    "Humanitarian Crisis" = "#CAD2C5",
                                    "Maritime Security" = "#C8102E"))
    
    ggplotly(p)
  })
  
  # Render risk table
  output$risk_table <- renderDT({
    data <- data.frame(
      Risk = c("Geopolitical Tension (China/Russia)", "Proxy Conflict (N. Korea/Cuba)", 
               "Environmental Disaster", "Humanitarian Crisis", "Maritime Security"),
      Description = c("China/Russia may oppose US-led extraction, risking sanctions or blockades.",
                      "N. Korea/Cuba could back local resistance, escalating tensions.",
                      "Mining/drift could cause spills or deforestation.",
                      "Displacement or poverty spikes from resource boom.",
                      "Piracy or Houthi-like attacks on shipping routes."),
      Mitigation = c("UN mediation, trade agreements.",
                     "Diplomatic engagement with regional powers.",
                     "Strict environmental regulations, monitoring.",
                     "Red Cross aid, WHO health programs.",
                     "US Coast Guard patrols, UN security mission.")
    )
    datatable(data, options = list(dom = 't', ordering = FALSE), rownames = FALSE)
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet(regions) %>%
      addTiles() %>%
      setView(lng = -73.5, lat = 18.5, zoom = 7) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = ~sqrt(iridium_reserve + oil_reserve/1000000 + gas_reserve/10000000000) / 5,
        color = "#800080",
        fillColor = "#800080",
        fillOpacity = 0.6,
        popup = ~paste(region,
                       "<br>Iridium:", scales::comma(iridium_reserve), "kg",
                       "<br>Oil:", scales::comma(oil_reserve), "barrels",
                       "<br>Gas:", scales::comma(gas_reserve), "cu.ft")
      ) %>%
      addLegend(
        position = "bottomright",
        colors = "#800080",
        labels = "Resource Reserves"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
```
