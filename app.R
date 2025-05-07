library(shiny)
library(sf)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(stringr)
library(gt)
library(gtExtras)
library(tidyr)
library(htmltools)
library(shinydashboard)
library(plotly)

# Load spatial data
powiaty <- read_sf("data/powiaty.shp") %>%
  mutate(JPT_NAZWA_ = tolower(str_remove(trimws(JPT_NAZWA_), "powiat")))

# Load wages data
d <- readxl::read_excel("data/dane_gus_powiat.xlsx", sheet = 2, skip = 1) %>%
  slice(-1) %>%
  rename("code" = `...1`, "region" = `...2`) %>%
  mutate(across(`2002`:`2023`, as.numeric, .names = "wage_{.col}")) 

# Filter for powiat-level data and clean names
d_powiat <- d %>%
  filter(str_detect(region, "Powiat")) %>%
  mutate(region = tolower(str_remove_all(region, "powiat| m\\. st\\.| m\\.")) %>%
           trimws()) %>%
  mutate(region = recode(region, "wałbrzych od 2013" = "wałbrzych",
                         "wałbrzych do 2002" = "wałbrzych"))

# UI
ui <- fluidPage(
  includeCSS("style.css"),
  shinyWidgets::setBackgroundColor(
    color = c("#70e000", "#ccff33", "#F3FFCC", "white"),
    gradient = "radial",
    direction = c("bottom", "left")
  ),
  titlePanel("Średnie pensje na poziomie Powiatu"),
  
  sidebarLayout(
    sidebarPanel(
      "Ta aplikacja pozwala sprawdzić średnie pensje na poziomie powiatów od 2002 do 2023 roku.",
      selectInput("rok", "Rok:", choices = 2002:2023, selected = 2023),
      textOutput("summary"),
      girafeOutput("hist", width = "100%", height = "400px")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
        fluidRow(
            h4("Mapa"),
            p("Poniższa mapa pozwala sprawdzić średnie pensje w każdym powiecie w wybranym roku. Najedź na powiat by zobaczyć jego średnią pensję")
          
        ),
        fluidRow(
            div(class = "map-container",shinycssloaders::withSpinner(uiOutput("inc"), color = "#004b23",  id = "spinner",
            type = 5))
        )
        
        ), 

        tabPanel("Table",
        fluidRow(
          h4("Tabela"),
          p("Poniższa tabela coś pokazuje, ale co to kto w sumie wie")
        
      ),
      fluidRow(
        gt_output("powiatTable")
      )
        ),

        tabPanel("Time",
        fluidRow(
          h4("Zmiany w czasie"),
          p("Poniższa wykres pokazuje zmiany w czasie średnich wypłat we wszystkich powiatach. Najedź na wykres by zobaczyć poszczególne powiaty, wypłaty i trajektorie zmian. Możesz również wybrać powiaty, które mają być przedstawione na wykresie."),
          selectInput("region", "Powiat:", choices = unique(d_powiat$region), multiple = TRUE)
      ),
      fluidRow(
        plotlyOutput("timeplot")
      )
        )
      ))
      
    
  )
)

# Server
server <- function(input, output, session) {
  selected_wage <- reactive({
    paste0("wage_", input$rok)
  })
  
  d_powiat_filtered <- reactive({
    d_powiat %>%
      mutate(wage = as.numeric(replace_na(.data[[selected_wage()]], 0))) # Ensure numeric column
  })

  selected_region <- reactive({
    if(!is.null(input$region)) {
      d_powiat %>%
      filter(region %in% input$region)
    } else {
      d_powiat
    }
    
  })
  
  output$hist <- renderGirafe({
    hist_plot <- d_powiat_filtered() %>%
      ggplot() +
      geom_histogram_interactive(aes(x = wage,tooltip =  paste0("[",round(..xmin..,2)," zł : ",round(..xmax..,2),"zł] ilość gmin: ",..count..), group = 1L), bins = 50, fill = "#70e000", color = "green4") +
      labs(x = "pensja", y = "ilość gmin", title = glue::glue("rozkład pensji w roku {input$rok}")) +
      theme_minimal()
    girafe(ggobj = hist_plot, bg = "transparent",
           options = list(opts_hover(css = "fill:#283618; stroke:black;"), opts_hover_inv(css = "opacity:0.4;")))
  })
  
  
  output$summary <- renderText({
    data <- d_powiat_filtered()
    
    max_powiat <- data %>% filter(!is.na(wage)) %>% slice_max(wage, n = 1)
    min_powiat <- data %>% filter(!is.na(wage)) %>% slice_min(wage, n = 1)
    
    glue::glue(
      "Powiat z najwyższą pensją: {max_powiat$region} z pensją {max_powiat$wage} zł\n
             Powiat z najmniejszą pensją: {min_powiat$region} z pensją {min_powiat$wage} zł\n"
    )
  })
  
  output$inc <- renderUI({
      tags$iframe(
      src = paste0("./maps/", selected_wage(), ".html"),
      width = "100%", height = "100%",frameborder = "0",
      scrolling = "no", style = "border:none; overflow:hidden;"
    )
  })

  output$timeplot <- renderPlotly({

    time_plot <- selected_region() |>
      pivot_longer(cols = wage_2002:wage_2023, names_to = "year", values_to = "wage") |>
      select(region, year, wage) |>
      mutate(year = as.numeric(str_remove_all(year, "wage_"))) |>
      highlight_key(~region) |>
      ggplot(aes(x = year, y = wage, group = region)) +
      geom_line(alpha = .1) +
      scale_x_continuous(breaks = 2002:2023) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            panel.background = element_blank(),
          text = element_text(family = "Jost"))

      ggplotly(time_plot) |>
        highlight(on = "plotly_hover", color = toRGB("darkgreen")) |>
          layout(
            paper_bgcolor = 'rgba(0,0,0,0)',  # Transparent paper background
            plot_bgcolor = 'rgba(0,0,0,0)',   # Transparent plot background
            xaxis = list(showgrid = FALSE),   # Remove x-axis grid
            yaxis = list(showgrid = FALSE)    # Remove y-axis grid
          )
        
  })
  
  # output$mapPlot <- renderGirafe({
  #   d_powiat_full <- powiaty %>%
  #     full_join(d_powiat_filtered(), by = c("JPT_NAZWA_" = "region"))
  #   
  #   gg <- ggplot(d_powiat_full) +
  #     geom_sf_interactive(aes(fill = wage, tooltip = paste0(JPT_NAZWA_, ": ", wage), data_id = JPT_NAZWA_)) +
  #     scale_fill_gradient(low = "#ccff33", high = "#004b23", na.value = "grey80") +  # Handle missing values
  #     labs(fill = "średnia pensja") +
  #     theme_void() +
  #     theme(legend.position = "top")
  #   
  #   girafe(ggobj = gg, bg = "transparent",
  #          options = list(opts_hover(css = "fill:#283618;stroke:black;"), opts_hover_inv(css = "opacity:0.4;")))
  # })
  
  output$powiatTable <- render_gt({
    d_powiat_filtered() %>%
      select(region, wage) %>%
      group_by(region) %>%
      summarise(
        `minimalna pensja` = paste(min(wage, na.rm = TRUE), "zł"),
        `średnia pensja` = paste(round(mean(wage, na.rm = TRUE), 2), "zł"),
        `najwyższa pensja` = paste(max(wage, na.rm = TRUE), "zł"),
        .groups = "drop"
      ) %>%
      arrange(region) %>%
      gt() %>%
      tab_options(table.background.color = "#FFFFFF00") %>%
      opt_table_font(font = google_font("Space Grotesk"), color = "black", weight = "bold") %>%
      opt_interactive(use_sorting = FALSE, use_filters = TRUE, use_compact_mode = TRUE)
  })
}

shinyApp(ui = ui, server = server)              
