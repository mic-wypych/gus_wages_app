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
      shinycssloaders::withSpinner(uiOutput("inc", fill = TRUE), color = "#004b23"),
      gt_output("powiatTable"),
      
    )
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
      "Powiat z najwyższą pensją: {max_powiat$region} z pensją {max_powiat$wage} zł <br />
             Powiat z najmniejszą pensją: {min_powiat$region} z pensją {min_powiat$wage} zł"
    )
  })
  
  output$inc <- renderUI({
    tags$iframe(
      src = paste0("./maps/", selected_wage(), ".html"),
      width = "90%", height = "500px", style = "border:none;"
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
