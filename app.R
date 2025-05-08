library(shiny)
library(sf)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(stringr)
library(reactable)
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
                         "wałbrzych do 2002" = "wałbrzych"),
          wage_2001 = wage_2002)

# UI
ui <- fluidPage(
  includeCSS("style.css"),
  shinyWidgets::setBackgroundColor(
    color = c("#70e000", "#ccff33", "#F3FFCC", "white"),
    gradient = "radial",
    direction = c("bottom", "left")
  ),
  titlePanel("Średnie pensje na poziomie Powiatu w latach 2002 - 2023"),
  div(id = "explainer", h4("Ta aplikacja pozwala sprawdzić średnie pensje na poziomie powiatu od 2002 do 2023 roku. Po prawej stronie można wybrać rok dla którego chce się sprawdzić pensje. Poniżej wyświetlą się powiaty z najwyższą i najniższą średnią pensją oraz rozkład średnich pensji w danym roku. W zakładkach można zobaczyć mapę wszystkich powiatów i ich średnie pensje, tabelę porównującą średnie pensje w danym roku oraz zmiany średnich pensji w czasie.")),
  p("\n"),
  sidebarLayout(
    sidebarPanel(
      selectInput("rok", "Wybierz rok:", choices = 2002:2023, selected = 2023),
      girafeOutput("hist", width = "100%", height = "400px"),
      div("Dane dotyczące pensji pobrane z Banku Danych Lokalnych GUS. Dane dotyczące granic geograficznych powiatów pobrane z bazy wiedzy GIS Support", id = "credits")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Mapa powiatów",
        fluidRow(
            p("Poniższa mapa pozwala sprawdzić średnie pensje w każdym powiecie w wybranym roku. Najedź na powiat by zobaczyć jego średnią pensję")
          
        ),
        fluidRow(
            div(class = "map-container",shinycssloaders::withSpinner(uiOutput("inc"), color = "#004b23",  id = "spinner",
            type = 5))
        )
        
        ), 

        tabPanel("Pensje na tle innych powiatów",
        fluidRow(
          p("Poniższa tabela pokazuje płace w danym roku na tle innych powiatów i poprzedniego roku. Centyl oznacza jaki procent powiatów w danym roku ma taką samą lub niższą pensję. Na przykład powiat znajdujący się w 50 centylu ma średnią pensją większą lub równą od 50% powiatów w danym roku.")
        
      ),
      fluidRow(
       shinycssloaders::withSpinner(reactableOutput("powiatTable"), color = "#004b23",
        type = 5)
      )
        ),

        tabPanel("Zmiany pensji w czasie",
        fluidRow(
          p("Poniższa wykres pokazuje zmiany w czasie średnich wypłat we wszystkich powiatach. Najedź na wykres by zobaczyć poszczególne powiaty, wypłaty i trajektorie zmian. Kliknij dwa razy aby wyłączyć zaznaczenie. Możesz również wybrać powiaty, które mają być przedstawione na wykresie."),
          selectInput("region", "Powiat:", choices = unique(d_powiat$region), multiple = TRUE)
      ),
      fluidRow(
        shinycssloaders::withSpinner(plotlyOutput("timeplot"), color = "#004b23",
        type = 5)
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
  
  previous_wage <- reactive({
    prevyear <- as.numeric(input$rok)-1
    paste0("wage_", prevyear)
  })

  d_powiat_filtered <- reactive({
    d_powiat %>%
      mutate(wage = as.numeric(replace_na(.data[[selected_wage()]], 0)),
             previous_wage = as.numeric(replace_na(.data[[previous_wage()]], 0))) # Ensure numeric column
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
    wage <- d_powiat_filtered() %>% pull(wage)
    hist_plot <- d_powiat_filtered() %>%
      ggplot() +
      geom_histogram_interactive(aes(x = wage,
                                     tooltip =  paste0("[",round(..xmin..,2)," zł : ",round(..xmax..,2),"zł] ilość gmin: ",..count..),
                                     group = 1L),
                                 bins = 50, fill = "#70e000", color = "green4") +
      scale_x_continuous(breaks = round(seq(0, max(wage) + 1000, length.out = 5),-1), labels = paste0(round(seq(0, max(wage) + 1000, length.out = 5),-1), "zł")) +
      labs(x = " średnia pensja", y = "ilość gmin", title = glue::glue("rozkład pensji w roku {input$rok}")) +
      theme_minimal() +
      theme(plot.title = element_text(family = "Jost", size = 20),
            axis.title = element_text(family = "Jost", size = 15),
            axis.text =  element_text(family = "Jost", size = 15),
            axis.ticks.x = element_line(color = "black"),
            panel.grid.minor = element_blank(),
            panel.grid.major= element_blank(),
            plot.title.position = "plot")
    girafe(ggobj = hist_plot, bg = "transparent",
           options = list(opts_hover(css = "fill:#283618; stroke:black;"), opts_hover_inv(css = "opacity:0.4;")))
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
      pivot_longer(cols = wage_2002:wage_2023, names_to = "rok", values_to = "pensja") |>
      select(region, rok, pensja) |>
      rename("powiat" = "region") |>
      mutate(rok = as.numeric(str_remove_all(rok, "wage_"))) |>
      highlight_key(~powiat) |>
      ggplot(aes(x = rok, y = pensja, group = powiat)) +
      geom_line(alpha = .1) +
      scale_x_continuous(breaks = 2002:2023) +
      scale_y_continuous(breaks = seq(0, 12000, 1000), labels = paste0(seq(0, 12000, 1000), "zł")) +
      labs(x = "rok", y = "średnia pensja") +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            panel.background = element_blank(),
          text = element_text(family = "Jost"),
          plot.title = element_text(family = "Jost", size = 20),
          axis.title = element_text(family = "Jost", size = 12),
          axis.text =  element_text(family = "Jost", size = 10))

      ggplotly(time_plot) |>
        highlight(on = "plotly_hover", off = "plotly_doubleclick", color = toRGB("darkgreen")) |>
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
  
  output$powiatTable <- renderReactable({

    bar_chart <- function(label, width = "100%", height = "1rem", fill = "#17C448", background = NULL) {
      bar <- div(style = list(background = fill, width = width, height = height, transition = "width 0.8s cubic-bezier(0.42, 0, 0.58, 1)"))
      chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
      div(style = list(display = "flex", alignItems = "center"), label, chart)
    }

    bar_chart_pos_neg <- function(label, value, max_value = 2000, height = "1rem",
                                  pos_fill = "#4361ee", neg_fill = "#780116") {
      neg_chart <- div(style = list(flex = "1 1 0"))
      pos_chart <- div(style = list(flex = "1 1 0"))
      width <- paste0(abs(value / max_value) * 100, "%")

      if (value < 0) {
        bar <- div(style = list(marginLeft = "0.5rem", background = neg_fill, width = width, height = height, transition = "width 0.8s cubic-bezier(0.42, 0, 0.58, 1)"))
        chart <- div(
          style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"),
          label,
          bar
        )
        neg_chart <- tagAppendChild(neg_chart, chart)
      } else {
        bar <- div(style = list(marginRight = "0.5rem", background = pos_fill, width = width, height = height, transition = "width 0.8s cubic-bezier(0.42, 0, 0.58, 1)"))
        chart <- div(style = list(display = "flex", alignItems = "center"), bar, label)
        pos_chart <- tagAppendChild(pos_chart, chart)
      }

      div(style = list(display = "flex"), neg_chart, pos_chart)
    }

    d_totable <- d_powiat_filtered() %>%
      select(region, wage, previous_wage) %>%
      mutate(diff_median = wage - median(wage, na.rm = T),
            percentile = ntile(wage, 100),
            diff_prev = wage - previous_wage,
            percentile = replace_na(percentile, 0),
            diff_prev = replace_na(diff_prev, 0)) %>%
      mutate(across(is.numeric, \(x) round(x, 2))) %>%
      rename("średnia płaca" = "wage", "powiat" = "region") %>%
      select(-previous_wage)

    
    
    
    
    reactable(d_totable,
      defaultColDef = colDef(
        align = "center",  # Center-aligns all columns by default
        headerStyle = list(textAlign = "center"),
        vAlign = "center",
        headerVAlign = "bottom" 
      ),
      columns = list(
        powiat = colDef(name = "powiat",filterable = TRUE,
        # Filter by case-sensitive text match
        filterMethod = JS("function(rows, columnId, filterValue) {
          return rows.filter(function(row) {
            return row.values[columnId].indexOf(filterValue) !== -1
          })
        }")),
        percentile = colDef(name = "Centyl", align = "left", cell = function(value) {
          width <- paste0(value / max(d_totable$percentile) * 100, "%")
          bar_chart(paste0(value, "%"), width = width, background = "#FFFFFF")
        }),
        diff_prev = colDef(
          name = "Zmiana rok do roku",
          defaultSortOrder = "desc",
          cell = function(value) {
            label <- paste0(value, "zł")
            bar_chart_pos_neg(label, value, max_value = max(abs(d_totable$diff_prev), na.rm = T))
          },
          align = "center",
          style = list(fontSize = ".7em"),
          minWidth = 100
        ),
        diff_median = colDef(
          name = "różnica od mediany",
          cell = function(value) {
            color <- if (value >= 0) "#0066CC" else "#CC0000"  # Blue if positive, red if negative
            div_style <- sprintf("color: %s; font-weight: 900;", color)
            div(style = div_style, value)}
        )
      ),
      language = reactableLang(
        searchPlaceholder = "Szukaj",
        noData = "nie znaleziono powiatów",
        pageInfo = "od {rowStart} do {rowEnd} z {rows} powiatów",
        pagePrevious = "\u276e",
        pageNext = "\u276f",
    
        # Accessible labels for assistive technologies such as screen readers.
        # These are already set by default, but don't forget to update them when
        # changing visible text.
        pagePreviousLabel = "Poprzednia strona",
        pageNextLabel = "Następna strona"
      ),
      style = list(fontFamily = "Jost, sans-serif", fontSize = "1.7rem", align = "center"),
      theme = reactableTheme(backgroundColor = "transparent",
                             borderColor = "black",
                             headerStyle = list(fontFamily = "Jost, sans-serif", fontSize = "2rem", textAlign = "center"))
    )



  })
}

shinyApp(ui = ui, server = server)              
