#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(stringr)


powiaty <- read_sf("data/powiaty.shp")
d <- readxl::read_excel("data/dane_gus_powiat.xlsx", sheet = 2, skip = 1)
d <- d[-1,]
d <- d %>%
    rename("code" = `...1`,
           "region" = `...2`)

d <- d %>%
    mutate(across(`2002`:`2023`, .names = "wage_{.col}"))

d_powiat <- d %>%
    filter(grepl("Powiat.*", region))



#yay this works!
#now All we need is to link powiaty to data on wages and
#then add information with some interactive thing like plotly etc.!

powiaty$JPT_NAZWA_ <- tolower(powiaty$JPT_NAZWA_)
d_powiat$region <- tolower(d_powiat$region)
d_powiat$region <- str_remove(d_powiat$region , " m\\. st\\.")

d_powiat$region <- str_remove(d_powiat$region , " m\\.")
d_powiat$region <- str_remove(d_powiat$region , "powiat")
powiaty$JPT_NAZWA_ <- str_remove(powiaty$JPT_NAZWA_, "powiat")

d_powiat$region <- trimws(d_powiat$region)
powiaty$JPT_NAZWA_ <- trimws(powiaty$JPT_NAZWA_)

d_powiat <- d_powiat %>%
    mutate(region = recode(region,
                           "wałbrzych od 2013" = "wałbrzych"))

# making the join




# Define UI for application that draws a histogram
ui <- fluidPage(
    includeCSS("style.css"),
    shinyWidgets::setBackgroundColor(
        color = c("#70e000","#ccff33","#F3FFCC","white"),
        gradient = c("radial"),
        direction = c("bottom", "left"),
        shinydashboard = FALSE
    ),
    # Application title
    titlePanel("Średnie pensje na poziomie Powiatu"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            "Ta aplikacja pozwala sprawdzić średnie pensje na poziomie powiatów od 2002 do 2023 roku.",
            selectInput("rok",
                        "Rok:",
                        choices = seq(2002, 2023, 1),
                        selected = 2023
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            shinycssloaders::withSpinner(
                girafeOutput("mapPlot"),
                color = "#004b23"
                )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    filtered <- reactive(paste0("wage_", input$rok))
    
    
    output$mapPlot <- renderGirafe({
        
        d_powiat$wage <- as.numeric(unlist(d_powiat[, filtered()]))
        
        d_powiat_full <- powiaty %>%
            full_join(d_powiat, by = c("JPT_NAZWA_" = "region"),
                      relationship = "many-to-many")
        
        map_final_interactive <- ggplot(d_powiat_full) +
            geom_sf_interactive(aes(fill = wage,
                                    tooltip = paste0(JPT_NAZWA_,": ",wage),
                                    data_id = JPT_NAZWA_),
                                ) +
            scale_fill_gradient(low ="#ccff33", high = "#004b23",
                                breaks = c(0, 
                                           round(mean(d_powiat_full$wage, na.rm = T), -3),
                                           round(max(d_powiat_full$wage, na.rm = T) - 500, -3))
                                ) +
            labs(fill = "średnia pensja") +
            theme_void() +
            theme(legend.position = "top",
                  panel.background = element_rect(fill = NA, color = NA),
                  plot.background = element_rect(fill = NA, color = NA))
        
        
        girafe(ggobj = map_final_interactive, 
               bg = "transparent",
               options = list(
                   opts_hover(css = girafe_css(css = "fill:#283618;stroke:black;")),
                   opts_hover_inv(css = "opacity:0.4;")
               ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
