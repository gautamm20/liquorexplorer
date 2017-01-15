library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("liquor.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Liquor Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "BEER"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Output",
      plotOutput("coolplot"),
      br(), br(),
      tableOutput("results")
    ),
    tabPanel("Documentation",
             p(h4("Liquor Explorer:")),
             br(),
             helpText("This application allows one to explore various types of liquors across different countries."),
             HTML("<b>You can filter across various filters:</b>
                  <ul><li>Liquor Type</li>
                  <li>Country</li>
                  <li>Price Range (in USD)</li></ul>
                  <p>Given the above inputs the application filters down results from the backend and displays a histogram of the various alcohol strengths and a detailed table.</p>
                  ")  
              )
             )
        )
    )
)
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$PRODUCT_COUNTRY_ORIGIN_NAME)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(CURRENT_DISPLAY_PRICE >= input$priceInput[1],
             CURRENT_DISPLAY_PRICE <= input$priceInput[2],
             PRODUCT_CLASS_NAME == input$typeInput,
             PRODUCT_COUNTRY_ORIGIN_NAME == input$countryInput
      )
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(PRODUCT_ALCOHOL_PERCENT)) +
      geom_histogram()
  })
  
  output$results <- renderTable({
    filtered()
  })
}

shinyApp(ui = ui, server = server)