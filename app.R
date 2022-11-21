library(shiny)
library(tidyverse)
library(DT)

bcl <- read_csv("https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/www/bcl-data.csv")

#ggplot(bcl, aes(Alcohol_Content)) + geom_histogram()


#feature1 : adding an image
ui <- fluidPage(
  titlePanel("BC Liquer Store Data"),
  h5("Welcome to my shiny app"),
  br(),
  
  img(src = "bclimage.png"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "price", 0, 100, value = c(25, 40), pre = "$"),
                 
                 radioButtons("typeInput","Type", choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      checkboxInput("France", "Only From France ", FALSE)),
    mainPanel(
      plotOutput("alcohol_hist"), 
      DT::dataTableOutput("data_table"))
  ),
  a(href = "https://github.com/daattali/shiny-server/tree/master/bcl", "CLICK_HERE_For original dataset")
)

# feature 2: add a checkbox by selecting the country "FRANCE"
server <- function(input, output) {
  filtered_data <- 
    reactive(if (input$France){
      bcl %>% filter(Price > input$priceInput[1] & 
                       Price < input$priceInput[2] & 
                       Type == input$typeInput)%>%
              filter(Country == "FRANCE")
      
    }
    else{bcl %>% filter(Price > input$priceInput[1] & 
                          Price < input$priceInput[2] & 
                          Type == input$typeInput)

    })
  
  
  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Alcohol_Content)) + geom_histogram()
    })
  
# feature 3: "Use the DT package to turn a static table into an interactive table."
  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    }) 
  
}

shinyApp(ui = ui, server = server)
