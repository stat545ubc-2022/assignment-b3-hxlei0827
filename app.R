library(shiny)
library(tidyverse)
library(DT)
library(shinythemes)

bcl <- read_csv("https://raw.githubusercontent.com/daattali/shiny-server/master/bcl/www/bcl-data.csv")

#ggplot(bcl, aes(Alcohol_Content)) + geom_histogram()


#A3_feature1 : adding an image
#A4_feature1: adding a theme selection for the user
#A4_feature2: adding selection of the country for the user
ui <- fluidPage(
  shinythemes::themeSelector(),
  titlePanel("BC Liquer Store Data"),
  h5("Welcome to my shiny app"),
  br(),
  
  img(src = "bclimage.png"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "price", 0, 100, value = c(0, 80), pre = "$"),
                 
      radioButtons("typeInput","Type", choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE")),
      
      selectInput("countryInput", "Country", choices = bcl$Country)
      ),
#A4_feature3: make the main Panel more clear using tabsetPanel
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("alcohol_hist")),
        tabPanel("Data Table", DT::dataTableOutput("data_table"))
                  )
      
      )
  ),
  a(href = "https://github.com/daattali/shiny-server/tree/master/bcl", "CLICK_HERE_For original dataset")
)

# A3_feature 2: add a checkbox by selecting the country "FRANCE"
server <- function(input, output) {
  filtered_data <- 
    reactive(if (is.null(input$countryInput)){
      return(NULL)
      
    }
    else{
      bcl %>% filter(Price > input$priceInput[1] & 
                       Price < input$priceInput[2] & 
                       Type == input$typeInput)%>%
        filter(Country == input$countryInput)
    })
  
  
  output$alcohol_hist <- 
    renderPlot({
      filtered_data() %>% 
        ggplot(aes(Alcohol_Content)) + geom_histogram()
    })
  
# A3_feature 3: "Use the DT package to turn a static table into an interactive table."
  output$data_table <- 
    DT::renderDataTable({
      filtered_data()
    }) 
  
}

shinyApp(ui = ui, server = server)
