# shiny app for computing skins winners!

library(shiny)
library(rhandsontable)

df1 = data.frame(tees = c('White','Blue'), 
                 slope = c(113,113), 
                 rating = c(72, 72))

# Define UI for application that finds skins winners
ui <- fluidPage(
   
   # Application title
   titlePanel("Giant Skins"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        h4("Game Setup"),
        sliderInput("index",
                    "% of Handicap Index",
                    min = 0,
                    max = 1,
                    value = 0.5),
        radioButtons("partial",
                     'Allocate Partial Strokes?',
                     choices = list('Yes','No'),
                     selected = 'Yes'),
        tags$hr(),
        h4("Tees Setup"),
        rHandsontableOutput("tees"),
        tags$hr(),
        h4("Hole Indices"),
        rHandsontableOutput("handicaps")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h4("WUBBALUBBADUBDUB")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$tees <- renderRHandsontable({
      if (is.null(input$tees)) {
        DF = data.frame(Tees = c("White","Blue"),
                        Slope = c(113, 113),
                        Rating = c(72.0, 72.0))
      } else {
        DF = hot_to_r(input$tees)
      }
     rhandsontable(DF) %>% hot_context_menu()
   })
   output$handicaps <- renderRHandsontable({
     if (is.null(input$handicaps)) {
       DF = data.frame(Index = c(1,3,5,7,9,11,13,15,17,
                                 2,4,6,8,10,12,14,16,18))
     } else {
       DF = hot_to_r(input$handicaps)
     }
     rhandsontable(DF) %>% hot_context_menu(allowRowEdit = FALSE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

