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
        tabsetPanel(
          tabPanel("Game Setup",
            sliderInput("index",
                        "% of Handicap Index",
                        min = 0,
                        max = 1,
                        value = 0.5),
            radioButtons("partial",
                         'Allocate Partial Strokes?',
                         choices = list('Yes','No'),
                         selected = 'Yes'),
            radioButtons("natural",
                         'Natural Beats Net?',
                         choices = list('Yes','No'),
                         selected = 'No')
          ),
          tabPanel("Tees Setup",
            rHandsontableOutput("tees")
          ),
          tabPanel("Hole Indices",
            rHandsontableOutput("handicaps")
          ),
          tabPanel("Player Scores",
            rHandsontableOutput("scores")
          )
        )
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
        DF <- data.frame(Tees = c("White","Blue"),
                        Slope = c(113, 113),
                        Rating = c(72.0, 72.0))
      } else {
        DF <- hot_to_r(input$tees)
      }
     rhandsontable(DF) %>% hot_context_menu()
   })
   
   output$handicaps <- renderRHandsontable({
     if (is.null(input$handicaps)) {
       DF <- t(data.frame(Index = c(1,3,5,7,9,11,13,15,17,
                                 2,4,6,8,10,12,14,16,18),
                         row.names = 1:18))
     } else {
       DF <- hot_to_r(input$handicaps)
     }
     rhandsontable(DF) %>% hot_context_menu(allowRowEdit = FALSE)
   })
   
   output$scores <- renderRHandsontable({
     if (is.null(input$scores)) {
       DF <- data.frame(Player = c("Player A","Player B"),
                        GHIN = c(3.2, 12.1),
                        Tees = c('Blue','White'),
                        "1" = c(3,4),
                        "2" = c(5,6),
                        "3" = c(3,4),
                        "4" = c(3,4),
                        "5" = c(3,4),
                        "6" = c(3,4),
                        "7" = c(3,4),
                        "8" = c(3,4),
                        "9" = c(3,4),
                        "10" = c(3,4),
                        "11" = c(3,4),
                        "12" = c(3,4),
                        "13" = c(3,4),
                        "14" = c(3,4),
                        "15" = c(3,4),
                        "16" = c(3,4),
                        "17" = c(3,4),
                        "18" = c(3,4))
     } else {
       DF <- hot_to_r(input$scores)
     }
     rhandsontable(DF) %>% hot_context_menu(allowColEdit = FALSE)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

