# shiny app for computing skins winners!

library(shiny)
library(rhandsontable)
a

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
         rHandsontableOutput("nets")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$tees <- renderRHandsontable({
      if (is.null(input$tees)) {
        DF <- data.frame(Tees = c("White","Blue"),
                        Slope = c(121, 126),
                        Rating = c(70.5, 72.9))
      } else {
        DF <- hot_to_r(input$tees)
      }
     rhandsontable(DF) %>% hot_context_menu()
   })
   
   output$handicaps <- renderRHandsontable({
     if (is.null(input$handicaps)) {
       DF <- t(data.frame(Index = c(13,3,9,1,15,5,11,7,17,
                                 4,12,16,14,6,10,8,18,2),
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
   
   output$nets <- renderRHandsontable({
     
     if (!is.null(input$scores))  {gross <- hot_to_r(input$scores)
       tees <- hot_to_r(input$tees)
       tees$diff_tee_adjust <- round(tees$Rating - min(tees$Rating))
       indices <- hot_to_r(input$handicaps)
       gross <- merge(gross,tees)
       gross$CH <- round(gross$GHIN * (gross$Slope / 113)) + tees$diff_tee_adjust
       gross$a_CH <- gross$CH * input$index
       if (input$partial != 'Yes') {
         gross$a_CH <- round(gross$a_CH)
       }
     } else {
       gross <- data.frame(X = c(1,2))
     }
     rhandsontable(gross)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

