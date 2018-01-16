# shiny app for computing skins winners!

library(shiny)
library(rhandsontable)

holeNames <- paste('Hole',1:18)

strokes <- function(hcp, ch, partial, gross_trump) {
  # calculate the number of strokes
  # no more than double pops allowed
  adj <- 0
  if (hcp <= ch) { # need to allocate strokes
    adj <- 1
    if (hcp <= ch - 18) { # need to allocate double pop
      adj <- adj + 1
    } else if (hcp == ceiling(ch - 18) & partial) { # second pop is a partial
      adj <- adj + (ch - floor(ch))
    }
  } else if (hcp == ceiling(ch) & partial) { # first pop is a partial
    adj <- (ch - floor(ch))
  }
  if (adj > 0 & gross_trump) { # adjust down if gross wins
    adj <- adj - .001
  }
  return(adj)
}

find_skins <- function(scores) {
  # if skin exists (one lowest score), return the index, else return null
  if (sum(scores == min(scores)) > 1) {
    return(NULL)
  } else {
    return(which(scores == min(scores)))
  }
}

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
         tableOutput("nets"),
         tableOutput("skins")
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
                         row.names = holeNames))
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
                        c(3,4),
                        c(5,6),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4),
                        c(3,4))
       colnames(DF) <- c('Player','GHIN','Tees',holeNames)
     } else {
       DF <- hot_to_r(input$scores)
     }
     rhandsontable(DF) %>% hot_context_menu(allowColEdit = FALSE)
   })
   
   net_results <- reactive({
     if (!is.null(input$scores))  {
       gross <- hot_to_r(input$scores)
       tees <- hot_to_r(input$tees)
       tees$diff_tee_adjust <- round(tees$Rating - min(tees$Rating))
       indices <- hot_to_r(input$handicaps)
       gross <- merge(gross,tees)
       gross$CH <- round(gross$GHIN * (gross$Slope / 113)) + tees$diff_tee_adjust
       gross$a_CH <- gross$CH * input$index
       n_players <- length(gross$a_CH)
       mask <- matrix(nrow = n_players, ncol = 18)
       
       if (input$partial != 'Yes') {
         gross$a_CH <- round(gross$a_CH)
       }
       
       for (i in 1:n_players) {
         for (j in 1:18) {
           mask[i,j] <- strokes(indices[,j], gross$a_CH[i],
                                input$partial == 'Yes', 
                                input$natural == 'Yes')
         }
       }
       
       nets <- gross
       nets[,holeNames] <- gross[,holeNames] - mask 
       
     } else {
       gross <- data.frame(X = "Incomplete Setup")
       mask <- gross
       nets <- mask
     }
     nets
   })
   
   output$nets <- renderTable({
     
     # if (!is.null(input$scores))  {
     #   gross <- hot_to_r(input$scores)
     #   tees <- hot_to_r(input$tees)
     #   tees$diff_tee_adjust <- round(tees$Rating - min(tees$Rating))
     #   indices <- hot_to_r(input$handicaps)
     #   gross <- merge(gross,tees)
     #   gross$CH <- round(gross$GHIN * (gross$Slope / 113)) + tees$diff_tee_adjust
     #   gross$a_CH <- gross$CH * input$index
     #   n_players <- length(gross$a_CH)
     #   mask <- matrix(nrow = n_players, ncol = 18)
     #   
     #   if (input$partial != 'Yes') {
     #     gross$a_CH <- round(gross$a_CH)
     #   }
     #   
     #   for (i in 1:n_players) {
     #     for (j in 1:18) {
     #       mask[i,j] <- strokes(indices[,j], gross$a_CH[i],
     #                            input$partial == 'Yes', 
     #                            input$natural == 'Yes')
     #     }
     #   }
     #   
     #   nets <- gross
     #   nets[,holeNames] <- gross[,holeNames] - mask 
     #   
     # } else {
     #   gross <- data.frame(X = "Incomplete Setup")
     #   mask <- gross
     #   nets <- mask
     # }
     net_results()
   }, digits = 3)
   
   output$skins <- renderTable({
     # if (ncol(net_results() == 1)) {
     #   "Inputs Required"
     # } else {
     #   "Inputs Required\nInputsRequired"
     # }
     nr <- net_results()
     if (ncol(nr) > 1) {
       skins <- data.frame(Player = character(),
                          Hole = character(),
                          "Net Score" = numeric(),
                          stringsAsFactors = FALSE)
       for (h in holeNames) {
         idx <- find_skins(nr[[h]])
         if (!is.null(idx)) {
           s2 <- data.frame(Player = nr$Player[idx],
                            Hole = h,
                            "Net Score" = nr[[h]][idx],
                            stringsAsFactors = FALSE)
           skins <- rbind(skins, s2)
            
         }
       }
       if (nrow(skins) == 0) {
         skins[1,1] <- 'No Skins'
       }
     } else {
       skins <- data.frame(x = 'Setup Required')
     }
     skins
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

