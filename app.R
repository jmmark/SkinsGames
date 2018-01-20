# shiny app for computing skins winners!

library(shiny)
library(rhandsontable)

# setup defaults

holeNames <- paste('Hole',1:18, sep = ".")

teesStart <- data.frame(Tees = c("White","Blue"),
                        Slope = c(121, 126),
                        Rating = c(70.5, 72.9),
                        stringsAsFactors = FALSE)

indicesStart <- t(data.frame(Index = c(13,3,9,1,15,5,11,7,17,
                                    4,12,16,14,6,10,8,18,2),
                          row.names = holeNames))

scoresStart <- data.frame(Player = c("Player A","Player B"),
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
                      c(3,4),
                      stringsAsFactors = FALSE)
colnames(scoresStart) <- c('Player','GHIN','Tees',holeNames)


strokes <- function(hcp, ch, partial, gross_trump) {
  # calculate the number of strokes
  # no more than double pops allowed
  #print(paste(hcp, ch, partial, gross_trump))
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
  scores[is.na(scores)] <- 99999
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
   #sidebarLayout(
      #sidebarPanel(
        tabsetPanel(
          tabPanel("Game Setup",
            sliderInput("index",
                        "% of Course Handicap",
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
                         selected = 'No'),
            br(),
            br(),
            h4("Instructions"),
            p("Choose the structure of the skins game"),
            br(),
            p("All players receive the given share of their course handicap"),
            br(),
            p("If \'Allocate Partial Strokes\' is selected, players receive 
              only a share of their highest handicap hole.  For example, 
              a player with a course handicap of 9 and 50% strokes would 
              receive 0.5 strokes on the #5 index hole, otherwise it is rounded"),
            br(),
            p("If \'Natural Beats Net\' is selected, net scores cannot tie gross scores")
          ),
          tabPanel("Tees Setup",
            rHandsontableOutput("tees"),
            br(),
            h4("Instructions"),
            br(),
            p("Add slope and rating information for each set of tees
              players in the game will be using.  Course handicaps
              will be calculated accordingly, including adjustment for
              different sets of tees")
            #actionButton('tees_update','Update Tees')
          ),
          tabPanel("Hole Indices",
            rHandsontableOutput("handicaps"),
            br(),
            h4("Instructions"),
            p("Enter the handicap numbers for each of the 18 holes at the course being used")
            #actionButton('indices_update','Update Indices')
          ),
          tabPanel("Player Scores",
            rHandsontableOutput("scores"),
            br(),
            #actionButton('scores_update','Update Scores'),
            downloadButton('save_scores', 'Save Scores'),
            fileInput('load_scores','Load Scores',
                      multiple = FALSE,
                      accept = c(".csv",
                                 "text/csv",
                                 "text/comma-separated-values"))
          ),
          tabPanel("Skins Results",
                   actionButton('calc','Calculate Skins'),
                   br(),
                    #tableOutput("nets"),
                    tableOutput("skins"),
                    textOutput("winners")
                   )
        )
      #)
      
      # Show a plot of the generated distribution
      # mainPanel(
      #   actionButton('calc','Calculate Skins'),
      #    tableOutput("nets"),
      #    tableOutput("skins"),
      #    textOutput("winners")
      # )
   #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  res <- reactiveValues(tees = teesStart, 
                        indices = indicesStart,
                        scores = scoresStart,
                        net_results = NULL, skin_results = NULL,
                        ready = FALSE)
   
   output$tees <- renderRHandsontable({
      if (is.null(input$tees)) {
        DF <- res$tees
      } else {
        DF <- hot_to_r(input$tees)
      }
     rhandsontable(DF) %>% hot_col(2, format = '0') %>% 
       hot_col(3, format = '0.0') %>% hot_context_menu()
   })
   
   output$handicaps <- renderRHandsontable({
     if (is.null(input$handicaps)) {
       DF <- res$indices
     } else {
       DF <- hot_to_r(input$handicaps)
     }
     rhandsontable(DF) %>% hot_context_menu(allowRowEdit = FALSE)
   })
   
   output$scores <- renderRHandsontable({
     if (is.null(input$scores)) {
       DF <- res$scores
     } else {
       DF <- hot_to_r(input$scores)
     }
     rhandsontable(DF) %>% hot_col(2, format = '0.0') %>% 
       hot_col(4:21, format = '0') %>% hot_context_menu(allowColEdit = FALSE)
   })
   
   output$save_scores <- downloadHandler(
     filename = paste0("scores-", Sys.Date(), ".csv"),
     content = function(file) {
       if (is.null(input$scores)) {
         df <- res$scores
       } else {
         df <- hot_to_r(input$scores)
       }
       write.csv(df, file, row.names = FALSE)
     }
   )
   
   observeEvent(input$calc, {
     if (!is.null(input$tees)) res$tees <- hot_to_r(input$tees)
     if (!is.null(input$handicaps)) res$indices <- hot_to_r(input$handicaps)
     if (!is.null(input$scores)) res$scores <- hot_to_r(input$scores)
     gross <- res$scores
     tees <- res$tees
     
     indices <- res$indices
     gross <- merge(gross,tees)
     gross$diff_tee_adjust <- round(gross$Rating - min(gross$Rating, na.rm = TRUE))
     gross$CH <- round(gross$GHIN * (gross$Slope / 113)) + gross$diff_tee_adjust
     gross$a_CH <- gross$CH * input$index
     n_players <- length(gross$a_CH)
     mask <- matrix(nrow = n_players, ncol = 18)
     
     if (input$partial != 'Yes') {
       gross$a_CH <- round(gross$a_CH)
     }
     
     #print(gross)
     for (i in 1:n_players) {
       for (j in 1:18) {
         mask[i,j] <- strokes(indices[,j], gross$a_CH[i],
                              input$partial == 'Yes', 
                              input$natural == 'Yes')
       }
     }
     
     nets <- gross
     nets[,holeNames] <- gross[,holeNames] - mask 
     res$net_results <- nets
     
     
     nr <- res$net_results
     if (!is.null(nr)) {
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
       skins <- NULL
     }
     res$skin_results <- skins
     res$ready <- TRUE
   })
   
   observe({
     if (is.null(input$scores)) return(NULL)
     #print(fromJSON(input$scores))
     if (!identical(hot_to_r(input$scores),res$scores)) {
       
       res$ready <- FALSE
     }
   })
   
   observe({
     if (is.null(input$tees)) return(NULL)
     if (!identical(hot_to_r(input$tees),res$tees)) {
       res$ready <- FALSE
     }
   })
   
   observe({
     if (is.null(input$handicaps)) return(NULL)
     if (!identical(hot_to_r(input$handicaps),res$indices)) {
       res$ready <- FALSE
     }
   })
   
   # update changed tees
   # observeEvent(input$tees_update,{
   #   res$tees <- hot_to_r(input$tees)
   # })
   
   # update changed indices
   # observeEvent(input$indices_update,{
   #   res$indices <- hot_to_r(input$handicaps)
   # })
   
   # update changed scores
   # observeEvent(input$scores_update,{
   #   res$scores <- hot_to_r(input$scores)
   # })
   # 

   
   output$nets <- renderTable({
     if (!res$ready) return(NULL)
     res$net_results
   }, digits = 3)
   
   output$skins <- renderTable({
     if (!res$ready) return(NULL)
     res$skin_results
     
   }, digits = 0)
   
   output$winners <- renderText({
     st <- res$skin_results
     if (!res$ready) {
       return("Game Setup Incomplete")
     }
     
     return(paste("A total of ",nrow(st), "skins won"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

