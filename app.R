# shiny app for computing skins winners!

library(shiny)
library(rhandsontable)

# address bug in hot_to_r:



new_toR <- function (data, changes, params, ...) {
  #print(changes)
  rClass = params$rClass
  colHeaders = unlist(params$rColHeaders)
  rowHeaders = unlist(params$rRowHeaders)
  rColClasses = unlist(params$rColClasses)[colHeaders]
  out = data
  if (changes$event == "afterCreateRow") {
    rowHeaders = seq_len(length(out))
  }
  else if (changes$event == "afterRemoveRow") {
    inds = seq(changes$ind + 1, length.out = changes$ct)
    rowHeaders = rowHeaders[-inds]
  }
  else if (changes$event == "afterRemoveCol") {
    if (!("matrix" %in% rClass)) {
      inds = seq(changes$ind + 1, 1, length.out = changes$ct)
      rColClasses = rColClasses[-inds]
    }
  }
  if ("matrix" %in% rClass) {
    nr = length(out)
    out = unlist(out, recursive = FALSE)
    out = unlist(lapply(out, function(x) if (is.null(x)) 
      NA
      else x))
    out = matrix(out, nrow = nr, byrow = TRUE)
    class(out) = params$rColClasses
  }
  else if ("data.frame" %in% rClass) {
    nr = length(out)
    out = unlist(out, recursive = FALSE)
    out = unlist(lapply(out, function(x) if (is.null(x)) 
      NA
      else x))
    out = matrix(out, nrow = nr, byrow = TRUE)
    out = rhandsontable:::colClasses(as.data.frame(out, stringsAsFactors = FALSE), 
                     rColClasses, params$columns, ...)
    if (length(rowHeaders) != nrow(out)) {
      rowHeaders <- seq_len(nrow(out))
    }
  }
  else {
    stop("Conversion not implemented: ", rClass)
  }
  if (changes$event == "afterCreateRow") {
    if (!("matrix" %in% rClass)) {
      inds_logical = which(rColClasses == "logical")
      for (i in inds_logical) out[[i]] = ifelse(is.na(out[[i]]), 
                                                FALSE, out[[i]])
    }
  }
  if (ncol(out) != length(colHeaders)) colHeaders = genColHeaders(changes, colHeaders)
  colnames(out) = colHeaders
  rownames(out) = rowHeaders
  if ("data.table" %in% rClass) 
    out = as(out, "data.table")
  out
}

new_hot_to_r <- function(...) {
  do.call(new_toR, ...)
}

# setup defaults

holeNames <- paste('Hole',1:18, sep = " ")

teesStart <- data.frame(Tees = c("White","Blue"),
                        Slope = c(121, 126),
                        Rating = c(70.5, 72.9),
                        stringsAsFactors = FALSE)

indicesStart <- t(data.frame(Index = c(13,3,9,1,15,5,11,7,17,
                                    4,12,16,14,6,10,8,18,2),
                          row.names = holeNames))

scoresStart <- data.frame(Player = c("Player A","Player B"),
                      Index = c(3.2, 12.1),
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
colnames(scoresStart) <- c('Player','Index','Tees',holeNames)

indexStart <- 50
partialStart <- 'Yes'
naturalStart <- 'No'

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
                        post = '%',
                        min = 0,
                        max = 100,
                        value = indexStart),
            radioButtons("partial",
                         'Allocate Partial Strokes?',
                         choices = list('Yes','No'),
                         selected = partialStart),
            radioButtons("natural",
                         'Natural Beats Net?',
                         choices = list('Yes','No'),
                         selected = naturalStart),
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
            downloadButton('save_scores', 'Download Scores'),
            # fileInput('load_scores','Load Scores',
            #           multiple = FALSE,
            #           accept = c(".csv",
            #                      "text/csv",
            #                      "text/comma-separated-values"))
            h4("Instructions"),
            p("Add player name, handicap index (not course handicap), tees, and gross scores.  Make sure tees match
              what is entered for tees exactly matches what has been entered as tee identifier 
              on proper tab, or else players entries will be ignored.  Right-click to add 
              additional rows, or copy-paste from a spreadsheet"),
            br(),
            p("Download scores frequently if entering over a protracted time, as disconnection 
              from the server will result in loss of data.  Scores can most easily be
              uploaded using copy/paste"),
            br(),
            p("Note: plus handicaps not yet supported")
          ),
          tabPanel("Skins Results",
            sidebarLayout(
              mainPanel = mainPanel(
                   actionButton('calc','Calculate Skins'),
                   br(),
                    #tableOutput("nets"),
                    tableOutput("skins"),
                    textOutput("winners")
                   ),
            
              sidebarPanel = sidebarPanel(
                h4('Setup Check:'),
                textOutput("holes_valid"),
                br(),
                textOutput("tees_valid"),
                br(),
                textOutput("ghin_valid"),
                br(),
                textOutput("tees_selected")
              ),
            position = 'right'
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
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  res <- reactiveValues(tees = teesStart, 
                        indices = indicesStart,
                        scores = scoresStart,
                        net_results = NULL, skin_results = NULL,
                        index = indexStart,
                        partial = partialStart,
                        natural = naturalStart,
                        ready = FALSE)
  
  output$holes_valid <- renderText({
    if(!res$ready) return(NULL)
    entered_in <- res$indices[1,]
    if(all(entered_in %in% 1:18)) {
      return("Hole Indices OK")
    } else {
      return("Problem with Hole Indices")
    }
  })
  
  output$tees_valid <- renderText({
    if(!res$ready) return(NULL)
    entered_in <- res$tees
    tees_good <- TRUE
    if (sum(is.na(entered_in)) > 0) tees_good <- FALSE
    
    if (sum(entered_in[,2] <= 100 | entered_in[,2] >= 200) > 0) tees_good <- FALSE
    
    if (sum(entered_in[,3] <= 60 | entered_in[,3] >= 90) > 0) tees_good <- FALSE
    
    if(tees_good) {
      return("Tees Setup OK")
    } else {
      return("Problem with Tees Setup")
    }
  })
  
  output$ghin_valid <- renderText({
    if(!res$ready) return(NULL)
    entered_in <- res$scores$Index
    if(sum(is.na(entered_in)) == 0 & is.numeric(entered_in)) {
      return("Player Handicap Indices OK")
    } else {
      return("Problem with Player Handicap Indices")
    }
  })
  
  output$tees_selected <- renderText({
    if(!res$ready) return(NULL)
    if(all(res$scores$Tees %in% res$tees$Tees)) {
      return("Player Tee Selections OK")
    } else {
      return("Problem with Player Tee Selections")
    }
  })
  
   
   output$tees <- renderRHandsontable({
      if (is.null(input$tees)) {
        DF <- res$tees
      } else {
        DF <- new_hot_to_r(input$tees)
      }
     rhandsontable(DF) %>% hot_col(2, format = '0') %>% 
       hot_col(3, format = '0.0') %>% hot_context_menu()
   })
   
   output$handicaps <- renderRHandsontable({
     if (is.null(input$handicaps)) {
       DF <- res$indices
     } else {
       DF <- new_hot_to_r(input$handicaps)
     }
     rhandsontable(DF) %>% hot_context_menu(allowRowEdit = FALSE)
   })
   
   output$scores <- renderRHandsontable({
     if (is.null(input$scores)) {
       DF <- res$scores
     } else {
       DF <- new_hot_to_r(input$scores)
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
         df <- new_hot_to_r(input$scores)
       }
       write.csv(df, file, row.names = FALSE)
     }
   )
   
   observeEvent(input$calc, {
     if (!is.null(input$tees)) res$tees <- new_hot_to_r(input$tees)
     if (!is.null(input$handicaps)) res$indices <- new_hot_to_r(input$handicaps)
     if (!is.null(input$scores)) res$scores <- new_hot_to_r(input$scores)
     res$index <- input$index
     res$partial <- input$partial
     res$natural <- input$natural
     gross <- res$scores
     tees <- res$tees
     
     indices <- res$indices
     gross <- merge(gross,tees)
     gross$diff_tee_adjust <- round(gross$Rating - min(gross$Rating, na.rm = TRUE))
     gross$CH <- round(gross$Index * (gross$Slope / 113)) + gross$diff_tee_adjust
     gross$a_CH <- gross$CH * res$index / 100
     n_players <- length(gross$a_CH)
     mask <- matrix(nrow = n_players, ncol = 18)
     
     if (input$partial != 'Yes') {
       gross$a_CH <- round(gross$a_CH)
     }
     
     #print(gross)
     for (i in 1:n_players) {
       for (j in 1:18) {
         mask[i,j] <- strokes(indices[,j], gross$a_CH[i],
                              res$partial == 'Yes', 
                              res$natural == 'Yes')
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
       colnames(skins) <- c('Player','Hole','Net Score')
       if (nrow(skins) == 0) {
         skins[1,1] <- 'No Skins'
       }
     } else {
       skins <- NULL
     }
     res$skin_results <- skins
     res$ready <- TRUE
     #print(skins)
   })
   
   observe({
     if (is.null(input$scores)) return(NULL)
     #print(fromJSON(input$scores))
     if (!all(new_hot_to_r(input$scores) == res$scores)) {
       
       res$ready <- FALSE
     }
   })
   
   observe({
     if (is.null(input$tees)) return(NULL)
     if (!all(new_hot_to_r(input$tees) == res$tees)) {
       res$ready <- FALSE
     }
   })
   
   observe({
     if (is.null(input$handicaps)) return(NULL)
     if (!all(new_hot_to_r(input$handicaps) == res$indices)) {
       res$ready <- FALSE
     }
   })
   
   observe({
     if (input$index != res$index) {
       res$ready <- FALSE
     }
   })
   
   observe({
     if (input$partial != res$partial) {
       res$ready <- FALSE
     }
   })
   
   observe({
     if (input$natural != res$natural) {
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
     #print(res$skin_results)
     if (!res$ready) return(NULL)
     res$skin_results
     
   }, digits = 0)
   
   output$winners <- renderText({
     st <- res$skin_results
     if (!res$ready) {
       return("Calculation Needed")
     }
     
     return(paste("A total of ",nrow(st), "skins won"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

