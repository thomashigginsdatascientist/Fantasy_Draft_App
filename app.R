
#load libraries ----
library(shiny)
library(tidyverse)
library(DT)
library(rsconnect)
library(shinythemes)

# Define main UI of the application ----
mainui <- fluidPage(
  
  theme = shinytheme("flatly"),

    # Application title
    titlePanel("Drafting For The Masters!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            uiOutput(outputId = "login"),
            br(),
            h5("The draft player button will become available when it is your turn to draft. To draft a player, select their row in the roster table and then click 'Draft This Guy Already!'"),
            br(),
            uiOutput(outputId = "current_drafter"),
            br(),
            uiOutput(outputId = "current_round"),
            br(),
            uiOutput(outputId = "draft_button"),
            br(),
            h4("Current Draft Board:"),
            br(),
            dataTableOutput(outputId = "current_roster"),
            br(),
            h4("Current Draft Picks:"),
            br(),
            dataTableOutput(outputId = "current_picks"),
            br(),
            uiOutput(outputId = "reset_picks"),
            br(),
            uiOutput(outputId = "reset_loop"),
            br(),
            uiOutput(outputId = "reset_players"),
            br(),
            uiOutput(outputId = "reset_rounds"),
            br(),
            uiOutput(outputId = "starter"),
            br(),
            uiOutput(outputId = "number_of_rounds"),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h4("Current Players:"),
          br(),
          br(),
           dataTableOutput(outputId = "Queue")
        )
    )
)

#Define the opening UI of the application, which requires user input to generate the main UI ----
ui <- fluidPage(
  
  uiOutput(outputId = "full_UI")
  
)

# server of application ----
server <- function(input, output, session) {
      
  #initial modal that shows when "logging in" ----
      showModal(
      modalDialog(textInput(inputId = "name", label = "Your Name"),
                  actionButton(inputId = "name_enter", label = "Enter The Draft!"),
                  size = "l", 
                  footer = NULL,
                  easyClose = FALSE)
      )

  
  observeEvent(input$name_enter, {
    
    if(input$name == "Higgy"){
    
    output$full_UI <- renderUI({
      
      mainui
      
    })
    
    output$login <- renderUI({
      h4(paste0("Currently logged in as: ", input$name))
    })
    
    output$starter <- renderUI({
      actionButton(inputId = "start_draft", label = "Start Draft")
    })
    
    output$number_of_rounds <- renderUI({
      numericInput(inputId = "round_numbers", label = "Number of Rounds:", value = 5)
    })
    
    output$reset_picks <- renderUI({
      actionButton(inputId = "reset_picks_b", label = "Reset Draft Picks")
    })
    
    output$reset_loop <- renderUI({
      actionButton(inputId = "reset_loop_b", label = "Reset Loop")
    })
    
    output$reset_players <- renderUI({
      actionButton(inputId = "reset_players_b", label = "Reset Players")
    })
    
    output$reset_rounds <- renderUI({
      actionButton(inputId = "reset_rounds_b", label = "Reset Rounds")
    })
    
    new_guest <- as.data.frame(input$name)
    colnames(new_guest) <- "player"
    
    old_guests <- readRDS("files/players.RDS")
    old_guests <- as.data.frame(old_guests)
    colnames(old_guests) <- "player"
    
    new_guest <- rbind(old_guests, new_guest)
    
    new_guest <- new_guest %>%
      filter(!is.na(player)) %>%
      distinct(player)
    
    new_guest %>%
      drafters()
    
    removeModal()
    
    saveRDS(new_guest, "files/players.RDS")
    
    data <- readRDS("files/roster.RDS")
    
    data %>%
      roster()
    
    }else{
      
      output$full_UI <- renderUI({
        
        mainui
        
      })
      
      output$login <- renderUI({
        h4(paste0("Currently logged in as: ", input$name))
      })
      
      new_guest <- as.data.frame(input$name)
      colnames(new_guest) <- "player"
      
      old_guests <- readRDS("files/players.RDS")
      old_guests <- as.data.frame(old_guests)
      colnames(old_guests) <- "player"
      
      new_guest <- rbind(old_guests, new_guest)
      
      new_guest <- new_guest %>%
        filter(!is.na(player)) %>%
        distinct(player)
      
      new_guest %>%
        drafters()
      
      removeModal()
      
      saveRDS(new_guest, "files/players.RDS")
      
      data <- readRDS("files/roster.RDS")
      
      data %>%
        roster()
      
    }
    
  })
  
  #observe reset button clicks ----
  observeEvent(input$reset_loop_b,{
    
    rv <- data.frame(loop = 0)
    
    saveRDS(rv, "files/loop.RDS")
    
  })
  
  observeEvent(input$reset_picks_b,{
    
    picks <- data.frame(player = NA, draft_pick = NA, round = NA)
    
    saveRDS(picks, "files/picks.RDS")
    
  })
  
  observeEvent(input$reset_players_b,{
    
    picks <- data.frame(player = "Higgy")
    
    saveRDS(picks, "files/players.RDS")
    
  })
  
  
  observeEvent(input$reset_rounds_b,{
    
    round <- data.frame(round = 1)
    
    saveRDS(round, "files/round.RDS")
    
    max_rounds <- data.frame(round = as.numeric(input$round_numbers))
    saveRDS(max_rounds, "files/max rounds.RDS")
    
  })
  
  #When draft is finished, give option to close modal message and view results in the application
  observeEvent(input$restart_draft,{
    
    round <- data.frame(round = 1)
    
    saveRDS(round, "files/round.RDS")
    
    max_rounds <- data.frame(round = 5)
    saveRDS(max_rounds, "files/max rounds.RDS")
    
    rv <- data.frame(loop = 0)
    
    saveRDS(rv, "files/loop.RDS")
    
    removeModal()
    
  })
  
  
  
  
  #loop through current players and only display draft player button when it is that player's turn ----
  
  observe({
    
    invalidateLater(1000)
    
    reader <- readRDS("files/loop.RDS")
    
    rv <- reader$loop
    rv %>%
      rv()
    
  })
  
  observe({
    
    max_rounds <- readRDS("files/max rounds.RDS")
    current_round <- readRDS("files/round.RDS")
    
    max <- max_rounds$round
    current <- current_round$round
    
    if(current == (max+1)){
      
      showModal(
        modalDialog(h3("The draft is complete. Please close the application."),
                    actionButton(inputId = "restart_draft", label = "See Draft Results!"),
                    size = "l",
                    footer = NULL,
                    easyClose = FALSE)
      )
      
    }else{
    
    if(is.null(input$name)){
      num <- 0
    }else{
      num <- rv()
    }
    
    if(num == 0){
      
      print("Need To Press Start Draft!")
      
    }else if(num <= nrow(drafters())){
      
      draftlist <- drafters()
      
      current_player <- draftlist[num,]
      
      output$current_drafter <- renderUI({
        
        h4(paste0("Current Drafter: ", current_player))
        
      })
      
      if(input$name == current_player){
        
        output$draft_button <- renderUI({
          actionButton(inputId = "draft_golfer", label = "Draft This Guy Already!")
        })
        
      }else{
        output$draft_button <- NULL
      }
    
      
    }else{
      
      rv <- 1
      rv %>%
        rv()
      num <- 1
      
      writer <- data.frame(loop = rv)
      
      saveRDS(writer, "files/loop.RDS")
      
      round <- round() + 1
      round %>%
        round()
      
      writer <- data.frame(round = round)
      saveRDS(writer, "files/round.RDS")
      
      draftlist <- drafters()
      
      current_player <- draftlist[num,]
      
      output$draft_button <- renderUI({
        h4(paste0("Current Drafter: ", current_player))
      })
      
      if(input$name == current_player){
        
        output$draft_button <- renderUI({
          actionButton(inputId = "draft_golfer", label = "Draft This Guy Already!")
        })
        
        
      }else{
        output$draft_button <- NULL
      }
      
    }
    
    }
  })
  
  

  #render UI objects for displaying data ----
  output$Queue <- renderDataTable({
    
    data <- drafters()
    
    DT::datatable(data = data)
    
  })
  
  output$current_roster <- renderDataTable({
    
    data <- roster()
    
    DT::datatable(data = data, selection = "single")
    
  })
  
  output$current_picks <- renderDataTable({
    
    data <- draft_picks()
    
    DT::datatable(data = data)
    
  })
  
  output$current_round <- renderUI({
    
    data <- round()
    h4(paste0("Current Round: ", data))
    
  })
  
  #Declare reactive values used to move data around the app ----
  draft_picks <- reactiveVal()
  drafters <- reactiveVal()
  roster <- reactiveVal()
  rv <- reactiveVal()
  round <- reactiveVal()
  
  
  #update current players lobby in "real time" ----
  observe({
    
    invalidateLater(1000)
    
    old_guests <- readRDS("files/players.RDS")
    old_guests <- as.data.frame(old_guests)
    colnames(old_guests) <- "player"
    
    old_guests <- old_guests %>%
      distinct(player)
    
    old_guests %>%
      drafters()
    
    
  })
  
  
  #start draft player button loop with start draft button ----
  #randomize draft order based on who is in player lobby
  #set "official" number of randoms that draft will be
  observeEvent(input$start_draft, {
    
    rv <- rv() + 1
    rv %>%
      rv()
    
    writer <- data.frame(loop = rv)
    
    saveRDS(writer, "files/loop.RDS")
    
    round <- 1
    round %>%
      round()
    
    writer <- data.frame(round = round)
    
    saveRDS(writer, "files/round.RDS")
    
    max_rounds <- data.frame(round = as.numeric(input$round_numbers))
    saveRDS(max_rounds, "files/max rounds.RDS")
    
    old_guests <- readRDS("files/players.RDS")
    old_guests <- as.data.frame(old_guests)
    colnames(old_guests) <- "player"
    
    old_guests <- old_guests %>%
      distinct(player)
    
    old_guests$player <- old_guests[sample(1:nrow(old_guests)),]
    
    old_guests %>%
      drafters()
    
    saveRDS(old_guests, "files/players.RDS")
    
  })
  
  #update current round in "real time" ----
  observe({
    
    invalidateLater(1000)
    
    round <- readRDS("files/round.RDS")
    round %>%
      round()
    
  })
  
  #update current draft picks in "real time" ----
  observe({
    
    invalidateLater(1000)
    
    picks <- readRDS("files/picks.RDS")
    
    picks <- picks %>%
      filter(!is.na(draft_pick))
    
    picks %>%
      draft_picks()
    
  })
  
  
  #update current roster data in "real time" ----
  observe({
    
    invalidateLater(1000)
    
    roster_data <- roster()
    
    if(is.null(roster())){
      print("logging in!")
    }else{
    
    selections <- readRDS("files/picks.RDS")
    
    new_data <- roster_data %>%
      filter(!golfer %in% selections$draft_pick)
    
    new_data %>%
      roster()
    
    }
    
    
    
  })
  
  #fire logic to move to the next player when previous player makes their selection ----
  observeEvent(input$draft_golfer, {
    
    roster_data <- roster()
    round <- round()
    
    row <- input$current_roster_rows_selected
    selected_data <- roster_data[row,]
    
    new_data <- roster_data %>%
      filter(!golfer %in% selected_data$golfer)
    
    pick <- data.frame(input$name)
    colnames(pick) <- "player"
    pick$draft_pick <- selected_data$golfer
    pick$round <- round
    
    selections <- readRDS("files/picks.RDS")
    
    selections <- rbind(selections, pick)
    
    saveRDS(selections, "files/picks.RDS")
    
    selections <- readRDS("files/picks.RDS")
    
    new_data <- new_data %>%
      filter(!golfer %in% selections$draft_pick)
    
    new_data %>%
      roster()
    
    rv <- rv() + 1
    
    rv %>%
      rv()
    writer <- data.frame(loop = rv)
    
    saveRDS(writer, "files/loop.RDS")
    
  })
  
}

# Run the application ---- 
shinyApp(ui = ui, server = server)
