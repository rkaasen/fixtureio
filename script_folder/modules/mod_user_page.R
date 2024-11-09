# login_module.R

library(shiny)
library(shinyjs)

user_page_UI <- function(id) {
  
  
  
  
  
  
  fluidPage(
    
    # hidden(
    #   div(class = "full-page-spinner", tags$img(src = "spinner.gif"))
    # ),
    
    style = "min-height: 90vh; display: flex; flex-direction: column; justify-content: space-between;", # Main container for full height
    useShinyjs(),
    fluidRow(
      
      style = "display: flex; align-items: flex-start; justify-content: center; padding-top: 20px;",
      # div(style  = "height: 20px;"),
      fluidRow(
        column(
          width = 6, 
          offset = 3,
          style = "background-color: #eef2f5;",
          class = "rounded-column",
          
          # Button to log out
          tagList(
            actionButton(NS(id, "log_out_btn"), "Log out"),
          ),
          
          reactableOutput( NS(id,"all_bets_table")),
          
          plotlyOutput(NS(id,"winnings_ts_plot"))
          
          
        )
      )
      
    )
  )
  
  
  
  
  
}



user_page_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$log_out_btn, ignoreInit =  T, {
      
      r6$user_info$logged_in <- FALSE
      r6$user_info$user_id <- NULL
      r6$user_info$username <- NULL
      r6$user_info$bets <- NULL
      r6$user_info$bets_week_starting <- NULL
      
      trigger("user_logged_out")
      
    })
    
    
    output$all_bets_table <- renderReactable({
      
      f_all_bets_table(r6)
      
    })
    
    
    
    output$winnings_ts_plot <- renderPlotly({
      
      df_plot <-  f_format_all_bets(r6) %>% 
        mutate(
          Match = paste0(HomeTeam, " VS. ", AwayTeam),
          bet_concluded = as.numeric(bet_concluded),
          coloring = ifelse(bet_concluded > 0, "win", "loss")
        )
      
      p <- ggplot(
        data = df_plot,
        aes(x = Date, y = bet_concluded, fill = coloring,
            text = paste0("Game: ", Match, "\n Result: ", winning_team, "\n Your Bet: ", bet))
      ) + 
        geom_bar(
          stat = "identity", color = "black", 
        ) + 
        scale_fill_manual(values = c("win" = "#4CAF50", "loss" = "#e22020"))
      
      ggplotly(p, tooltip = c("text"))
    })
    
  })
}
