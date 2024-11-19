

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI


popup_UI <- function(id) {
  
  
  fluidPage(
    
    # textOutput("popup-check_text")
    # h3("222")
    
  )}


popup_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    
    df_stat_init = r6$stats$stats_init
    
    observe({
      
      df_stat_init <- df_stat_init %>% 
        arrange(order) %>% 
        mutate(`Importance Percent` = paste0(importance_perc, " %"),
               Selected = ifelse(preselect, "Selected", "More Stats")
               ) %>% 
        select(Selected, Select = preselect, Metric = metrics, Importance = importance, `Importance Percent`)
      
      r6$stats$columns <-  df_stat_init %>% names()
      
      output$stat_select_table <- renderReactable({
        reactable(df_stat_init, groupBy = "Selected", defaultExpanded = TRUE,
                  
                  columns = list(
                    Selected    = colDef(name = "", minWidth = 140, align = "left"), 
                    Select      = colDef(cell = checkbox_extra(id = "Select_id", key="Select_id", class = "checkbox-extra"), align = "left"),
                    Metric      = colDef(name = "Metric")
                  ),

                  onClick = JS("function(rowInfo, column) {
    if (window.Shiny) {
      Shiny.setInputValue('popup-stat_table_data', Reactable.getState('popup-stat_select_table'), { priority: 'event' })
      // Reactable.getState('stat_select_table')
    }
  }")
                    
                    )

        
      })
      
    })
    
    
    
 

    
    observeEvent(watch("teams_selected_from_table"), ignoreInit = T, {
      
      # CALCULATIOS FOR MODAL
        output$home_team <- renderText(r6$selected_home_team)
        output$away_team <- renderText(r6$selected_away_team)

      
      # SHOW MODAL
      showModal(modalDialog(
        # title = "Somewhat important message",
        
        fluidRow(
          column(5, align = 'right', 
                 h2(textOutput(NS(id,"home_team"))),
                 h4("Home team"),
          ),
          column(2, align = 'center',
                 h3("VS.")
          ),
          column(5,
                 h2(textOutput(NS(id,"away_team"))),
                 h4("Away team"),
          ),
        ),
        br(), br(), hr(),
        
        fluidRow(
          h4("Stat importance"),
          column(12, align = 'center',
                 reactableOutput(NS(id,"stat_select_table"))
                 )
        ),
        
        
        
        easyClose = TRUE,
        size = "l",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("popup-ok", "OK")
        )
      ))
    })
    
    observeEvent(input$ok, {
      # some logic
      removeModal()
    })
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}










