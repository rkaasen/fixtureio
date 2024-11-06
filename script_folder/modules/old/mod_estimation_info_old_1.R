

source("script_folder/modules/mod_estimation_stats_offence.R")
source("script_folder/modules/mod_estimation_stats_overall.R")
source("script_folder/modules/mod_estimation_stats_defence.R")

estimation_page_info_UI <- function(id) {
  
  fluidPage(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    # ),
    tags$style(HTML("
    .gradient-bg-estimation {
      background: linear-gradient(to bottom, #eef2f5, #6b7073); 
      margin: 0; padding: 0 border: 0;;
    }
  ")),
    div(class = "gradient-bg-estimation",
        
        # home and away team
        fluidRow(
          column(5,align = 'right',
                 h2(textOutput(NS(id,"home_team"))),
                 h4("Home team"),
          ),
          column(2,align = 'center',
                 br(),
                 h3("VS.")
          ),
          column(5,align = 'left',
                 h2(textOutput(NS(id,"away_team"))),
                 h4("Away team")
          )
        ),
        
        # Chance on winning:
        fluidRow( hr(),
                  column(12, align = 'center',
                         h3("Chance of Winning:")
                  ),
                  
        ),
        
        fluidRow(
          column(3 ,align = "right",
                 # div(style = "height: 6px;"),
                 h5(textOutput(NS(id, "perc_winning_home"))),
                 class = "no-right-padding-margin"
          ),
          column(6,align = 'center',
                 div(style = "height: 7px;"),
                 progressBar(NS(id, "pb_winning"), value = 30, size = "sm"),
                 class = "no-left-padding-margin no-right-padding-margin",
                 
                 column(4, align = "center", 
                        numericInput(NS(id, "weight_offence"), label = "Weight of offence (0-10)", value = 5, min = 0, max = 10, width = "50%"),
                        textOutput(NS(id, "perc_offence"))
                 ),
                 column(4, align = "center", 
                        numericInput(NS(id, "weight_overall"), label = "Weight of overall (0-10)", value = 5, min = 0, max = 10, width = "50%"),
                        textOutput(NS(id, "perc_overall"))
                 ),
                 column(4, align = "center", 
                        numericInput(NS(id, "weight_defence"), label = "Weight of defence (0-10)", value = 5, min = 0, max = 10, width = "50%"),
                        textOutput(NS(id, "perc_defence"))
                 ),
                 
          ),
          column(2,align = 'left',
                 # div(style = "height: 0px;"),
                 h5(textOutput(NS(id, "perc_winning_away"))),
                 class = "no-left-padding-margin"
          )
        ),
        
        # subbars (overall, offence, defece, )
        fluidRow( hr(),
                  # OFFENCE
                  column(4, align = "center",
                         h3("OFFENCE", class = "custom-header-offence"),
                         
                         fluidRow(
                           column(4 ,align = "right",
                                  # div(style = "height: 12px;"),
                                  h5(textOutput(NS(id, "perc_offence_home"))),
                                  class = "no-right-padding-margin"
                           ),
                           column(4,align = 'center',
                                  div(style = "height: 7px;"),
                                  progressBar(NS(id, "pb_offence"), value = 40, status = "danger"),
                                  class = "no-right-padding-margin no-left-padding-margin"
                           ),
                           column(3,align = 'left',
                                  # div(style = "height: 12px;"),
                                  h5(textOutput(NS(id, "perc_offence_away"))),
                                  class = "no-left-padding-margin"
                           )
                         ),
                         
                  ),
                  
                  # OVERALL
                  column(4, align = "center",
                         h3("OVERALL", class = "custom-header-overall"),
                         
                         fluidRow(
                           column(4 ,align = "right",
                                  # div(style = "height: 12px;"),
                                  h5(textOutput(NS(id, "perc_overall_home"))),
                                  class = "no-right-padding-margin"
                           ),
                           column(4,align = 'center',
                                  div(style = "height: 7px;"),
                                  progressBar(NS(id, "pb_overall"), value = 20, status = "warning"),
                                  class = "no-right-padding-margin no-left-padding-margin"
                           ),
                           column(3,align = 'left',
                                  # div(style = "height: 11px;"),
                                  h5(textOutput(NS(id, "perc_overall_away"))),
                                  class = "no-left-padding-margin"
                           )
                         )
                         
                  ),
                  
                  # DEFENCE
                  column(4, align = "center",
                         h3("DEFENCE", class = "custom-header-defence"),
                         
                         fluidRow(
                           column(4 ,align = "right",
                                  # div(style = "height: 12px;"),
                                  h5(textOutput(NS(id, "perc_defence_home"))),
                                  class = "no-right-padding-margin"
                           ),
                           column(4,align = 'center',
                                  div(style = "height: 7px;"),
                                  progressBar(NS(id, "pb_defence"), value = 50, status = "success"),
                                  class = "no-right-padding-margin no-left-padding-margin"
                           ),
                           column(3,align = 'left',
                                  # div(style = "height: 11px;"),
                                  h5(textOutput(NS(id, "perc_defence_away"))),
                                  class = "no-left-padding-margin"
                           )
                         ),
                         
                  )
        ),
        
        fluidRow(
          # BUTTONS
          
          column(4, align = "center",
                 actionButton(NS(id,"button_to_offence"), label = "Change Offence")
          ),
          column(4, align = "center",
                 actionButton(NS(id,"button_to_overall"), label = "Change Overall")
          ),
          column(4, align = "center",
                 actionButton(NS(id,"button_to_defence"), label = "Change Defence")
          ),
        ),
        
        
        fluidRow(
          column(12,
                 hr(),
                 br(),
                 # div(style = "height: 40px;")
          ),
          
          tabsetPanel(
            id = NS(id,"tabset_estimation"),
            type = "hidden",
            
            # Offence Tab 
            tabPanel("Offence Tab",
                     fluidRow(
                       column(2),
                       column(8,
                              estimation_page_stats_offence_UI(NS(id,"offence"))
                       ),
                       column(2)
                     )
            ),
            
            # Overall Tab
            tabPanel("Overall Tab",
                     fluidRow(
                       column(2),
                       column(8,
                              estimation_page_stats_overall_UI(NS(id,"overall"))
                       ),
                       column(2)
                     )
            ),
            
            # Defence Tab 
            tabPanel("Defence Tab",
                     fluidRow(
                       column(2),
                       column(8,
                              estimation_page_stats_defence_UI(NS(id,"defence"))
                       ),
                       column(2)
                     )
            )
            
          )
        )
    )
  )
  # ~~~~~~~~~~~
}


estimation_page_info_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(watch("button_to_estimation"), {
      output$home_team <- renderText(r6$selected_home_team)
      output$away_team <- renderText(r6$selected_away_team)
      
    })
    
    observeEvent(watch("change_val_offence") , {
      val_offence = r6$val_offence
      output$perc_offence_home <-  renderText(paste(round(val_offence, 0), "%"))
      output$perc_offence_away <-  renderText(paste(" ", round(100-val_offence, 0), "%"))
      updateProgressBar(id = "pb_offence", value = val_offence)
    })
    
    
    observeEvent(watch("change_val_overall") , {
      val_overall = r6$val_overall
      output$perc_overall_home <-  renderText(paste(round(val_overall, 0), "%"))
      output$perc_overall_away <-  renderText(paste(" ", round(100-val_overall, 0), "%"))
      updateProgressBar(id = "pb_overall", value = val_overall)
    })
    
    observeEvent(watch("change_val_defence") , {
      val_defence = r6$val_defence
      output$perc_defence_home <-  renderText(paste(round(val_defence, 0), "%"))
      output$perc_defence_away <-  renderText(paste(" ", round(100-val_defence, 0), "%"))
      updateProgressBar(id = "pb_defence", value = val_defence)
    })
    
    
    observeEvent(list(input$weight_offence, input$weight_overall, input$weight_defence,
                      watch("change_val_offence"),  watch("change_val_overall"),  watch("change_val_defence")
    ) , {
      
      val_offence = r6$val_offence  
      val_overall = r6$val_overall                  
      val_defence = r6$val_defence  
      
      w_total = input$weight_offence + input$weight_overall + input$weight_defence
      perc_offence = input$weight_offence / w_total * 100
      perc_overall = input$weight_overall / w_total * 100
      perc_defence = input$weight_defence / w_total * 100
      
      output$perc_offence <-  renderText(paste(round(perc_offence, 0), "%"))
      output$perc_overall <-  renderText(paste(round(perc_overall, 0), "%"))
      output$perc_defence <- renderText(paste(round( perc_defence, 0), "%"))
      
      total_winning_chance =  100* (perc_offence*val_offence + perc_overall*val_overall + perc_defence*val_defence) / ((perc_offence+perc_overall+perc_defence)*100)
      
      updateProgressBar(id = "pb_winning", value =total_winning_chance)
      
      output$perc_winning_home <-  renderText(paste(round(total_winning_chance, 0), "%"))
      output$perc_winning_away <-  renderText(paste(" ", round(100-total_winning_chance, 0), "%"))
      
      
    })
    
    
    observeEvent(input$button_to_offence, {
      updateTabsetPanel(session, "tabset_estimation", selected = "Offence Tab")
    })
    observeEvent(input$button_to_overall, {
      updateTabsetPanel(session, "tabset_estimation", selected = "Overall Tab")
    })
    observeEvent(input$button_to_defence, {
      updateTabsetPanel(session, "tabset_estimation", selected = "Defence Tab")
    })
    
    
    
    estimation_page_stats_offence_Server("offence", r6)
    estimation_page_stats_overall_Server("overall", r6)    
    estimation_page_stats_defence_Server("defence", r6)
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}
