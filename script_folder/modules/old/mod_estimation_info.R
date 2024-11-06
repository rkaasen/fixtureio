

source("script_folder/modules/mod_stat_numeric_2.R")

estimation_page_info_UI <- function(id) {
  
  fluidPage(

    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css")
    # ),
    
    div(class = "gradient-bg-estimation",
        
        fluidPage(
          # tags$head(
          #   tags$link(rel = "stylesheet", type = "text/css", href = "styles2.css")
          # ),
          
          page_sidebar(
            sidebar = sidebar(
              width = 400,
              class = "my-gradient-background",
              
              # Button to go back
              button_to_selection_UI("button_on_estimation_tab"),
              materialSwitch(inputId = NS(id,"output_switch"), label = "Output Mode", status = "primary"),
              
              
              hr(),
              
              h3("Select Stats:"),
              
              checkboxGroupInput(
                NS(id,"choose_stats"), label = "",
                choices = list("POINTS" = "POINTS", 
                               "GOALS SCORED" = "GOALS SCORED",
                               "GOALS CONCEDED" = "GOALS CONCEDED",
                               "SHOTS ON TARGET" = "SHOTS ON TARGET",
                               "SHOTS ON TARGET AGAINST" = "SHOTS ON TARGET AGAINST"
                               ), 
                selected = c("POINTS", "GOALS SCORED", "GOALS CONCEDED")
              ),
              
              
            ),
            

            fluidRow(
              class = "sticky-fluidrow",
              
              column(10, offset = 1,  
                     style = "background: linear-gradient(to bottom, #eef2f5, #98AFD3);",
                     class = "rounded-column",
                     
                     fluidRow(
                       
                       column(3,align = 'right',
                              column(10, offset = 2, h2(textOutput(NS(id,"home_team")), style = "font-weight: bold;")),
                              column(10, offset = 2, align = "right", 
                                     plotOutput(NS(id,"form_home"), width = "150px", height = "40px")),
                       ), 
                       # LOGO HOME TEAM
                       column(1, align = "center", 
                              div(style = "height: 10px;"),
                              uiOutput(NS(id,"logo_ht"))
                       ),
                       column(3,align = 'center',
                              # br(),
                              h3(textOutput(NS(id,"league"))),
                              h5(textOutput(NS(id,"date"))),
                              h5(textOutput(NS(id,"time"))),
                              plotOutput(NS(id,"ggplot_overall_percent"), width = "80%", height = "75px")
                       ),
                       
                       # LOGO AWAY TEAM
                       column(1, align = "center", 
                              div(style = "height: 10px;"),
                              uiOutput(NS(id,"logo_at"))
                              
                       ),
                       
                       column(3, offset = 0,align = 'left',
                              h2(textOutput(NS(id,"away_team")), style = "font-weight: bold;"),
                              
                              plotOutput(NS(id,"form_away"), width = "150px", height = "40px"),
                              
                       ),
                       # column(1, div(style = "height: 40px;"),
                       #        materialSwitch(inputId = NS(id,"output_switch"), label = "Output Mode", status = "primary")),
                       
                       column(10, offset = 1,
                              div(style = "height: 0; border-top: 2px dashed black; margin: 5px 0;")
                       ),
                       # ~~~~~~~~~~~~~~~~
                       column(width = 2, offset = 1, #br(),
                              column(10, offset=2, div(style = "height: 5px;"),
                                     h4(style = "font-weight: bold;", "Stat:")
                              )
                       ),
                       column(width = 3, offset=1, align = 'center', div(style = "height: 5px;"),
                              column(width = 12, align = "center", h4(style = "font-weight: bold;", "HEAD - TO - HEAD:")),
                       ),
                       column(width = 2, offset = 1, align = "center", 
                              column(11, offset = 1, div(style = "height: 5px;"),
                                     h4(style = "font-weight: bold;", textOutput(NS(id,"weight_or_include")))
                              )
                       ),
                       column(width = 2, br()
                       ),
                       
                     ),
                     column(width = 1)
              )
            ),
            
            # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            # Make the stat rows: ----
            
            estimation_page_stats_numeric_UI(id = NS(id,"POINTS"),
                                             header = "POINTS", default_on = T),
            
            estimation_page_stats_numeric_UI(id = NS(id,"GOALS SCORED"),
                                             header = "GOALS SCORED", default_on = T),
            estimation_page_stats_numeric_UI(id = NS(id,"GOALS CONCEDED"),
                                             header = "GOALS CONDEDED", default_on = T),
            
            
            estimation_page_stats_numeric_UI(id = NS(id,"SHOTS ON TARGET"),
                                             header = "SHOTS ON TARGET", default_on = F),
            estimation_page_stats_numeric_UI(id = NS(id,"SHOTS ON TARGET AGAINST"),
                                             header = "SHOTS ON TARGET AGAINST", default_on = F),

          )
        ) # MAIN PAGE
    )  # SIDEBAR PAGE
    # ~~~~~~~~~~~
  )
}


estimation_page_info_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$output_switch,ignoreInit = F,   {
      if(input$output_switch){
        r6$output_mode <- TRUE
        output$weight_or_include = renderText("Weight:")
      } else{
        r6$output_mode <- FALSE
        output$weight_or_include = renderText("Include stat:")
      }
      trigger("output_mode_trigger")
    })
    
    observeEvent(watch("button_to_estimation"),ignoreInit = T,   {
      updateMaterialSwitch(session, "output_switch", FALSE)
      
      output$league <- renderText(r6$data$league)
      
      date_to_use <- paste0(f_format_date_with_suffix(as.Date(r6$data$match_date)), " ", r6$data$match_time, "h")
      
      output$date <-renderText(date_to_use)
      
      output$home_team <- renderText(r6$selected_home_team)
      output$away_team <- renderText(r6$selected_away_team)
      
      output$home_teamm <- renderText(r6$selected_home_team)
      output$away_teamm <- renderText(r6$selected_away_team)
      
      output$form_home <-  f_form_plot(r6$data$filtered, r6$selected_home_team, r6$data$match_date)
      output$form_away <-  f_form_plot(r6$data$filtered, r6$selected_away_team, r6$data$match_date)
      
      output$logo_ht <- renderUI({
        img(src = paste0("Logos/", r6$selected_home_team, ".png"), height = "100%", width = "100%")
      })
      output$logo_at <- renderUI({
        img(src = paste0("Logos/", r6$selected_away_team, ".png"), height = "100%", width = "100%")
      })
      
      
    })
    
    
    
    estimation_page_stats_numeric_Server("POINTS", r6, metric_name = "Points_lx", positive_metric = T)
    
    estimation_page_stats_numeric_Server("GOALS SCORED", r6, metric_name = "Goals_Scored_lx", positive_metric = T)
    estimation_page_stats_numeric_Server("GOALS CONCEDED", r6, metric_name = "Goals_Conceded_lx", positive_metric = F)
    
    estimation_page_stats_numeric_Server("SHOTS ON TARGET", r6, metric_name = "Shots_OT_For_lx", positive_metric = T)
    estimation_page_stats_numeric_Server("SHOTS ON TARGET AGAINST", r6, metric_name = "Shots_OT_Against_lx", 
                                         positive_metric = F, header = "SHOTS ON TARGET \n AGAINST")
    
    
    
    observeEvent(watch("update_main_prediction"), ignoreInit = T, {
      metrics <- r6$metrics
      
      # Calculate overall percentages
      overall_percentages <-
        f_calculate_overall_percentages(metrics,
                                        Home_label = r6$selected_home_team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short),
                                        Away_label = r6$selected_away_team %>% as_tibble() %>% setNames("Team") %>% left_join(r6$team_list$league, by = c("Team")) %>% pull(Team_short)
        )
      
      output$ggplot_overall_percent = renderPlot({
        overall_percentages
      }, bg="transparent")
      
      
    })
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}
