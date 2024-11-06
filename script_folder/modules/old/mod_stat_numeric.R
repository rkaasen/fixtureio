




if(F){
  metric_name =  "Goals_Scored_lx"
}


estimation_page_stats_numeric_UI <- function(id, header = "DEFAULT_HEADER", default_on = F) {
  
  style = "background: linear-gradient(to bottom, #f3f6f8, #c1c3c4);"
  
  fluidRow(
    column(10, offset = 1,  style = style,
           column(10, offset = 1,
                  div(style = "height: 0; border-top: 1px dashed black; margin: 5px 0;")
           ),
           
           # header row
           fluidRow(
             column(width = 2, offset = 2, align = "left",
                    h4(style = "font-weight: bold;",header),
                    h6(textOutput(NS(id,"n_matches")))
             ),
             column(width = 3, offset = 0, align = "center",
                    column(width = 5, align = "right",
                           h4(style = "font-weight: bold;", textOutput(NS(id,"rank_ht")))  
                    ),
                    column(width = 2,
                    ),
                    column(width = 5, align = "left",
                           h4(style = "font-weight: bold;", textOutput(NS(id,"rank_at")))   
                    ),
                    
             ),
             column(width = 1, offset =  1, align = "left",
                    br(),
                    column(10, offset = 2,
                    materialSwitch(inputId = NS(id,"include"), label = NULL, status = "primary", value = default_on )
                    )
             ),
             column(width = 2,
             )
           ),
           
           # CONTENT:
           hidden(
             fluidRow(id = NS(id, "content_row"),
                      column(width = 2, offset = 2, align = "left",
                             radioButtons(NS(id,"n_matches"), label = "",
                                          choices = list("Latest 5 Games" = 5, "Latest 10 Games" = 10), 
                                          selected = 5),
                      ),
                      column(width = 3, offset = 0, align = "center",
                             plotOutput(NS(id,"ggplot"), width = "300px", height = "120px")
                      ),
                      column(width = 3, align = "center",
                             br(),
                             sliderTextInput(NS(id,"Slider"),choices = paste0(seq(0,200,1), "%"), label = NULL, selected = "100%", width = "60%")
                             
                      ),
                      column(width = 1,
                      )
                      
             )
           )
           
    ),column(1)
  )
  
  # ~~~~~~~~~~~
}



estimation_page_stats_numeric_Server <- function(id, r6, 
                                                 metric_name =  "NO METIRC",
                                                 positive_metric = T) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$include, {
      if ( input$include ){
        show("content_row")
      } else {
        hide("content_row")
      }
    })
    
    observeEvent(list(input$n_matches,watch("button_to_estimation")),ignoreInit = T, {
      # Make string for header
      output$n_matches = renderText(glue("Latest {input$n_matches} Matches"))
      
      # Take n_matches as numeric
      n_matches = input$n_matches %>% as.numeric()
      
      # calculate all metrics
      metrics_df <- f_metrics_league(r6$data$filtered, n_matches = n_matches)
      
      # filter to metriC and hometeam
      home_team_df  = metrics_df %>% 
        filter(
          Team==r6$selected_home_team,
          metric == metric_name
        )
      output$rank_ht = renderText(home_team_df$rank  %>% toOrdinal())
      
      
      # filter to metriC and awayteam
      away_team_df  = metrics_df %>% 
        filter(
          Team==r6$selected_away_team,
          metric == metric_name
        )
      output$rank_at = renderText(away_team_df$rank %>% toOrdinal() )
      
      metrics_df_filtered <- metrics_df %>%  
        filter(metric == metric_name)
      
      
      if(positive_metric){
      FType = c("Max" = "#1E8449", "Min" = "#EF2E25", "Mean" = "black" )
      } else{
        FType = c("Min" = "#1E8449", "Max" = "#EF2E25", "Mean" = "black" )
      }
      SType = c("Max" = 24, "Min" = 25, "Mean" = 23)
      
      
      # GGPLOT 
      output$ggplot = renderPlot({
        
        ggplot() + 
          # theme_bw() +
          geom_col(data = home_team_df,
                   aes(x = -1, y = value),
                   fill = "#1B4F72", width = 1) + 
          geom_label(data = home_team_df, aes(x = -1, y = max(value * 1.1), label = value)) +
          
          geom_col(data = away_team_df,
                   aes(x = 1, y = value),
                   fill = "#1B4F72", width = 1) + 
          geom_label(data = away_team_df, aes(x = 1, y = max(value * 1.1), label = value)) + 
          
          geom_point(
            aes(x = 0,  y = metrics_df_filtered$value %>% max, shape = "Max",fill = "Max"), size = 4
          ) + 
          geom_point(
            aes(x = 0,  y = metrics_df_filtered$value %>% min, shape = "Min",fill = "Min"), size = 4
          ) + 
          geom_point(
            aes(x = 0,  y = metrics_df_filtered$value %>% mean, shape = "Mean",fill = "Mean"), size = 4
          ) + 
          
          scale_y_continuous(limits = c(0, metrics_df_filtered$value %>% max * 1.2), n.breaks = 4) + 
          scale_fill_manual(name = "League stats", values = FType) +
          scale_shape_manual(name = "League stats", values = SType) +
          theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                axis.title.y.left =  element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill='transparent'),
                plot.background = element_rect(fill='transparent', color=NA),
                # legend.background = element_rect(fill='transparent'),
                # legend.box.background = element_rect(fill='transparent'),
                panel.border = element_blank(),
                panel.grid.major.y = element_line(colour = "black", linetype = "dashed", size = 0.4),
                panel.grid.major.x = element_line(colour = "black", linetype = "dotted", size = 0.1),
                legend.position = c(.9, .6)
          ) + 
          
          scale_x_continuous(limits = c(-2, 3)) 

        
      }, bg="transparent")
      
      
    })
    
    
    
  })
}
