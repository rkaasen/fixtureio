





estimation_page_stats_offence_UI <- function(id) {
  
  fluidPage(
    
    tags$style(HTML("
    .gradient-bg-offence {
      background: linear-gradient(to bottom, #da9c8e, #bc4c33); /* Adjust colors as needed */
      height: 100vh; /* Adjust height as needed */
    }
  ")),
    div(class = "gradient-bg-offence",
        
        fluidRow(
          br(),
          column(1),
          column(10,
                 sliderInput(NS(id, "change_offence"), "Change Offence", min = 0, max = 100, value = 25, step = 5),
          ),
          column(1)
        ),
    )
  )
  
  # ~~~~~~~~~~~
}


estimation_page_stats_offence_Server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    
    
    observeEvent(input$change_offence , {
      r6$val_offence <-  input$change_offence
      trigger("change_val_offence")
    })
    
    
  })
}
