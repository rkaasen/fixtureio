

authors_UI <- function(id) {
  
  fluidPage(
    fluidRow(
      style = "min-height: 95vh;",
      
      column(width = 6, offset = 3,style = "background-color: #eef2f5;", class = "rounded-column",
             
             h2("Please do reach out!"), 
             br(),
             h5("- If you have FEEDBACK. Positive or Negative."), 
             # br(),
             h5("- If you have ideas and/or want to CONTRIBUTE."), 
             # br(),
             h5("- if you simply want to CHAT about this project, a project you are working on, or something else."), 
             # br(),
             h5("- Or anything something differnet.. :-)"), 
             br(), br(), 
             hr(),
             h2("The author:"),
             br(),
             h4("Rasmus Kaasen"),
             a("Linked", href = "https://www.linkedin.com/in/rasmus-kaasen/", target = "_blank", style = "color: blue; text-decoration: none;"),
             HTML("&nbsp; &nbsp; | &nbsp; &nbsp;"),
             a("Mail", href = "mailto:Kaasen1995@hotmail,com", target = "_blank", style = "color: blue; text-decoration: none;"),
             HTML("&nbsp; &nbsp; | &nbsp; &nbsp;"),
             a("Github", href = "https://github.com/rkaasen/fixtureio", target = "_blank", style = "color: blue; text-decoration: none;"),
             h6("(Github repo currently private.)")
             
             
             
      ) 
      
    )
    
  )
  
}


authors_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    #~~~~~~~~~~~~~~~~~
    # Module Server
  })
}