library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("SearcHit"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      numericInput("val", 
                   "Valence", 
                   value = 0.5),
      
      numericInput("temp", 
                   "Tempo", 
                   value = 150),
      
      numericInput("danc", 
                   "Danceability", 
                   value = 0.5),
      
      numericInput("energy", 
                   "Energy", 
                   value = 0.5),
    
      numericInput("inst", 
                   "Instrumentalness", 
                   value = 0.5),
      
      numericInput("dur", 
                   "Duration (in ms)", 
                   value = 250000),
      
      selectInput("select", "Genre", 
                  choices = list("Rock" = "rock", "Pop" = "pop",
                                 "Hip Hop" = "hip hop", "R&B" = "R&B", "Metal" = "metal", "Country" = "country", "Dance/Electronic" = "Dance/Electronic", "Easy Listening" = "easy listening", "Latin" = "latin",  "World/Traditional" = "World/Traditional", "Set()" = "set()"), selected = 1),
      width = 25),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      verbatimTextOutput("prediction"),
      
      renderText({input$val}),
      renderText({input$temp}),
      renderText({input$danc}),
      renderText({input$energy}),
      renderText({input$inst}),
      renderText({input$dur}),
      renderText({input$select}),
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot")
      
    )
  )
)


server <- function(input, output) {
  
  output$prediction <- renderPrint({
    
    ss <- readr::read_csv('spotify_songs.csv')
    ss <- na.omit(ss)
    full_model <- lm(popularity~duration_ms+tempo+danceability+valence+energy+instrumentalness+genre, data=ss)
    
    newsong <- data.frame(duration_ms=c({input$dur}),tempo=c({input$temp}),danceability=c({input$danc}),valence=c({input$val}),energy=c({input$energy}),instrumentalness=c({input$inst}), genre=c({input$select}))
    predict(full_model, newsong)
    predict(full_model, newsong, interval = "prediction", level = 0.05)
  })
  
 
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
 
  
}

shinyApp(ui = ui, server = server)
