# setwd("/Users/jiahoong/Library/CloudStorage/OneDrive-NottinghamTrentUniversity/_Teach/IDCI_PSYC40725/2023_2024/lectures/TIPI_shiny")

library(shiny)
library(shinyjs)

dim <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
score <- c(NA)
df <- data.frame(dim,score)

# Define UI ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Ten-Item Personality Inventory (TIPI)"),
  sidebarLayout(
    sidebarPanel(
      p("For each statement, please indicate the extent to which 
      you agree or disagree with that statement."),
      p("You should rate the extent to which 
        the pair of traits applies to you, even if one characteristic applies 
        more strongly than the other."),
      br(),
      h4("I see myself as:"),
      br(),
      
      div(
        
        # item 1
        selectInput("item01", h5("Extraverted, enthusiatic"), 
                    choices = list( "Please select" = "", 
                                    "Disagree strongly" = 1, "Disagree moderately" = 2,
                                   "Disagree a little" = 3, "Neither agree nor disagree" = 4,
                                   "Agree a little" = 5, "Agree moderately" = 6,
                                   "Agree strongly" = 7), selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 2 - RS
        selectInput("item02",  h5("Critical, quarrelsome"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 7, "Disagree moderately" = 6,
                                    "Disagree a little" = 5, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 3, "Agree moderately" = 2,
                                    "Agree strongly" = 1), selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 3
        selectInput("item03",  h5("Dependable, self-disciplined"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 1, "Disagree moderately" = 2,
                                    "Disagree a little" = 3, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 5, "Agree moderately" = 6,
                                    "Agree strongly" = 7), selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 4 - RS
        selectInput("item04",  h5("Anxious, easily upset"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 7, "Disagree moderately" = 6,
                                    "Disagree a little" = 5, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 3, "Agree moderately" = 2,
                                    "Agree strongly" = 1),  selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 5
        selectInput("item05",  h5("Open to new experiences, complex"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 1, "Disagree moderately" = 2,
                                    "Disagree a little" = 3, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 5, "Agree moderately" = 6,
                                    "Agree strongly" = 7), selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 6 - RS
        selectInput("item06",   h5("Reserved, quiet"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 7, "Disagree moderately" = 6,
                                    "Disagree a little" = 5, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 3, "Agree moderately" = 2,
                                    "Agree strongly" = 1), selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 7
        selectInput("item07",  h5("Sympathetic, warm"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 1, "Disagree moderately" = 2,
                                    "Disagree a little" = 3, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 5, "Agree moderately" = 6,
                                    "Agree strongly" = 7), selected = NULL,
                    multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 8 - RS
        selectInput("item08",   h5("Disorganised, careless"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 7, "Disagree moderately" = 6,
                                    "Disagree a little" = 5, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 3, "Agree moderately" = 2,
                                    "Agree strongly" = 1),  selected = NULL,
                     multiple = FALSE, selectize = TRUE),
        br(),
        
        # item 9
        selectInput("item09",   h5("Calm, emotionally stable"), 
                     choices = list("Please select" = "", 
                                    "Disagree strongly" = 1, "Disagree moderately" = 2,
                                    "Disagree a little" = 3, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 5, "Agree moderately" = 6,
                                    "Agree strongly" = 7),  selected = NULL,
                     multiple = FALSE, selectize = TRUE),
        br(),

        # item 10 - RS
        selectInput("item10",   h5("Conventional, uncreative"), 
                     choices = list("Please select" = "",
                                    "Disagree strongly" = 7, "Disagree moderately" = 6,
                                    "Disagree a little" = 5, "Neither agree nor disagree" = 4,
                                    "Agree a little" = 3, "Agree moderately" = 2,
                                    "Agree strongly" = 1),  selected = NULL,
                     multiple = FALSE, selectize = TRUE),
        br(),
      ),
      
      actionButton("go", "Enter"),
      actionButton("reset", "Clear"), 
      br(),
      br(),
      p("Ref: Gosling, S. D., Rentfrow, P. J., & Swann, W. B., Jr. (2003). A Very Brief Measure of the Big Five Personality Domains. Journal of Research in Personality, 37, 504-528."),
    ),
    
    mainPanel(
      h4("Here's your results:"),
      plotOutput("plot")
      #textOutput("text"),
     )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  tipi <- reactiveValues(data = NULL)
  tipi$dim <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
  
  #df_out <- reactiveVal(df)
  
  
  observeEvent(input$go, {
    
    #tipi <- df_out()
    
    item01 <- as.integer(input$item01)
    item02 <- as.integer(input$item02)
    item03 <- as.integer(input$item03)
    item04 <- as.integer(input$item04)
    item05 <- as.integer(input$item05)
    item06 <- as.integer(input$item06)
    item07 <- as.integer(input$item07)
    item08 <- as.integer(input$item08)
    item09 <- as.integer(input$item09)
    item10 <- as.integer(input$item10)
    
    open <- sum(item05, item10, na.rm=TRUE)
    cons <- sum(item03, item08, na.rm=TRUE)
    extr <- sum(item01, item06, na.rm=TRUE)
    agre <- sum(item02, item07, na.rm=TRUE)
    neur <- sum(item04, item09, na.rm=TRUE)
  
    tipi$score <- c(open, cons,extr, agre, neur)
    #df_out(tipi) # write data?
    #tipi$sum <- c(open, cons,extr, agre, neur)
    #tipi$dim <- c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism")
  })
  
  
  observeEvent(input$reset, {
    tipi$score <- NULL
    #df_out$score <- NULL
    reset("item01")
    reset("item02")
    reset("item03")
    reset("item04")
    reset("item05")
    reset("item06")
    reset("item07")
    reset("item08")
    reset("item09")
    reset("item10")
  })  
  
  output$plot <- renderPlot({
    #if (is.null(df_out$score)) return()
    if (is.null(tipi$score)) return()
    plot(tipi$score, xaxt = "n", xlab='Factor', ylab='Score', type = "h", 
         col = "gray", lwd = 20)
    axis(1, at=1:5, labels=c("Open", "Consci.", "Extrav", "Agree", "Neuro"))
    #plot(df_out)
  })
  
#  output$text <- renderText({ 
#    print(open$data)
#  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


# runApp("App-1", display.mode = "showcase") # to show the app and the code together
