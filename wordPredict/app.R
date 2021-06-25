library(shiny)
library(readr)
# predictFuzzy <- getPredictFuzzyFunc(readFiles(c("./data/raw/final/en_US/en_US.twitter.txt"), 300))

pathPrefix = "./model/300/"
predictFuzzy <- getPredictFuzzyFuncInner(
    read_csv(paste(pathPrefix, "table2.bz2", sep = "")),
    read_csv(paste(pathPrefix, "table3.bz2", sep = "")),
    read_csv(paste(pathPrefix, "table4.bz2", sep = "")),
    read_csv(paste(pathPrefix, "ct.bz2", sep = ""))
    )

library(promises)
loadModelAsync <- function(pathPrefix) {
    p1 <- promise(function(resolve, reject) {
        l <- list()
        l$table2 <- read_csv(paste(pathPrefix, "table2.bz2", sep = ""))
        resolve(l)
    }) %>%
        then(function(l) {
            l$table3 <- read_csv(paste(pathPrefix, "table3.bz2", sep = ""))
            l
        }) %>%
        then(function(l) {
            l$table4 <-  read_csv(paste(pathPrefix, "table4.bz2", sep = ""))
            l
        }) %>%
        then(function(l) {
            l$ct <- read_csv(paste(pathPrefix, "ct.bz2", sep = ""))
            l
        }) %>%
        then(function(l) {
            getPredictFuzzyFuncInner(l$table2, l$table3, l$table4, l$ct)
        })
    then(p1,
         onFulfilled = function(p) {
             # predictFuzzy <<- p # We already use promises but the UI stills hangs ...
             later::later(function() { predictFuzzy <<- p }, delay = 5)
         },
         onRejected = ~message("Failure")
    )
}
loadModelAsync("./model/9000/")


ui <- fluidPage(

    titlePanel("Word Prediction"),

    sidebarLayout(
        sidebarPanel(
            textAreaInput("input", "Input", "", height = "200px"),
            tableOutput('predict'),
            # https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
            p("1. Use nxt for your predicted next word"),
            p("2. prob is its probability under N-grams and correlation")
        ),

        mainPanel(
           # plotOutput("distPlot"),
           
           # https://shiny.rstudio.com/reference/shiny/1.6.0/textOutput.html
           textOutput("text"),    
           # textOutput("outputForInput")
           tableOutput('table')
           )
    )
)

# https://shiny.rstudio.com/articles/action-buttons.html
server <- function(input, output) {
    # data <- reactiveVal()
    # https://stackoverflow.com/questions/57104314/in-shiny-loading-data-then-preprocessing-to-the-global-environment-then-showi
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # observeEvent(input$button, {
    #    output$text <- renderText({ sample(1) })
    # })
    
    observeEvent(input$input, {
        tokens <- totokens(input$input)
        if (length(tokens) > 0) {
            N <- length(tokens)
            from <- N
            
            res <- NULL
            if (length(tokens) >= 3) {
                predicted <- tokens[(N-2):N]
                res <- predictFuzzy(predicted)
                message("predicted: ", predicted, " res length: ", nrow(res))
            }
            
            if ((is.null(res) || nrow(res) == 0) && length(tokens) >= 2) {
                predicted <- tokens[(N-1):N]
                res <- predictFuzzy(predicted)
                message("predicted: ", predicted, " res length: ", nrow(res))
            } 
            
            if ((is.null(res) || nrow(res) == 0)) {
                predicted <- tokens[N:N]
                res <- predictFuzzy(predicted)
                message("predicted: ", predicted, " res length: ", nrow(res))
            } 
            
            library(pryr)
            message("size of predict: ", object_size(predictFuzzy))
            
            # output$outputForInput <- renderText({ res[1,]$nxt })     
            # message(head(res, 3))
            # https://shiny.rstudio.com/reference/shiny/latest/renderTable.html
            if (nrow(res) > 10) {
                res <- res[1:10,]
            }
            
            predict <- NULL
            if (nrow(res) > 0) {
                predict <- res %>% 
                    select(nxt, dprob) %>%
                    group_by(nxt) %>%
                    summarize(dprob = sum(dprob)) %>%
                    rename(prob = dprob) %>%
                    arrange(desc(prob)) %>%
                    ungroup()
            }
            output$predict <- renderTable(predict, digits = 2)
                
            output$table <- renderTable(res, digits = 5)
        } 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
