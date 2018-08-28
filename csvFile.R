
# Module server function

csvFile <- function(input, output, session) {

    userFile <- reactive({
        # If no file is selected, don't do anything
        # validate(need(input$file, message = FALSE))
        input$file
    })
    
    # The user's data, parsed into a data frame
    dataframe <- reactive({
        read.table(userFile()$datapath,
                 header = TRUE,
                 sep = input$sep,
                 dec = input$dec,
                 stringsAsFactors = FALSE)
    })
    
    # We can run observers in here if we want to
    observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
    })
    
    # Return the reactive that yields the data frame
    return(dataframe)
}

