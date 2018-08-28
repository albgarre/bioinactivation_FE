
tableFile <- function(input, output, session,
                      label_1 = "time", label_2 = "N") {
    
    ## File part
    
    userFile <- reactive({
        validate(need(input$file, label = "File"))
        input$file
    })
    
    file_frame <- reactive({
        read.table(userFile()$datapath,
                   header = TRUE,
                   sep = input$sep,
                   dec = input$dec,
                   stringsAsFactors = FALSE)
    })
    
    ## Matrix part
    
    input_manual <- reactive({
        out <- input$manual_table
        colnames(out) <- c(label_1, label_2)
        as.data.frame(out)
    })
    
    ## Select the right frame
    
    out_table <- eventReactive(input$update_table, {

        if (input$my_tabBox == "Manual") {
            input_manual()
        } else {
            file_frame()
        }
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    ## Show the table
    
    # output$my_table <- renderTable(out_table())
    output$my_plot <- renderPlot({
        
        p <- ggplot(out_table()) +
            geom_point(aes_string(label_1, label_2))
        
        if (label_2 == "N") {
            p <- p + scale_y_log10()
        }
        
        p
        
    })
    
    ## Export the table
    
    # observeEvent(input$export_table, {
    #     write.table(out_table(), 
    #                 file = "aa.csv", row.names = FALSE,
    #                 sep = "\t")
    # })
    
    output$export_table <- downloadHandler(
        filename = "mytable.csv",
        content = function(file) {
            write.table(out_table(),
                        file = file, row.names = FALSE, sep = "\t")
        }
    )
    
    # Return the reactive that yields the data frame
    
    return(out_table)
    
}































