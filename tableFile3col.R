
tableFile3col <- function(input, output, session,
                          label_1 = "time", label_2 = "logS", label_3 = "temperature") {
    
    ## File part
    
    userFile <- reactive({
        validate(need(input$file, label = "Text"))
        input$file
    })
    
    file_frame <- reactive({
        read.table(userFile()$datapath,
                   header = TRUE,
                   sep = input$sep,
                   dec = input$dec,
                   stringsAsFactors = FALSE)
    })
    
    excelFile <- reactive({
        validate(need(input$excel_file, label = "Excel"))
        input$excel_file
    })
    
    excel_frame <- reactive({
        read_excel(excelFile()$datapath,
                   sheet = input$excel_sheet,
                   skip = input$excel_skip)
    })
    
    ## Matrix part
    
    input_manual <- reactive({
        out <- input$manual_table
        colnames(out) <- c(label_1, label_2, label_3)
        as.data.frame(out)
    })
    
    ## Select the right frame
    
    out_table <- eventReactive(input$update_table, {
        
        if (input$my_tabBox == "Manual") {
            input_manual()
        } else if(input$my_tabBox == "Text") {
            file_frame()
        } else {
            excel_frame()
        }
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

    ## Show the table
    
    # output$my_table <- renderTable(out_table())
    output$my_table <- renderPlot({
        out_table() %>%
            mutate(temperature = factor(temperature)) %>%
            
            ggplot() +
                geom_point(aes(x = time, y = logS, colour = temperature))
    })
    
    ## Export the table
    
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

















