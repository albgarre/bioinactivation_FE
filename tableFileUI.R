
tableFileUI <- function(id, inputBoxTitle = "Input", outputBoxTitle = "Output",
                        csvlabel = "CSV file", label_1 = "time", label_2 = "N",
                        default_frame = data.frame(c(0, 10), c(70, 80))) {
    
    ns <- NS(id)
    
    names(default_frame) <- c(label_1, label_2)

    tagList(
        fluidRow(
            
            ## Input
            
            tabBox(title = inputBoxTitle, id = ns("my_tabBox"), side = "right",
                   
                   tabPanel("Manual",
                            matrixInput(inputId = ns("manual_table"),
                                        label = paste(label_1, label_2, sep = " - "),
                                        data = default_frame)
                            ),
                   
                   tabPanel("File",
                            fileInput(ns("file"), csvlabel),
                            radioButtons(ns("sep"), "Separator",
                                         c(Comma = ",", Semicolon = ";", Tab = "\t"), "\t"),
                            radioButtons(ns("dec"), "Decimal Point",
                                         c(Point = ".", Comma = ","), ".")
                            )
                   ),
            
            ## Output
            
            box(title = outputBoxTitle, collapsible = TRUE, status = "primary",
                actionButton(ns("update_table"), "Refresh"),
                # tableOutput(ns("my_table"))
                plotOutput(ns("my_plot")),
                tags$hr(),
                downloadLink(ns("export_table"), "Export")
                # actionLink(ns("export_table"), "Export")
                )
            
        )
        

    )

}





