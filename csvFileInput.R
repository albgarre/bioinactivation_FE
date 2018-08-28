
# Module UI function
csvFileInput <- function(id, label = "CSV file") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        fileInput(ns("file"), label),
        radioButtons(ns("sep"), "Separator",
                     c(Comma = ",", Semicolon = ";", Tab = "\t"), "\t"),
        radioButtons(ns("dec"), "Decimal Point",
                     c(Point = ".", Comma = ","), ".")
    )
}

































