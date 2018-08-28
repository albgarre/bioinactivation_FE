
predParsInput <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        selectInput(ns("inactv_model"),
                    "Inactivation model",
                    list(Bigelow = "Bigelow",
                         Peleg = "Peleg",
                         Mafart = "Mafart",
                         Geeraerd = "Geeraerd",
                         Arrhenius = "Arrhenius")
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Bigelow'", ns("inactv_model")),
            wellPanel( 
                numericInput(ns("bigelow_D"), "D-value at ref. temp.",
                             value = 10, min = 0),
                numericInput(ns("bigelow_z"), "z-value",
                             value = 10, min = 0),
                numericInput(ns("bigelow_ref_temp"), "Reference Temperature",
                             value = 70, min = 0),
                numericInput(ns("bigelow_N0"), "N0", value = 1e6, min = 0)
            )
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Peleg'", ns("inactv_model")),
            wellPanel( 
                numericInput(ns("peleg_k"), "k",
                             value = 1, min = 0),
                numericInput(ns("peleg_tempcrit"), "Critical temperature",
                             value = 75, min = 0),
                numericInput(ns("peleg_n"), "n (shape factor)",
                             value = 1.5, min = 0),
                numericInput(ns("peleg_N0"), "N0", value = 1e6, min = 0)
            )
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Mafart'", ns("inactv_model")),
            wellPanel( 
                numericInput(ns("mafart_delta"), "delta-value at ref. temp.",
                             value = 10, min = 0),
                numericInput(ns("mafart_z"), "z-value",
                             value = 10, min = 0),
                numericInput(ns("mafart_ref_temp"), "Reference Temperature",
                             value = 70, min = 0),
                numericInput(ns("mafart_p"), "p (shape factor)",
                             value = 0.9, min = 0),
                numericInput(ns("mafart_N0"), "N0", value = 1e6, min = 0)
            )
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Geeraerd'", ns("inactv_model")),
            wellPanel(
                numericInput(ns("geeraerd_D"), "D-value at ref.temp",
                             value = 5, min = 0),
                numericInput(ns("geeraerd_z"), "z-value",
                             value = 10, min = 0),
                numericInput(ns("geeraerd_ref_temp"), "Reference Temperature",
                             value = 70, min = 0),
                numericInput(ns("geeraerd_Nmin"), "Tail height",
                             value = 5e3, min = 0),
                numericInput(ns("geeraerd_Cc0"), "Initial value of Cc",
                             value = 10, min = 0),
                numericInput(ns("geeraerd_N0"), "N0",
                                value = 1e6, min = 0)
            )
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Arrhenius'", ns("inactv_model")),
            wellPanel(
                numericInput(ns("arrhenius_k"), "Inact. rate at ref. temp.",
                             value = 0.5, min = 0),
                numericInput(ns("arrhenius_Ea"), "Activation energy",
                             value = 5e3, min = 0),
                numericInput(ns("arrhenius_reftemp"), "Ref. temperature",
                             value = 70, min = 0),
                numericInput(ns("arrhenius_N0"), "N0",
                             value = 1e6, min = 0)
            )
        )
    )
}










