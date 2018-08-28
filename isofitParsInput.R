
isofitParsInput <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        selectInput(ns("inactv_model"),
                    "Inactivation model",
                    list(Bigelow = "Bigelow",
                         Peleg = "Peleg",
                         Mafart = "Mafart",
                         Arrhenius = "Arrhenius"
                         )
        ),
        
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Bigelow'", ns("inactv_model")),
            fluidRow(
                column(6,
                       numericInput(ns("iso_bigelow_D"), "D-value at ref. temp.", 1)
                ),
                column(2,
                       checkboxInput(ns("iso_bigelow_D_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_bigelow_z"), "z-value", 10)
                ),
                column(2,
                       checkboxInput(ns("iso_bigelow_z_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_bigelow_reftemp"), "Ref. temperature", 100)
                ),
                column(2,
                       checkboxInput(ns("iso_bigelow_reftemp_fix"), "Fixed?", TRUE)
                )
            )
            
            ),
        
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Peleg'", ns("inactv_model")),
            fluidRow(
                column(6,
                       numericInput(ns("iso_peleg_k"), "k", 1)
                ),
                column(2,
                       checkboxInput(ns("iso_peleg_k_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_peleg_n"), "n (shape factor)", 1)
                ),
                column(2,
                       checkboxInput(ns("iso_peleg_n_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_peleg_crittemp"), "Critical temperature", 100)
                ),
                column(2,
                       checkboxInput(ns("iso_peleg_critemp_fix"), "Fixed?")
                )
            )
            
        ),
        
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Mafart'", ns("inactv_model")),
            fluidRow(
                column(6,
                       numericInput(ns("iso_mafart_delta"), "dela-value at ref. temp.", 1)
                ),
                column(2,
                       checkboxInput(ns("iso_mafart_delta_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_mafart_z"), "z-value", 10)
                ),
                column(2,
                       checkboxInput(ns("iso_mafart_z_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_mafart_p"), "p (shape factor)", 1)
                ),
                column(2,
                       checkboxInput(ns("iso_mafart_p_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_mafart_reftemp"), "Ref. temperature", 100)
                ),
                column(2,
                       checkboxInput(ns("iso_mafart_reftemp_fix"), "Fixed?", TRUE)
                )
            )
            
        ),
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Arrhenius'", ns("inactv_model")),
            fluidRow(
                column(6,
                       numericInput(ns("iso_arrhenius_k"), "Inact. rate at ref. temp", 0.1)
                ),
                column(2,
                       checkboxInput(ns("iso_arrhenius_k_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_arrhenius_Ea"), "Activation energy", 1e3)
                ),
                column(2,
                       checkboxInput(ns("iso_arrhenius_Ea_fix"), "Fixed?")
                )
            ),
            fluidRow(
                column(6,
                       numericInput(ns("iso_arrhenius_reftemp"), "Ref. temperature", 100)
                ),
                column(2,
                       checkboxInput(ns("iso_arrhenius_reftemp_fix"), "Fixed?", TRUE)
                )
            )
            
        )
        )
}





























