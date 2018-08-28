
isofitPars <- function(input, output, session, withN0 = TRUE) {
   
    out <- reactive({
        
        guess <- numeric()
        known <- numeric()
        
        if (input$inactv_model == "Bigelow") {
            
            model <- "Bigelow"

            if (input$iso_bigelow_D_fix) {
                known <- c(known, D_R = input$iso_bigelow_D)
            } else {
                guess <- c(guess, D_R = input$iso_bigelow_D)
            }
            
            if (input$iso_bigelow_z_fix) {
                known <- c(known, z = input$iso_bigelow_z)
            } else {
                guess <- c(guess, z = input$iso_bigelow_z)
            }
            
            if (input$iso_bigelow_reftemp_fix) {
                known <- c(known, temp_ref = input$iso_bigelow_reftemp)
            } else {
                guess <- c(guess, temp_ref = input$iso_bigelow_reftemp)
            }
            
            
        } else if (input$inactv_model == "Peleg") {
            
            model <- "Peleg"
            
            if (input$iso_peleg_k_fix) {
                known <- c(known, k_b = input$iso_peleg_k)
            } else {
                guess <- c(guess, k_b = input$iso_peleg_k)
            }
            
            if (input$iso_peleg_n_fix) {
                known <- c(known, n = input$iso_peleg_n)
            } else {
                guess <- c(guess, n = input$iso_peleg_n)
            }
            
            if (input$iso_peleg_critemp_fix) {
                known <- c(known, temp_crit = input$iso_peleg_crittemp)
            } else {
                guess <- c(guess, temp_crit = input$iso_peleg_crittemp)
            }
            
        } else if (input$inactv_model == "Mafart") {
            
            model <- "Mafart"
            
            if (input$iso_mafart_delta_fix) {
                known <- c(known, delta_ref = input$iso_mafart_delta)
            } else {
                guess <- c(guess, delta_ref = input$iso_mafart_delta)
            }
            
            if (input$iso_mafart_z_fix) {
                known <- c(known, z = input$iso_mafart_z)
            } else {
                guess <- c(guess, z = input$iso_mafart_z)
            }
            
            if (input$iso_mafart_p_fix) {
                known <- c(known, p = input$iso_mafart_p)
            } else {
                guess <- c(guess, p = input$iso_mafart_p)
            }
            
            if (input$iso_mafart_reftemp_fix) {
                known <- c(known, temp_ref = input$iso_mafart_reftemp)
            } else {
                guess <- c(guess, temp_ref = input$iso_mafart_reftemp)
            }
            
        } else if (input$inactv_model == "Arrhenius") {
            
            model <- "Arrhenius"
            
            if (input$iso_arrhenius_k_fix) {
                known <- c(known, k_ref = input$iso_arrhenius_k)
            } else {
                guess <- c(guess, k_ref = input$iso_arrhenius_k)
            }
            
            if (input$iso_arrhenius_Ea_fix) {
                known <- c(known, Ea = input$iso_arrhenius_Ea)
            } else {
                guess <- c(guess, Ea = input$iso_arrhenius_Ea)
            }
            
            if (input$iso_arrhenius_reftemp_fix) {
                known <- c(known, temp_ref = input$iso_arrhenius_reftemp)
            } else {
                guess <- c(guess, temp_ref = input$iso_arrhenius_reftemp)
            }
            
        } else {
            NULL
        }
        
        list(model = model, guess = guess, known = known)
    })
    
    return(out)
}



























