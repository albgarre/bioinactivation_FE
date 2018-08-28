
predPars <- function(input, output, session) {
    
    out <- reactive({
        
        if (input$inactv_model == "Bigelow") {
            list(model = "Bigelow",
              D_R = input$bigelow_D, z = input$bigelow_z, 
              temp_ref = input$bigelow_ref_temp,
              N0 = input$bigelow_N0)
            
        } else if (input$inactv_model == "Peleg") {
            list(model = "Peleg",
              k_b = input$peleg_k, n = input$peleg_n,
              temp_crit = input$peleg_tempcrit,
              N0 = input$bigelow_N0)
            
        } else if (input$inactv_model == "Mafart") {
            list(model = "Mafart",
              delta_ref = input$mafart_delta, z = input$mafart_z,
              temp_ref = input$mafart_ref_temp, p = input$mafart_p,
              N0 = input$mafart_N0)
            
        } else if (input$inactv_model == "Geeraerd") {
            list(model = "Geeraerd",
                 D_R = input$geeraerd_D, z = input$geeraerd_z,
                 N_min = input$geeraerd_Nmin, 
                 temp_ref = input$geeraerd_ref_temp, N0 = input$geeraerd_N0,
                 C_c0 = input$geeraerd_Cc0)
            
        } else if (input$inactv_model == "Arrhenius") {
            list(model = "Arrhenius",
                 k_ref = input$arrhenius_k,
                 Ea = input$arrhenius_Ea, 
                 temp_ref = input$arrhenius_reftemp,
                 N0 = input$arrhenius_N0)
            
        } else {
            NULL
        }
    })
    
    print(out)
    return(out)
    
}




















