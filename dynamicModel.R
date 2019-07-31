
dynamicModel <- function(input, output, session, dyna_temp_profile, dyna_micro_data, withN0 = TRUE) {
  
  ## Plot of the guess
  
  output$dyna_plot_guess <- renderPlot({
    
    validate({
      need(dyna_temp_profile(), "Define a temperature profile")
    })

    my_temperature <- as.data.frame(dyna_temp_profile()) %>% na.omit()

    times <- seq(0, max(my_temperature$time), length = 200)

    my_model <- dyna_pars()$model

    my_data <- dyna_micro_data() %>%
      mutate(logN = log10(N))

    model_pars <- list(guess = c(dyna_pars()$guess, dyna_pars()$known),
                       lower = c(dyna_pars()$lower, dyna_pars()$known),
                       upper = c(dyna_pars()$upper, dyna_pars()$known))

    model_pars %>%
      map(~ predict_inactivation(my_model, times, ., my_temperature)) %>%
      map(~ .$simulation) %>%
      imap_dfr(~ mutate(.x, which = .y)) %>%
      ggplot(.) +
      geom_line(aes(x = time, y = logN, colour = which)) +
      geom_point(aes(x = time, y = logN), data = my_data)

  })
  
  ## Extract the parameters
  
  dyna_pars <- reactive({
    
    guess <- numeric()
    lower <- numeric()
    upper <- numeric()
    known <- numeric()
    
    ## Bigelow model
    
    if (input$inactv_model == "Bigelow") {
      
      model <- "Bigelow"
      
      if (input$bigelow_D_fix) {
        known <- c(known, D_R = input$guess_bigelow_D)
      } else {
        guess <- c(guess, D_R = input$guess_bigelow_D)
        upper <- c(upper, D_R = input$upper_bigelow_D)
        lower <- c(lower, D_R = input$lower_bigelow_D)
      }
      
      if (input$bigelow_z_fix) {
        known <- c(known, z = input$guess_bigelow_z)
      } else {
        guess <- c(guess, z = input$guess_bigelow_z)
        upper <- c(upper, z = input$upper_bigelow_z)
        lower <- c(lower, z = input$lower_bigelow_z)
      }
      
      if (input$bigelow_reftemp_fix) {
        known <- c(known, temp_ref = input$guess_bigelow_reftemp)
      } else {
        guess <- c(guess, temp_ref = input$guess_bigelow_reftemp)
        upper <- c(upper, temp_ref = input$upper_bigelow_reftemp)
        lower <- c(lower, temp_ref = input$lower_bigelow_reftemp)
      }
      
      if (withN0) {
        if (input$bigelow_N0_fix) {
          known <- c(known, logN0 = input$guess_bigelow_N0)
        } else {
          guess <- c(guess, logN0 = input$guess_bigelow_N0)
          upper <- c(upper, logN0 = input$upper_bigelow_N0)
          lower <- c(lower, logN0 = input$lower_bigelow_N0)
        }
      }
      
      ## Peleg model
      
    } else if (input$inactv_model == "Peleg") {
      
      model <- "Peleg"
      
      if (input$peleg_k_fix) {
        known <- c(known, k_b = input$guess_peleg_k)
      } else {
        guess <- c(guess, k_b = input$guess_peleg_k)
        upper <- c(upper, k_b = input$upper_peleg_k)
        lower <- c(lower, k_b = input$lower_peleg_k)
      }
      
      if (input$peleg_Tcrit_fix) {
        known <- c(known, temp_crit = input$guess_peleg_Tcrit)
      } else {
        guess <- c(guess, temp_crit = input$guess_peleg_Tcrit)
        upper <- c(upper, temp_crit = input$upper_peleg_Tcrit)
        lower <- c(lower, temp_crit = input$lower_peleg_Tcrit)
      }
      
      if (input$peleg_n_fix) {
        known <- c(known, n = input$guess_peleg_n)
      } else {
        guess <- c(guess, n = input$guess_peleg_n)
        upper <- c(upper, n = input$upper_peleg_n)
        lower <- c(lower, n = input$lower_peleg_n)
      }
      
      if (withN0) {
        if (input$peleg_N0_fix) {
          known <- c(known, logN0 = input$guess_peleg_N0)
        } else {
          guess <- c(guess, logN0 = input$guess_peleg_N0)
          upper <- c(upper, logN0 = input$upper_peleg_N0)
          lower <- c(lower, logN0 = input$lower_peleg_N0)
        }
      }
      
      ## Mafart model
      
    } else if (input$inactv_model == "Mafart") {
      
      model <- "Mafart"
      
      if (input$mafart_delta_fix) {
        known <- c(known, delta_ref = input$guess_mafart_delta)
      } else {
        guess <- c(guess, delta_ref = input$guess_mafart_delta)
        upper <- c(upper, delta_ref = input$upper_mafart_delta)
        lower <- c(lower, delta_ref = input$lower_mafart_delta)
      }
      
      if (input$mafart_z_fix) {
        known <- c(known, z = input$guess_mafart_z)
      } else {
        guess <- c(guess, z = input$guess_mafart_z)
        upper <- c(upper, z = input$upper_mafart_z)
        lower <- c(lower, z = input$lower_mafart_z)
      }
      
      if (input$mafart_reftemp_fix) {
        known <- c(known, temp_ref = input$guess_mafart_reftemp)
      } else {
        guess <- c(guess, temp_ref = input$guess_mafart_reftemp)
        upper <- c(upper, temp_ref = input$upper_mafart_reftemp)
        lower <- c(lower, temp_ref = input$lower_mafart_reftemp)
      }
      
      if (input$mafart_p_fix) {
        known <- c(known, p = input$guess_mafart_p)
      } else {
        guess <- c(guess, p = input$guess_mafart_p)
        upper <- c(upper, p = input$upper_mafart_p)
        lower <- c(lower, p = input$lower_mafart_p)
      }
      
      if (withN0) {
        if (input$mafart_N0_fix) {
          known <- c(known, logN0 = input$guess_mafart_N0)
        } else {
          guess <- c(guess, logN0 = input$guess_mafart_N0)
          upper <- c(upper, logN0 = input$upper_mafart_N0)
          lower <- c(lower, logN0 = input$lower_mafart_N0)
        }
      }
      
      ## Geeraerd model
      
    } else if (input$inactv_model == "Geeraerd") {
      
      model <- "Geeraerd"
      
      if (input$geeraerd_D_fix) {
        known <- c(known, D_R = input$guess_geeraerd_D)
      } else {
        guess <- c(guess, D_R = input$guess_geeraerd_D)
        upper <- c(upper, D_R = input$upper_geeraerd_D)
        lower <- c(lower, D_R = input$lower_geeraerd_D)
      }
      
      if (input$geeraerd_z_fix) {
        known <- c(known, z = input$guess_geeraerd_z)
      } else {
        guess <- c(guess, z = input$guess_geeraerd_z)
        upper <- c(upper, z = input$upper_geeraerd_z)
        lower <- c(lower, z = input$lower_geeraerd_z)
      }
      
      if (input$geeraerd_reftemp_fix) {
        known <- c(known, temp_ref = input$guess_geeraerd_reftemp)
      } else {
        guess <- c(guess, temp_ref = input$guess_geeraerd_reftemp)
        upper <- c(upper, temp_ref = input$upper_geeraerd_reftemp)
        lower <- c(lower, temp_ref = input$lower_geeraerd_reftemp)
      }
      
      if (input$geeraerd_Nmin_fix) {
        known <- c(known, N_min = input$guess_geeraerd_Nmin)
      } else {
        guess <- c(guess, N_min = input$guess_geeraerd_Nmin)
        upper <- c(upper, N_min = input$upper_geeraerd_Nmin)
        lower <- c(lower, N_min = input$lower_geeraerd_Nmin)
      }
      
      if (input$geeraerd_Cc0_fix) {
        known <- c(known, C_c0 = input$guess_geeraerd_Cc0)
      } else {
        guess <- c(guess, C_c0 = input$guess_geeraerd_Cc0)
        upper <- c(upper, C_c0 = input$upper_geeraerd_Cc0)
        lower <- c(lower, C_c0 = input$lower_geeraerd_Cc0)
      }
      
      if (withN0) {
        if (input$geeraerd_N0_fix) {
          known <- c(known, logN0 = input$guess_geeraerd_N0)
        } else {
          guess <- c(guess, logN0 = input$guess_geeraerd_N0)
          upper <- c(upper, logN0 = input$upper_geeraerd_N0)
          lower <- c(lower, logN0 = input$lower_geeraerd_N0)
        }
      }
      
      
    } else if (input$inactv_model == "Arrhenius") {
      
      model <- "Arrhenius"
      
      if (input$arrhenius_k_fix) {
        known <- c(known, k_ref = input$guess_arrhenius_k)
      } else {
        guess <- c(guess, k_ref = input$guess_arrhenius_k)
        upper <- c(upper, k_ref = input$upper_arrhenius_k)
        lower <- c(lower, k_ref = input$lower_arrhenius_k)
      }
      
      if (input$arrhenius_Ea_fix) {
        known <- c(known, Ea = input$guess_arrhenius_Ea)
      } else {
        guess <- c(guess, Ea = input$guess_arrhenius_Ea)
        upper <- c(upper, Ea = input$upper_arrhenius_Ea)
        lower <- c(lower, Ea = input$lower_arrhenius_Ea)
      }
      
      if (input$arrhenius_reftemp_fix) {
        known <- c(known, temp_ref = input$guess_arrhenius_reftemp)
      } else {
        guess <- c(guess, temp_ref = input$guess_arrhenius_reftemp)
        upper <- c(upper, temp_ref = input$upper_arrhenius_reftemp)
        lower <- c(lower, temp_ref = input$lower_arrhenius_reftemp)
      }
      
      if (withN0) {
        if (input$arrhenius_N0_fix) {
          known <- c(known, logN0 = input$guess_arrhenius_N0)
        } else {
          guess <- c(guess, logN0 = input$guess_arrhenius_N0)
          upper <- c(upper, logN0 = input$upper_arrhenius_N0)
          lower <- c(lower, logN0 = input$lower_arrhenius_N0)
        }
      }
      
    } else {
      NULL
    }
    
    list(model = model,
         guess = guess, lower = lower, upper = upper,
         known = known)
  })
  
  ## Prior distribution
  
  prior_data <- reactive({
    
    ## Bigelow model
    
    if (input$inactv_model == "Bigelow") {
      
      my_indexes <- !c(input$bigelow_D_fix, input$bigelow_z_fix, input$bigelow_N0_fix)
      
      mu <- c(input$mean_prior_Bigelow_D, input$mean_prior_Bigelow_z, input$mean_prior_Bigelow_N0)[my_indexes]
      sigma <- c(input$sd_prior_bigelow_D, input$sd_prior_bigelow_z, input$sd_prior_bigelow_N0)[my_indexes]

    } else if (input$inactv_model == "Peleg") {
      
      my_indexes <- !c(input$peleg_k_fix, input$peleg_n_fix, input$peleg_Tcrit_fix, input$peleg_N0_fix)
      
      mu <- c(input$mean_prior_peleg_k, input$mean_prior_peleg_n, input$mean_prior_peleg_Tcrit, input$mean_prior_peleg_N0)[my_indexes]
      sigma <- c(input$sd_prior_peleg_k, input$sd_prior_peleg_n, input$sd_prior_peleg_Tcrit, input$sd_prior_peleg_N0)[my_indexes]
     
    } else if (input$inactv_model == "Mafart") {
      
      my_indexes <- !c(input$mafart_delta_fix, input$mafart_z_fix, input$mafart_p_fix, input$mafart_N0_fix)
      
      mu <- c(input$mean_prior_Mafart_delta, input$mean_prior_mafart_z, input$mean_prior_mafart_p, input$mean_prior_mafart_N0)[my_indexes]
      sigma <- c(input$sd_prior_Mafart_delta, input$sd_prior_mafart_z, input$sd_prior_mafart_p, input$sd_prior_mafart_N0)[my_indexes]
      
      
    } else if (input$inactv_model == "Geeraerd") {
      
      my_indexes <- !c(input$geeraerd_D_fix, input$geeraerd_z_fix, input$geerared_Nmin_fix, input$geeraerd_Cc0_fix, input$geeraerd_N0_fix)
      
      mu <- c(input$mean_prior_geeraerd_D, input$mean_prior_geeraerd_z, input$mean_prior_geeraerd_Nmin, input$mean_prior_geeraerd_Cc0, input$mean_prior_geeraerd_N0)[my_indexes]
      sigma <- c(input$sd_prior_geeraerd_D, input$sd_prior_geeraerd_z, input$sd_prior_geeraerd_Nmin, input$sd_prior_geeraerd_Cc0, input$sd_prior_geeraerd_N0)[my_indexes]
      
     
    } else if (input$inactv_model == "Arrhenius") {
      
      my_indexes <- !c(input$arrhenius_k_fix, input$arrhenius_Ea_fix, input$arrhenius_N0_fix)
      
      mu <- c(input$mean_prior_arrhenius_k, input$mean_prior_arrhenius_Ea, input$mean_prior_arrhenius_N0)[my_indexes]
      sigma <- c(input$sd_prior_arrhenius_k, input$sd_prior_arrhenius_Ea, input$sd_prior_arrhenius_N0)[my_indexes]
    
      
    } else {
      NULL
    }

    list(mu = mu,
         sigma = sigma)
  })
  
  ## Model fitting
  
  dyna_model_fit <- eventReactive(input$dyna_fit_button, {

    my_temperature <- as.data.frame(dyna_temp_profile()) %>% na.omit()

    my_model <- dyna_pars()$model

    my_data <- dyna_micro_data() %>%
      mutate(logN = log10(N))

    withProgress(message = "Fitting model", {

      if (input$dyna_algorithm == "nlr") {

        fit_dynamic_inactivation(my_data, my_model, my_temperature,
                                 dyna_pars()$guess,
                                 dyna_pars()$upper,
                                 dyna_pars()$lower,
                                 dyna_pars()$known)
        
      } else if (input$dyna_algorithm == "MCMC") {
        
        fit_inactivation_MCMC(my_data, my_model, my_temperature,
                              dyna_pars()$guess,
                              dyna_pars()$upper,
                              dyna_pars()$lower,
                              dyna_pars()$known,
                              niter = input$dyna_niters)
        
      } else {
        
        my_prior <- function(p) {

          R <- diag(length(p))  # Expecting that people are able to define a positive definite matrix is just too much
          sds <- prior_data()$sigma
          sigma <- R * sds * rep(sds, each = nrow(R))
          
          dmvnorm(p, mean = prior_data()$mu, sigma = sigma)

        }
        
        fit_inactivation_MCMC(my_data, my_model, my_temperature,
                              dyna_pars()$guess,
                              dyna_pars()$upper,
                              dyna_pars()$lower,
                              dyna_pars()$known,
                              niter = input$dyna_niters,
                              prior = my_prior)
      }
    })
  })
  
  ## Return the interesting stuff
  
  return(dyna_model_fit)
  
  
}






