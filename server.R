
## Load libraries

library(shiny)
library("shinyIncubator")

# library(ggplot2)
library(tidyverse)

library(bioinactivation)

library(FME)

source("tableFileUI.R")
source("tableFile.R")
source("predPars.R")
source("predParsInput.R")
source("fitPars.R")
source("tableFile3col.R")
source("isofitPars.R")

## Server

shinyServer(function(input, output) {
    
    ## Header
    
    # output$notificationMenu <- renderMenu({
    #     dropdownMenu(type = "notifications",
    #                  notificationItem(
    #                      text = "5 new users today",
    #                      icon("users")
    #                  ))
    # })
    
    ## Prediction module
    
    #' Data input
    
    pred_temp_profile <- callModule(tableFile, "pred_temp_input", label_2 = "temperature")
    pred_micro_data <- callModule(tableFile, "pred_micro_data")
    pred_model_pars <- callModule(predPars, "pred_pars")
    
    #' Make the simulations
    
    pred_simulation <- eventReactive(input$pred_calculate, {

        validate({
            need(pred_temp_profile(), "Define a temperature profile")
        })

        my_temperature <- as.data.frame(pred_temp_profile()) %>% na.omit()

        times <- seq(0, max(my_temperature$time), length = 100)
        
        model_pars <- pred_model_pars()
        my_model <- model_pars$model
        model_pars <- model_pars[-1]

        predict_inactivation(my_model, times, model_pars,
                             my_temperature)

    })
    
    #' Plot of the simulation
    
    output$pred_simul_plot <- renderPlot({
        p <- plot(pred_simulation(), plot_temp = input$pred_addTemp,
                  label_y1 = input$pred_ylabel,
                  label_y2 = input$pred_ylabel2,
                  ylims = c(input$pred_ymin, input$pred_ymax)) +
            xlab(input$pred_xlabel)

        micro_data <- as.data.frame(pred_micro_data()) %>%
            mutate(logN = log10(N))
        
        p + geom_point(aes(x = time, y = logN), data = micro_data) # +
            # ylim(input$pred_ymin, input$pred_ymax)

    })
    
    #' Residual analysis
    
    pred_residuals <- reactive({
        
        my_simulation <- pred_simulation() %>%
            .$simulation %>%
            select(time, logN)
        
        my_obs <- pred_micro_data() %>%
            mutate(logN = log10(N)) %>%
            select(time, logN)
        
        modCost(model = my_simulation,
                obs = my_obs)
    })
    
    output$pred_residuals <- renderTable(
        pred_residuals()$residuals %>%
            select(Oberved = obs, Predicted = mod, 
                   Residual = res)
    )
    
    output$pred_residual_statistics <- renderTable({ 
        
        residuals <- pred_residuals()$residuals$res
        
        n_pars <- switch(pred_simulation()$model,
            Bigelow = 2,
            Mafart = 3,
            Peleg = 3,
            Geeraerd = 4,
            Arrhenius = 2
        )
        
        n_obs <- nrow(pred_micro_data())
        
        data.frame(ME = mean(residuals),
                   RMSE = pred_residuals()$model,
                   loglik = pred_residuals()$minlogp
                   ) %>%
            mutate(AIC = 2*n_pars - 2*loglik) %>%
            mutate(AICc = AIC + 2*n_pars*(n_pars + 1)/(n_obs - n_pars - 1),
                   BIC = log(n_obs)*n_pars - 2*loglik) %>%
            mutate(Af = 10^RMSE, Bf = 10^ME)
    })
    
    output$pred_residuals_normality <- renderText({
        
        test_results <- shapiro.test(pred_residuals()$residuals$res)
        
        if (test_results$p.value < 0.05) {
            paste0("There is enough statistical signifficante to state that residuals are not normal, p-value: ", 
                   round(test_results$p.value, 3))
        } else {
            paste0("There is NOT enough statistical signifficante to state that residuals are not normal, p-value: ", 
                   round(test_results$p.value, 3))
        }
        
    })
    
    #' Results download
    
    output$pred_down_curve <- downloadHandler(
        filename = "survivor-curve.csv",
        content = function(file) {
            write.csv(pred_simulation()$simulation, file = file,
                      row.names = FALSE)
        }
    )
    
    output$pred_down_residuals <- downloadHandler(
        filename = "residual-table.csv",
        content = function(file) {
            pred_residuals()$residuals %>%
                select(Oberved = obs, Predicted = mod, 
                       Residual = res) %>%
                write.csv(., file = file,
                          row.names = FALSE)
        }
    )
    
    ## Isothermal fitting module
    
    #' Data input
    
    iso_my_data <- callModule(tableFile3col, "iso_data_input")
    iso_par_guess <- callModule(isofitPars, "iso_model_pars")
    
    #' Model fit
    
    iso_fitted_model <- eventReactive(input$iso_fit_button, {

        my_data <- iso_my_data() %>%
            as.data.frame() %>%
            rename(temp = temperature, log_diff = logS)
        
        model_name <- iso_par_guess()$model
        known_params <- as.list(iso_par_guess()$known)
        starting_point <- iso_par_guess()$guess
        
        fit_isothermal_inactivation(model_name,
                                    my_data, starting_point,
                                    known_params)
        
    })
    
    output$iso_fitted_curve <- renderPlot({
        
        validate(need(iso_fitted_model(), message = FALSE))
        
        plot(iso_fitted_model(), make_gg = TRUE)
    })
    
    output$iso_pars_table <- renderTable({
        
        validate(need(iso_fitted_model(), message = FALSE))
        
        summary_iso_fit(iso_fitted_model())
        
    })
    
    output$iso_residual_statistics <- renderTable({ 
        
        residuals <- residuals(iso_fitted_model()$nls)
        
        my_dof <- summary(iso_fitted_model()$nls)$df
        
        n_pars <- my_dof[1]
        
        n_obs <- sum(my_dof)
        
        data.frame(ME = mean(residuals),
                   RMSE = sqrt(mean(residuals^2)),
                   loglik = logLik(iso_fitted_model()$nls)
        ) %>%
            mutate(AIC = 2*n_pars - 2*loglik) %>%
            mutate(AICc = AIC + 2*n_pars*(n_pars + 1)/(n_obs - n_pars - 1),
                   BIC = log(n_obs)*n_pars - 2*loglik) %>%
            mutate(Af = 10^RMSE, Bf = 10^ME)
    })
    
    output$iso_residual_plot <- renderPlot({
        
        iso_fitted_model() %>%
            .$data %>%
            mutate(res = residuals(summary(iso_fitted_model()))) %>%
            mutate(temperature = factor(temp)) %>%
            ggplot() +
                geom_point(aes(x = time, y = res, colour = temperature)) +
                geom_hline(yintercept = 0, linetype = 2, size = 1) +
                ylab("Residual") + xlab("Time")
    })
    
    output$iso_residuals_normality <- renderText({

        test_results <- shapiro.test(residuals(iso_fitted_model()$nls))
        
        if (test_results$p.value < 0.05) {
            paste0("There is enough statistical signifficante to state that residuals are not normal, p-value: ", 
                   round(test_results$p.value, 3))
        } else {
            paste0("There is NOT enough statistical signifficante to state that residuals are not normal, p-value: ", 
                   round(test_results$p.value, 3))
        }
        
    })
    
    ## Dynamic fitting module
    
    #' Reset seeds
    
    observeEvent(input$btn_dyna_seed, {
        set.seed(1720172)
    })
    
    observeEvent(input$btn_interv_seed, {
        set.seed(1720172)
    })
    
    #' Data input
    
    dyna_temp_profile <- callModule(tableFile, "dyna_temp_input", 
                                    label_2 = "temperature")
    dyna_micro_data <- callModule(tableFile, "dyna_micro_data")
    dyna_model_pars <- callModule(fitPars, "dyna_model_pars")
    
    #' Plot of the guesses
    
    output$dyna_plot_guess <- renderPlot({
        
        validate({
            need(dyna_temp_profile(), "Define a temperature profile")
        })
        
        my_temperature <- as.data.frame(dyna_temp_profile()) %>% na.omit()
        
        times <- seq(0, max(my_temperature$time), length = 200)
        
        my_model <- dyna_model_pars()$model
        
        my_data <- dyna_micro_data() %>%
            mutate(logN = log10(N))
        
        model_pars <- list(guess = c(dyna_model_pars()$guess, dyna_model_pars()$known),
                           lower = c(dyna_model_pars()$lower, dyna_model_pars()$known),
                           upper = c(dyna_model_pars()$upper, dyna_model_pars()$known))
        
        model_pars %>%
            map(~ predict_inactivation(my_model, times, ., my_temperature)) %>%
            map(~ .$simulation) %>%
            imap_dfr(~ mutate(.x, which = .y)) %>%
            ggplot(.) +
                geom_line(aes(x = time, y = logN, colour = which)) +
                geom_point(aes(x = time, y = logN), data = my_data)
        
        })
    
    #' Model fitting
    
    dyna_model_fit <- eventReactive(input$dyna_fit_button, {
        
        my_temperature <- as.data.frame(dyna_temp_profile()) %>% na.omit()

        my_model <- dyna_model_pars()$model
        
        my_data <- dyna_micro_data() %>%
            mutate(logN = log10(N))
        
        withProgress(message = "Fitting model", {
            
            if (input$dyna_algorithm == "nlr") {
                
                fit_dynamic_inactivation(my_data, my_model, my_temperature,
                                         dyna_model_pars()$guess, 
                                         dyna_model_pars()$upper, 
                                         dyna_model_pars()$lower,
                                         dyna_model_pars()$known)
            } else {
                fit_inactivation_MCMC(my_data, my_model, my_temperature,
                                      dyna_model_pars()$guess, 
                                      dyna_model_pars()$upper, 
                                      dyna_model_pars()$lower,
                                      dyna_model_pars()$known,
                                      niter = input$dyna_niters)
            }
        })
    })
    
    dyna_modCost <- reactive({
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        my_simulation <- dyna_model_fit() %>%
            .$best_prediction %>%
            .$simulation %>%
            select(time, logN)
        
        my_data <- dyna_micro_data() %>%
            mutate(logN = log10(N)) %>%
            select(time, logN)
        
        modCost(model = my_simulation,
                obs = my_data)
    })
    
    #' Plot of the fit
    
    output$dyna_plot_fit <- renderPlot({
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        plot(dyna_model_fit(), plot_temp = input$dyna_addTemp,
             label_y1 = input$dyna_ylabel,
             label_y2 = input$dyna_ylabel2,
             ylims = c(input$dyna_ymin, input$dyna_ymax)) +
             xlab(input$dyna_xlabel)
        
    })
    
    #' Model parameters and indexes of the fit
    
    output$dyna_fitted_pars <- renderTable({
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        if (is.FitInactivation(dyna_model_fit()) ) {
            summary_dynamic_fit(dyna_model_fit())
        } else {
            summary_MCMC_fit(dyna_model_fit())
        }
        
    })

    output$dyna_residuals_statistics <- renderTable({ 
        
        residuals <- dyna_modCost()$residuals$res

        if (is.FitInactivation(dyna_model_fit())) {
            n_pars <- length(dyna_model_fit()$fit_results$par)
        } else {
            n_pars <- length(dyna_model_fit()$modMCMC$bestpar)
        }

        n_obs <- nrow(dyna_micro_data())

        out <- data.frame(ME = mean(residuals),
                   RMSE = dyna_modCost()$model,
                   loglik = dyna_modCost()$minlogp
        ) %>%
            mutate(AIC = 2*n_pars - 2*loglik) %>%
            mutate(AICc = AIC + 2*n_pars*(n_pars + 1)/(n_obs - n_pars - 1),
                   BIC = log(n_obs)*n_pars - 2*loglik) %>%
            mutate(Af = 10^RMSE, Bf = 10^ME)
        
        out
    })
    
    #' Diagnostics
    
    output$dyna_residuals_plot <- renderPlot({
        
        dyna_modCost() %>%
            .$residuals %>%
            as.data.frame() %>%
            ggplot() +
                geom_point(aes(x = x, y = res)) +
                xlab("Time") + ylab("Residuals") +
                geom_hline(yintercept = 0, linetype = 2, size = 1)
    })
    
    output$dyna_MCMC_pairs <- renderPlot({
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        if (is.FitInactivationMCMC(dyna_model_fit())) {
            pairs(dyna_model_fit()$modMCMC)
            
        } else {
            NULL
        }
    })
    
    output$dyna_par_cor <- renderTable({
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        if (is.FitInactivation(dyna_model_fit())) {
            cov2cor(summary(dyna_model_fit())$cov.unscaled)
            
        } else {
            NULL
        }
        
    })
    
    output$dyna_MCMC_conv_plot <- renderPlot({
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        if (is.FitInactivationMCMC(dyna_model_fit())) {
            plot(dyna_model_fit()$modMCMC)
            
        } else {
            NULL
        }
    })
    
    output$dyna_residuals_normality <- renderText({
        
        test_results <- shapiro.test(dyna_modCost()$residuals$res)
        
        if (test_results$p.value < 0.05) {
            paste0("There is enough statistical signifficante to state that residuals are not normal, p-value: ", 
                   round(test_results$p.value, 3))
        } else {
            paste0("There is NOT enough statistical signifficante to state that residuals are not normal, p-value: ", 
                   round(test_results$p.value, 3))
        }
        
    })
    
    #' Results download
    
    output$dyna_down_curve <- downloadHandler(
        filename = "survivor-curve.csv",
        content = function(file) {
            write.csv(dyna_model_fit()$best_prediction$simulation,
                      file = file, row.names = FALSE)
        }
    )
    
    output$dyna_down_residuals <- downloadHandler(
        filename = "residual-table.csv",
        content = function(file) {
            dyna_modCost()$residuals %>%
                select(Oberved = obs, Predicted = mod, 
                       Residual = res) %>%
                write.csv(., file = file,
                          row.names = FALSE)
        }
    )
    
    ## Prediction interval
    
    #' Data input
    
    interv_temp_profile <- callModule(tableFile, "interv_temp_input",
                                      label_2 = "temperature")
    
    #' Calculation
    
    interv_prediction_interval <- eventReactive(input$interv_calculate, {
        
        validate(need(dyna_model_fit(),
                      message = "A model must be fitted first."))
        
        my_temperature <- as.data.frame(interv_temp_profile()) %>% na.omit()
        
        times <- seq(0, max(my_temperature$time), length = 50)
        
        withProgress(message = "Calculating interval", {
            
            predict_inactivation_MCMC(dyna_model_fit(), my_temperature,
                                      input$interv_niter, times,
                                      quantiles = input$interv_quantile)
        })

    }) 
    
    output$interv_out_plot <- renderPlot({
        
        validate(need(interv_prediction_interval(), message = FALSE))
        
        plot(interv_prediction_interval()) +
            xlab(input$interv_xlabel) + ylab(input$interv_ylabel) +
            ylim(input$interv_ymin, input$interv_ymax)
        
    })
    
})

## Helping functions

#' 
#' Summary of a dynamic fit object
#' 
#' Returns a data frame with the estimate, standard deviation and 95% confidence
#' interval of the model paramters of an object generated using non linear
#' regression.
#' 
summary_dynamic_fit <- function(dynamic_fit) {
    
    fit_summary <- summary(dynamic_fit)
    
    out_frame <- as.data.frame(fit_summary$par)
    out_frame <- cbind(rownames(out_frame), out_frame, stringsAsFactors=FALSE)
    out_frame <- out_frame[ , 1:3]
    names(out_frame) <- c("parameter", "estimate", "std")
    n_df <- fit_summary$df[2]
    t_value <- qt(0.975, n_df)
    
    out_frame <- mutate(out_frame,
                        lower95 = estimate - t_value*std,
                        upper95 = estimate + t_value*std
    )
    
    rownames(out_frame) <- out_frame$parameter
    
    if ("N0" %in% out_frame$parameter) {
        new_row <- c("logN0", log10(out_frame["N0", 2:5]))
        names(new_row) <- NULL
        out_frame <- rbind(out_frame, new_row)
    }
    
    names(out_frame) <- c("Parameter", "Estimate", "Std. deviation", "lower CI (95%)", "Upper CI (95%)")
    
    out_frame
    
}

#'
#' Summary of MCMC fit object
#' 
#' Returns a data frame with the estimate, standard deviation and 95% confidence
#' interval of the model paramters of an object generated using MCMC.
#' 
summary_MCMC_fit <- function(MCMC_fit) {
    
    fit_summary <- summary(MCMC_fit)
    
    intervals <- apply(MCMC_fit$modMCMC$pars, 2, quantile, probs = c(0.025, 0.975))
    
    out_frame <- data.frame(parameter = names(fit_summary),
                            estimate = MCMC_fit$modMCMC$bestpar,
                            std = unlist(fit_summary[2, ]),
                            lower95 = intervals[1, ],
                            upper95 = intervals[2, ],
                            stringsAsFactors = FALSE)
    
    if ("N0" %in% out_frame$parameter) {
        new_row <- c("logN0", log10(out_frame["N0", 2:5]))
        names(new_row) <- NULL
        out_frame <- rbind(out_frame, new_row)
    }
    
    names(out_frame) <- c("Parameter", "Estimate", "Std. deviation", "lower CI (95%)", "Upper CI (95%)")
    
    out_frame
    
}

#' 
#' Summary of a isothermal fit object
#' 
#' Returns a data frame with the estimate, standard deviation and 95% confidence
#' interval of the model paramters of an object generated using non linear
#' regression.
#' 
summary_iso_fit <- function(iso_fit) {
    
    fit_summary <- summary(iso_fit)
    
    out_frame <- as.data.frame(fit_summary$par)
    out_frame <- cbind(rownames(out_frame), out_frame, stringsAsFactors=FALSE)
    out_frame <- out_frame[ , 1:3]
    names(out_frame) <- c("parameter", "estimate", "std")
    n_df <- fit_summary$df[2]
    t_value <- qt(0.975, n_df)
    
    out_frame <- mutate(out_frame,
                        lower95 = estimate - t_value*std,
                        upper95 = estimate + t_value*std
    )
    
    rownames(out_frame) <- out_frame$parameter
    
    names(out_frame) <- c("Parameter", "Estimate", "Std. deviation", "lower CI (95%)", "Upper CI (95%)")
    
    out_frame
    
}
























