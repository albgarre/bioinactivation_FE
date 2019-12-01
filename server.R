
## Load libraries

library(shiny)
library("shinyIncubator")

# library(ggplot2)
library(tidyverse)

library(bioinactivation)

library(FME)

library(GGally)
library(zoo)
library(mvtnorm)
library(cowplot)

source("tableFileUI.R")
source("tableFile.R")
source("predPars.R")
source("predParsInput.R")
source("fitPars.R")
source("tableFile3col.R")
source("isofitPars.R")
source("dynamicModel.R")

library(FSK2R)
library(readxl)
# library(DT)
library(rhandsontable)

##

theme_set(theme_grey())

## Global variables

par_description_map <- data.frame(
    parameter = c("temp_ref", "D_R", "z", "N0", "delta_ref", "p", "n", "k_b", "temp_crit", "N_min", "C_c0",
                  "k_ref", "Ea"),
    description = c("Reference temperature.", "Treatment time required for one log-reduction.",
                    "Temperature increase requried to reduce the D (or delta) value a 90%.",
                    "Initial microbial count",
                    "Treatment time required for the 1st log reduction.",
                    "Shape factor of the Weibull distribution",
                    "Shape factor of the Weibull distribution",
                    "Slope of the b vs temperature curve for temperatures about the critical one",
                    "Critical temperature for inactivation.",
                    "Tail height.",
                    "Initial value of the ideal substance defining the shoulder.",
                    "Inactivation rate at the reference temperature",
                    "Activation energy"),
    stringsAsFactors = FALSE
)

## Server

shinyServer(function(input, output, session) {
    
    ## Manual
    
    output$download_manual <- downloadHandler(
        filename = "manual_Bioinactivatino.pdf",
        content = function(file) {
            file.copy("manual_Bioinactivation.pdf", file)
        }
    )
    
    ## Popup message
    
    # toggleModal(session, "startupModal", toggle = "open")
    
    ## Header
    
    # output$notificationMenu <- renderMenu({
    #     dropdownMenu(type = "notifications",
    #                  notificationItem(
    #                      text = "5 new users today"
    #                      icon("users")
    #                  ))
    # })
    
    ## FSK2R
    
    output$fsk_pred_download <- downloadHandler(
        filename = "predictive_model.fskx",
        content = function(file) {
            
            ## Import the 'basic' model
            
            my_fsk <- import_fsk("fskBioinactivation.fskx")
            
            ## Metadata - general informatino
            
            my_fsk$metadata$generalInformation$name <- input$fsk_pred_name
            my_fsk$metadata$generalInformation$source <- input$fsk_pred_source
            my_fsk$metadata$generalInformation$identifier <- input$fsk_pred_identifier
            my_fsk$metadata$generalInformation$creationDate <- input$fsk_pred_date
            my_fsk$metadata$generalInformation$rights <- input$fsk_pred_rights
            my_fsk$metadata$generalInformation$language <- input$fsk_pred_language
            
            creators <- hot_to_r(input$fsk_pred_creators) %>%
                mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Contact")
            
            authors <- hot_to_r(input$fsk_pred_authors) %>%
                mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Contact")
            
            refs <- hot_to_r(input$fsk_pred_reference) %>%
                mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Reference",
                       isReferenceDescription = TRUE
                       )
            
            my_fsk$metadata$generalInformation$creators <- apply(creators, 1, as.list)
            my_fsk$metadata$generalInformation$author <- apply(authors, 1, as.list)
            my_fsk$metadata$generalInformation$reference <- apply(refs, 1, as.list)
            
            ## Metadata - scope
            
            my_fsk$metadata$scope$generalComment <- input$fsk_pred_genCom
            
            
            prods <- hot_to_r(input$fsk_pred_product) %>%
                mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Product")
            
            my_fsk$metadata$scope$product <- apply(prods, 1, as.list)
            
            haz <- hot_to_r(input$fsk_pred_hazard) %>%
                mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Hazard")
            
            my_fsk$metadata$scope$hazard <- apply(haz, 1, as.list)
            
            ## Metadata - data background
            
            my_fsk$metadata$dataBackground$study <- input$fsk_pred_studyTitle
            
            ## Metadata - model parameters
            
            model_name <- paste0("'", pred_simulation()$model, "'")
            max_time <- max(pred_simulation()$simulation$time)
            
            times <- paste0("seq(0, ", max_time, ", length=100)")
            
            my_temperature <- as.data.frame(pred_temp_profile()) %>% 
                filter(!is.na(temperature))
            
            time_points <- my_temperature$time
            temp_points <- my_temperature$temperature
            
            time_points <- paste(time_points, collapse = ",")
            time_points <- paste0("c(", time_points, ")")
            temp_points <- paste(temp_points, collapse = ",")
            temp_points <- paste0("c(", temp_points, ")")
            
            
            
            par_table <- hot_to_r(input$fsk_pred_model) %>%
                rename(parameterID = parameter,
                       parameterUnit = unit,
                       parameterDescription = description,
                       parameterValueMin = Min.value,
                       parameterValueMax = Max.value,
                       parameterValue = estimate
                ) %>%
                mutate(eClass = "http://BfR/bund/de/knime/model/metadata_V1.0.3#//Parameter",
                       parameterName = parameterID,
                       parameterClassification = "Input",
                       parameterUnitCategory = "NA",
                       parameterDataType = "Double",
                       parameterSource = "NA",
                       parameterSubject = "NA",
                       parameterDistribution = "Constant",
                       parameterVariabilitySubject = "NA",
                       parameterError = "NA"
                )
            
            other_pars <- tibble(parameterID = c("model_name", "max_time", "time_points", "temp_points"),
                                 parameterName = c("model_name", "max_time", "time_points", "temp_points"),
                                 parameterDescription = c("Inactivation model", "Maximum time for the simulation",
                                                          "Time points of the temperature profile",
                                                          "Temperature points of the temperature profile"),
                                 parameterValue = c(model_name, max_time, time_points, temp_points),
                                 eClass ="http://BfR/bund/de/knime/model/metadata_V1.0.3#//Parameter",
                                 parameterUnit = "",
                                 parameterValueMin = "",
                                 parameterValueMax = "",
                                 parameterClassification = "Input",
                                 parameterUnitCategory = "NA",
                                 parameterDataType = c("Character", "Double", "Double", "Double"),
                                 parameterSource = "NA",
                                 parameterSubject = "NA",
                                 parameterDistribution = "Constant",
                                 parameterVariabilitySubject = "NA",
                                 parameterError = "NA")
            
            list_par_table <- apply(par_table, 1, as.list)
            list_par_other <- apply(other_pars, 1, as.list)
            
            my_fsk$metadata$modelMath$parameter <- c(list_par_table, list_par_other)
            
            ## Define the simulation
            
            par_sims <- lapply(1:nrow(par_table), function(i) {
                
                new_elem <- list()
                attr(new_elem, "newValue") <- par_table$parameterValue[i]
                attr(new_elem, "target") <- par_table$parameterID[i]
                new_elem
                
            })
            
            other_sims <- lapply(1:nrow(other_pars), function(i) {
                
                new_elem <- list()
                attr(new_elem, "newValue") <- other_pars$parameterValue[i]
                attr(new_elem, "target") <- other_pars$parameterID[i]
                new_elem
                
            })
            
            my_sims <- c(par_sims, other_sims)
            
            my_sims <- set_names(my_sims, rep("changeAttribute", length(my_sims)))
            
            # print(my_sims)
            
            my_fsk$simulation$sedML$listOfModels$model$listOfChanges <- my_sims
            
            ## Export the model
            
            export_fsk(my_fsk, file)
            
            
            # file.copy("ToyModelv4.fskx", file)
        }
    )
    
    output$fsk_pred_creators <- renderRHandsontable({
        # rhandsontable(data.frame(Email = c("google@chucknorris.com", NA), `Family name` = c("Doe", NA), `Given Name` = c("Jon", NA)),
        #               rowHeaders = NULL, readOnly = FALSE
        #               )
        
        default_data <- data.frame(
            title = "",
            familyName = "",
            givenName = "",
            email = "",
            telephone = "",
            streetAdress = "",
            country = "",
            city = "",
            region = "",
            organization = ""
            )
        
        if (!is.null(input$fsk_pred_creators)) {
            DF = hot_to_r(input$fsk_pred_creators)
        } else {
            DF = default_data
        }
        
        DF %>%
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$fsk_pred_authors <- renderRHandsontable({
        
        default_data <- data.frame(
            title = "",
            familyName = "",
            givenName = "",
            email = "",
            telephone = "",
            streetAdress = "",
            country = "",
            city = "",
            region = "",
            organization = ""
        )
        
        if (!is.null(input$fsk_pred_authors)) {
            DF = hot_to_r(input$fsk_pred_authors)
        } else {
            DF = default_data
        }
        
        DF %>%
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$fsk_pred_reference <- renderRHandsontable({
        
        default_data <- data.frame(
            publicationType = "",
            publicationDate = "",
            doi = "",
            authorList = "",
            publicationTitle = "",
            publicationAbstract = "",
            publicationStatus = "",
            publicationWebsite = "",
            comment = ""
        )
        
        if (!is.null(input$fsk_pred_reference)) {
            DF = hot_to_r(input$fsk_pred_reference)
        } else {
            DF = default_data
        }
        
        DF %>%
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$fsk_pred_product <- renderRHandsontable({
        
        default_data <- data.frame(
            productName = "",
            productDescription = "",
            productUnit = "",
            productionMethod = "",
            packaging = "",
            productTreatment = "",
            originCountry = "",
            originArea = "",
            fisheriesArea = "",
            productionDate = "",
            expiryDate = ""
        )
        
        if (!is.null(input$fsk_pred_product)) {
            DF = hot_to_r(input$fsk_pred_product)
        } else {
            DF = default_data
        }
        
        DF %>%
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$fsk_pred_hazard <- renderRHandsontable({
        
        default_data <- data.frame(
            hazardType = "",
            hazardName = "",
            hazardDescription = "",
            hazardUnit = "",
            adverseEffect = "",
            sourceOfContamination = "",
            maximumResidueLimit = "",
            noObservedAdverseAffectLevel = "",
            lowestObservedAdverseAffectLevel = "",
            acceptableOperatorExposureLevel = "",
            acuteReferenceDose = "",
            acceptableDailyIntake = ""
        )
        
        if (!is.null(input$fsk_pred_hazard)) {
            DF = hot_to_r(input$fsk_pred_hazard)
        } else {
            DF = default_data
        }
        
        DF %>%
            rhandsontable() %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
    
    output$fsk_pred_model <- renderRHandsontable({
        my_prediction <- pred_simulation()
        
        out <- data.frame(parameter = names(my_prediction$model_parameters),
                   estimate = unlist(my_prediction$model_parameters),
                   unit = "",
                   `Min value` = 0,
                   `Max value` = "") %>%
            left_join(., par_description_map, by = "parameter")
        
        rhandsontable(out,
                      rowHeaders = NULL, readOnly = FALSE
                      )
    })
    
    output$fsk_iso_model <- renderRHandsontable({
        my_model <- iso_fitted_model()
        
        out <- data.frame(parameter = names(my_model$parameters),
                          estimate = unlist(my_model$parameters),
                          unit = "",
                          `Min value` = 0,
                          `Max value` = "") %>%
            left_join(., par_description_map, by = "parameter")
        
        rhandsontable(out,
                      rowHeaders = NULL, readOnly = FALSE
        )
    })
    
    
    ## Prediction module
    
    #' Data input
    
    pred_temp_profile <- callModule(tableFile, "pred_temp_input", 
                                    label_2 = "temperature", 
                                    default_data = data.frame(c(0, 10), c(70, 80))
                                    )
    
    pred_micro_data <- callModule(tableFile, "pred_micro_data",
                                  default_data = data.frame(c(0, 5, 7.5, 2.5, 6, 8),
                                                            c(1e6, 1e5, 15000, 800000, 30000, 1e3)
                                                            )
                                  )
    
    pred_model_pars <- callModule(predPars, "pred_pars")
    
    #' Make the simulations
    
    pred_simulation <- eventReactive(input$pred_calculate, {

        validate({
            need(pred_temp_profile(), "Define a temperature profile")
        })

        my_temperature <- as.data.frame(pred_temp_profile()) %>% 
            filter(!is.na(temperature))

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
        
        p <- p + geom_point(aes(x = time, y = logN), data = micro_data) # +
            # ylim(input$pred_ymin, input$pred_ymax)
        
        if (input$pred_add_time_to_X) {
            
            logN0 <- pred_simulation()$simulation$logN[1]
            t_D <- time_to_logreduction(input$pred_tgr_logreductions, pred_simulation())
            
            # print(logN0)
            # print(t_D)
            
            if (!is.na(t_D)) {
                
                p <- p + 
                    geom_hline(yintercept = logN0 - input$pred_tgr_logreductions, linetype = 3) +
                    geom_vline(xintercept = t_D, linetype = 3) +
                    geom_label(x = t_D, y = logN0, label = paste("Time =", round(t_D, 2)))
                
            }
            
        }
        
        p

    })
    
    #' Residual analysis
    
    pred_residuals <- reactive({
        
        my_simulation <- pred_simulation() %>%
            .$simulation %>%
            select(time, logN)
        
        my_obs <- pred_micro_data() %>%
            as.data.frame() %>%
            mutate(logN = log10(N)) %>%
            select(time, logN)

        modCost(model = my_simulation,
                obs = my_obs)
    })
    
    output$pred_residuals <- renderTable(
        pred_residuals()$residuals %>%
            select(Observed = obs, Predicted = mod, 
                   Residual = res)
    )
    
    output$pred_plot_pred <- renderPlot({
        pred_residuals()$residuals %>%
            select(Observed = obs, Predicted = mod, 
                   Residual = res) %>%
            ggplot() +
                geom_point(aes(x = Observed, y = Predicted)) +
                geom_abline(slope = 1, intercept = 0, linetype = 2)
    })
    
    output$pred_plot_res <- renderPlot({
        pred_residuals()$residuals %>%
            select(Observed = obs, Predicted = mod, 
                   Residual = res) %>%
            ggplot() +
            geom_point(aes(x = Predicted, y = Residual)) +
            geom_hline(yintercept = 0, linetype = 2)
        
    })
    
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
    
    iso_my_data <- callModule(tableFile3col, "iso_data_input",
                              default_data = data.frame(time = c(0, 6, 8, 0, 6, 10, 0, 4, 8),
                                                        logS = c(0, -1.7, -2, 0, -0.3, -1, 0, -0.7, -1.5),
                                                        temperature = c(100, 100, 100, 95, 95, 95, 97, 97, 97)
                                                        )
                              )
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
                geom_point(aes(x = time, y = res)) + # , colour = temperature)) +
                geom_hline(yintercept = 0, linetype = 2, size = 1) +
                ylab("Residual") + xlab("Time") +
                facet_wrap("temperature", scales = "free_x")
    })
    
    output$iso_residuals_hist <- renderPlot({
        
        iso_fitted_model() %>%
            .$data %>%
            mutate(res = residuals(summary(iso_fitted_model()))) %>%
            mutate(temperature = factor(temp)) %>%
            ggplot() +
                geom_histogram(aes(res, fill = temperature)) +
                geom_vline(xintercept = 0, linetype = 2, colour = "red") +
                xlab("Residual")
        
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
                                    label_2 = "temperature",
                                    default_data = data.frame(c(0, 10), c(70, 80))
                                    )
    dyna_micro_data <- callModule(tableFile, "dyna_micro_data",
                                  default_data = data.frame(c(0, 5, 7.5, 2.5, 6, 8),
                                                            c(1e6, 1e5, 15000, 800000, 30000, 1e3))
                                  )
    # dyna_model_pars <- callModule(fitPars, "dyna_model_pars")
    dyna_model_fit <- callModule(dynamicModel, "dyna_model_pars", 
                                  dyna_temp_profile = dyna_temp_profile, dyna_micro_data = dyna_micro_data)
    
    #' Plot of the guesses  (on a module now)
    
    # output$dyna_plot_guess <- renderPlot({
    #     
    #     validate({
    #         need(dyna_temp_profile(), "Define a temperature profile")
    #     })
    #     
    #     my_temperature <- as.data.frame(dyna_temp_profile()) %>% na.omit()
    #     
    #     times <- seq(0, max(my_temperature$time), length = 200)
    #     
    #     my_model <- dyna_model_pars()$model
    #     
    #     my_data <- dyna_micro_data() %>%
    #         mutate(logN = log10(N))
    #     
    #     model_pars <- list(guess = c(dyna_model_pars()$guess, dyna_model_pars()$known),
    #                        lower = c(dyna_model_pars()$lower, dyna_model_pars()$known),
    #                        upper = c(dyna_model_pars()$upper, dyna_model_pars()$known))
    #     
    #     model_pars %>%
    #         map(~ predict_inactivation(my_model, times, ., my_temperature)) %>%
    #         map(~ .$simulation) %>%
    #         imap_dfr(~ mutate(.x, which = .y)) %>%
    #         ggplot(.) +
    #             geom_line(aes(x = time, y = logN, colour = which)) +
    #             geom_point(aes(x = time, y = logN), data = my_data)
    #     
    #     })
    
    #' Model fitting
    
    # dyna_model_fit <- eventReactive(input$dyna_fit_button, {  # In the module now
    #     
    #     my_temperature <- as.data.frame(dyna_temp_profile()) %>% na.omit()
    # 
    #     my_model <- dyna_model_pars()$model
    #     
    #     my_data <- dyna_micro_data() %>%
    #         mutate(logN = log10(N))
    #     
    #     withProgress(message = "Fitting model", {
    #         
    #         if (input$dyna_algorithm == "nlr") {
    #             
    #             fit_dynamic_inactivation(my_data, my_model, my_temperature,
    #                                      dyna_model_pars()$guess, 
    #                                      dyna_model_pars()$upper, 
    #                                      dyna_model_pars()$lower,
    #                                      dyna_model_pars()$known)
    #         } else {
    #             fit_inactivation_MCMC(my_data, my_model, my_temperature,
    #                                   dyna_model_pars()$guess, 
    #                                   dyna_model_pars()$upper, 
    #                                   dyna_model_pars()$lower,
    #                                   dyna_model_pars()$known,
    #                                   niter = input$dyna_niters)
    #         }
    #     })
    # })
    
    dyna_modCost <- reactive({

        validate(need(dyna_model_fit(), message = FALSE))

        my_simulation <- dyna_model_fit() %>%
            .$best_prediction %>%
            .$simulation %>%
            select(time, logN)

        my_data <- dyna_micro_data() %>%
            as.data.frame() %>%
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
    
    output$dyna_shoulder_geeraerd <- renderUI({
        
        
        validate(need(dyna_model_fit(), message = FALSE))
        
        if (dyna_model_fit()$best_prediction$model != "Geeraerd") {
            
            return(NULL)
        } else {
            
            my_fit <- dyna_model_fit()
            
            if (is.FitInactivation(my_fit)) {
                
                kmax <- log(10)/my_fit$fit_results$par[["D_R"]]
                SL <- log(my_fit$fit_results$par["C_c0"] + 1)/kmax
                
            } else {
                
                kmax <- log(10)/my_fit$modMCMC$bestpar[["D_R"]]
                SL <- log(my_fit$modMCMC$bestpar["C_c0"] + 1)/kmax
            }

            tagList(
                wellPanel(
                    tags$p(paste("Shoulder length at reference temperature:", round(SL, 2)))
                )
            )
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
    
    output$dyna_fit_diagnostic <- renderPlot({
        
        if("FitInactivationMCMC" %in% class(dyna_model_fit())) {
            
            p1 <- dyna_model_fit()$modMCMC$pars %>%
                as.data.frame() %>%
                mutate(iter = row_number()) %>%
                gather(par, value, -iter) %>%
                # split(.$par) %>%
                # map(.,
                #     ~ mutate(., rmean = rollmeanr(value, 7, na.pad = TRUE))
                #     )
                ggplot(aes(x = iter, y = value)) +
                geom_line() +
                geom_line(aes(y = rollmeanr(value, 10, na.pad = TRUE)), colour = "grey", size = 1) +
                # geom_smooth(se = FALSE, method = "rlm") +
                facet_wrap("par", scales = "free") +
                theme_minimal() +
                ylab("")
            
            p2 <- dyna_model_fit()$modMCMC$pars %>%
                as.data.frame() %>%
                ggpairs()
            
            plot_grid(p1, ggmatrix_gtable(p2), ncol = 1)
            
        } else {
            NULL
        }
        
    })
    # 
    # output$dyna_MCMC_pairs <- renderPlot({
    # 
    #     validate(need(dyna_model_fit(), message = FALSE))
    # 
    #     if (is.FitInactivationMCMC(dyna_model_fit())) {
    #         pairs(dyna_model_fit()$modMCMC)
    # 
    #     } else {
    #         NULL
    #     }
    # })
    # 
    # output$dyna_par_cor <- renderTable({
    # 
    #     validate(need(dyna_model_fit(), message = FALSE))
    # 
    #     if (is.FitInactivation(dyna_model_fit())) {
    #         cov2cor(summary(dyna_model_fit())$cov.unscaled)
    # 
    #     } else {
    #         NULL
    #     }
    # 
    # })
    # 
    # output$dyna_MCMC_conv_plot <- renderPlot({
    # 
    #     validate(need(dyna_model_fit(), message = FALSE))
    # 
    #     if (is.FitInactivationMCMC(dyna_model_fit())) {
    #         plot(dyna_model_fit()$modMCMC)
    # 
    #     } else {
    #         NULL
    #     }
    # })
    # 
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
                                      label_2 = "temperature",
                                      default_data = data.frame(c(0, 10), c(70, 80)))
    
    interv_micro_data <- callModule(tableFile, "interv_micro_data",
                                    default_data = data.frame(c(0, 5, 7.5, 2.5, 6, 8),
                                                              c(1e6, 1e5, 15000, 800000, 30000, 1e3)
                                    )
    )
    
    
    #' Calculation
    
    interv_prediction_interval <- eventReactive(input$interv_calculate, {
        
        validate(need(dyna_model_fit(),
                      message = "A model must be fitted first."))
        
        my_temperature <- as.data.frame(interv_temp_profile()) %>% 
            filter(!is.na(temperature)) %>%
            as.data.frame()
        
        times <- seq(0, max(my_temperature$time, na.rm = TRUE), length = 50)
        
        withProgress(message = "Calculating interval", {
            
            predict_inactivation_MCMC(dyna_model_fit(), my_temperature,
                                      input$interv_niter, times,
                                      quantiles = input$interv_quantile)
        })

    }) 
    
    output$interv_out_plot <- renderPlot({
        
        validate(need(interv_prediction_interval(), message = FALSE))
        
        p <- plot(interv_prediction_interval()) +
            xlab(input$interv_xlabel) + ylab(input$interv_ylabel) +
            ylim(input$interv_ymin, input$interv_ymax)
        
        if (!is.null(interv_micro_data())) {
            p <- p + geom_point(aes(x = time, y = log10(N)), data = interv_micro_data(),
                                inherit.aes = FALSE)
        }
        
        p
            
        
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

























