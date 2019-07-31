
dynamicModelUI <- function(id, withN0 = TRUE) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(title = "Model parameters", width = 12, status = "primary",
          collapsible = TRUE, solidHeader = TRUE,
          selectInput(ns("inactv_model"),
                      "Inactivation model",
                      list(Bigelow = "Bigelow",
                           Peleg = "Peleg",
                           Mafart = "Mafart",
                           Geeraerd = "Geeraerd",
                           Arrhenius = "Arrhenius")
          ),
          
          ## Bigelow model
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Bigelow'", ns("inactv_model")),
            wellPanel( 
              
              tags$h4("D-value at ref.temp"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_bigelow_D"), "Initial guess",
                                    value = 10, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_bigelow_D"), "Lower bound",
                                    value = 7, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_bigelow_D"), "Upper bound",
                                    value = 20, min = 0)
                ),
                column(3,
                       checkboxInput(ns("bigelow_D_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("z-value"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_bigelow_z"), "Initial guess",
                                    value = 10, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_bigelow_z"), "Lower bound",
                                    value = 7, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_bigelow_z"), "Upper bound",
                                    value = 20, min = 0)
                ),
                column(3,
                       checkboxInput(ns("bigelow_z_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Reference temperature"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_bigelow_reftemp"), "Initial guess",
                                    value = 70, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_bigelow_reftemp"), "Lower bound",
                                    value = 60, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_bigelow_reftemp"), "Upper bound",
                                    value = 90, min = 0)
                ),
                column(3,
                       checkboxInput(ns("bigelow_reftemp_fix"), "Fixed?", TRUE)
                )
              ),
              
              if (withN0) {
                tagList(
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("guess_bigelow_N0"), "Initial guess",
                                        value = 6, max = 20)
                    ),
                    column(3,
                           numericInput(ns("lower_bigelow_N0"), "Lower bound",
                                        value = 5, max = 20)
                    ),
                    column(3,
                           numericInput(ns("upper_bigelow_N0"), "Upper bound",
                                        value = 7, max = 20)
                    ),
                    column(3,
                           checkboxInput(ns("bigelow_N0_fix"), "Fixed?", TRUE)
                    )
                  )
                )
              }
            )
          ),
          
          ## Peleg model
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Peleg'", ns("inactv_model")),
            wellPanel( 
              
              tags$h4("k"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_peleg_k"), "Initial guess",
                                    value = 1, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_peleg_k"), "Lower bound",
                                    value = 0.1, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_peleg_k"), "Upper bound",
                                    value = 5, min = 0)
                ),
                column(3,
                       checkboxInput(ns("peleg_k_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Critical temperature"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_peleg_Tcrit"), "Initial guess",
                                    value = 75, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_peleg_Tcrit"), "Lower bound",
                                    value = 70, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_peleg_Tcrit"), "Upper bound",
                                    value = 80, min = 0)
                ),
                column(3,
                       checkboxInput(ns("peleg_Tcrit_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("n (shape factor)"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_peleg_n"), "Initial guess",
                                    value = 1.5, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_peleg_n"), "Lower bound",
                                    value = 1, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_peleg_n"), "Upper bound",
                                    value = 2, min = 0)
                ),
                column(3,
                       checkboxInput(ns("peleg_n_fix"), "Fixed?", FALSE)
                )
              ),
              
              if (withN0) {
                tagList(
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("guess_peleg_N0"), "Initial guess",
                                        value = 6, max = 20)
                    ),
                    column(3,
                           numericInput(ns("lower_peleg_N0"), "Lower bound",
                                        value = 5, max = 20)
                    ),
                    column(3,
                           numericInput(ns("upper_peleg_N0"), "Upper bound",
                                        value = 7, max = 20)
                    ),
                    column(3,
                           checkboxInput(ns("peleg_N0_fix"), "Fixed?", TRUE)
                    )
                  )
                )
              }
            )
          ),
          
          ## Mafart model
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Mafart'", ns("inactv_model")),
            wellPanel( 
              
              tags$h4("delta-value at ref. temp"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_mafart_delta"), "Initial guess",
                                    value = 10, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_mafart_delta"), "Lower bound",
                                    value = 7, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_mafart_delta"), "Upper bound",
                                    value = 20, min = 0)
                ),
                column(3,
                       checkboxInput(ns("mafart_delta_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("z-value at ref. temp"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_mafart_z"), "Initial guess",
                                    value = 10, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_mafart_z"), "Lower bound",
                                    value = 7, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_mafart_z"), "Upper bound",
                                    value = 20, min = 0)
                ),
                column(3,
                       checkboxInput(ns("mafart_z_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Reference temperature"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_mafart_reftemp"), "Initial guess",
                                    value = 70, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_mafart_reftemp"), "Lower bound",
                                    value = 60, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_mafart_reftemp"), "Upper bound",
                                    value = 80, min = 0)
                ),
                column(3,
                       checkboxInput(ns("mafart_reftemp_fix"), "Fixed?", TRUE)
                )
              ),
              
              tags$h4("p (shape parameter)"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_mafart_p"), "Initial guess",
                                    value = 0.9, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_mafart_p"), "Lower bound",
                                    value = 0.5, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_mafart_p"), "Upper bound",
                                    value = 1, min = 0)
                ),
                column(3,
                       checkboxInput(ns("mafart_p_fix"), "Fixed?", FALSE)
                )
              ),
              
              if (withN0) {
                tagList(
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("guess_mafart_N0"), "Initial guess",
                                        value = 6, max = 20)
                    ),
                    column(3,
                           numericInput(ns("lower_mafart_N0"), "Lower bound",
                                        value = 5, max = 20)
                    ),
                    column(3,
                           numericInput(ns("upper_mafart_N0"), "Upper bound",
                                        value = 7, max = 20)
                    ),
                    column(3,
                           checkboxInput(ns("mafart_N0_fix"), "Fixed?", TRUE)
                    )
                  )
                )
              }
            )
          ),
          
          ## Geeraerd model
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Geeraerd'", ns("inactv_model")),
            wellPanel(
              
              tags$h4("D-value at ref. temp"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_geeraerd_D"), "Initial guess",
                                    value = 5, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_geeraerd_D"), "Lower bound",
                                    value = 1, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_geeraerd_D"), "Upper bound",
                                    value = 10, min = 0)
                ),
                column(3,
                       checkboxInput(ns("geeraerd_D_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("z-value at ref. temp"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_geeraerd_z"), "Initial guess",
                                    value = 10, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_geeraerd_z"), "Lower bound",
                                    value = 1, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_geeraerd_z"), "Upper bound",
                                    value = 20, min = 0)
                ),
                column(3,
                       checkboxInput(ns("geeraerd_z_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Reference temperature"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_geeraerd_reftemp"), "Initial guess",
                                    value = 70, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_geeraerd_reftemp"), "Lower bound",
                                    value = 60, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_geeraerd_reftemp"), "Upper bound",
                                    value = 80, min = 0)
                ),
                column(3,
                       checkboxInput(ns("geeraerd_reftemp_fix"), "Fixed?", TRUE)
                )
              ),
              
              tags$h4("Tail height"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_geeraerd_Nmin"), "Initial guess",
                                    value = 5e3, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_geeraerd_Nmin"), "Lower bound",
                                    value = 1e3, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_geeraerd_Nmin"), "Upper bound",
                                    value = 1e4, min = 0)
                ),
                column(3,
                       checkboxInput(ns("geeraerd_Nmin_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Initial value of Cc (shoulder)"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_geeraerd_Cc0"), "Initial guess",
                                    value = 10, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_geeraerd_Cc0"), "Lower bound",
                                    value = 0, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_geeraerd_Cc0"), "Upper bound",
                                    value = 1e3, min = 0)
                ),
                column(3,
                       checkboxInput(ns("geeraerd_Cc0_fix"), "Fixed?", FALSE)
                )
              ),
              
              if (withN0) {
                tagList(
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("guess_geeraerd_N0"), "Initial guess",
                                        value = 6, max = 20)
                    ),
                    column(3,
                           numericInput(ns("lower_geeraerd_N0"), "Lower bound",
                                        value = 5, max = 20)
                    ),
                    column(3,
                           numericInput(ns("upper_geeraerd_N0"), "Upper bound",
                                        value = 7, max = 20)
                    ),
                    column(3,
                           checkboxInput(ns("geeraerd_N0_fix"), "Fixed?", TRUE)
                    )
                  )
                )
              }
            )
          ),
          ## Arrhenius model
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Arrhenius'", ns("inactv_model")),
            wellPanel( 
              
              tags$h4("Inactivation rate at ref.temp"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_arrhenius_k"), "Initial guess",
                                    value = .5, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_arrhenius_k"), "Lower bound",
                                    value = .1, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_arrhenius_k"), "Upper bound",
                                    value = 1, min = 0)
                ),
                column(3,
                       checkboxInput(ns("arrhenius_k_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Activation energy"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_arrhenius_Ea"), "Initial guess",
                                    value = 1e3, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_arrhenius_Ea"), "Lower bound",
                                    value = 1e2, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_arrhenius_Ea"), "Upper bound",
                                    value = 5e3, min = 0)
                ),
                column(3,
                       checkboxInput(ns("arrhenius_Ea_fix"), "Fixed?", FALSE)
                )
              ),
              
              tags$h4("Reference temperature"),
              fluidRow(
                column(3,
                       numericInput(ns("guess_arrhenius_reftemp"), "Initial guess",
                                    value = 70, min = 0)
                ),
                column(3,
                       numericInput(ns("lower_arrhenius_reftemp"), "Lower bound",
                                    value = 60, min = 0)
                ),
                column(3,
                       numericInput(ns("upper_arrhenius_reftemp"), "Upper bound",
                                    value = 90, min = 0)
                ),
                column(3,
                       checkboxInput(ns("arrhenius_reftemp_fix"), "Fixed?", TRUE)
                )
              ),
              
              if (withN0) {
                tagList(
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("guess_arrhenius_N0"), "Initial guess",
                                        value = 6, max = 20)
                    ),
                    column(3,
                           numericInput(ns("lower_arrhenius_N0"), "Lower bound",
                                        value = 5, max = 20)
                    ),
                    column(3,
                           numericInput(ns("upper_arrhenius_N0"), "Upper bound",
                                        value = 7, max = 20)
                    ),
                    column(3,
                           checkboxInput(ns("arrhenius_N0_fix"), "Fixed?", TRUE)
                    )
                  )
                )
              }
            )
          ) ## End Arrhenius
      )
    ),
    fluidRow(
      box(title = "Settings of the fitting algorithm", status = "primary",
          solidHeader = TRUE, collapsible = TRUE,
          selectInput(ns("dyna_algorithm"), "Fitting algorithm",
                      list(`Non-linear regression` = "nlr",
                           `Adaptive Monte Carlo` = "MCMC",
                           `Bayesian Monte Carlo` = "bayesian")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'nlr'", ns("dyna_algorithm")),
            numericInput(ns("dyna_niters"), "Number of iterations",
                         1000, min = 0),
            actionButton(ns("btn_dyna_seed"), "Reset Seed")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bayesian'", ns("dyna_algorithm")),
            tags$hr(),
            tags$h3("Prior distributions"),
            
            conditionalPanel(  # Bigelow model
              condition = sprintf("input['%s'] == 'Bigelow'", ns("inactv_model")),
              wellPanel( 
                
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("bigelow_D_fix")),
                  tags$h4("D-value at ref.temp"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_Bigelow_D"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_bigelow_D"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("bigelow_z_fix")),
                  tags$h4("z-value"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_Bigelow_z"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_bigelow_z"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("bigelow_N0_fix")),
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_Bigelow_N0"), "Mean",
                                        value = 6, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_bigelow_N0"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                )
                
              )
            ),  # End Bigelow
            conditionalPanel(  # Mafart model
              condition = sprintf("input['%s'] == 'Mafart'", ns("inactv_model")),
              wellPanel( 
                
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("mafart_delta_fix")),
                  tags$h4("delta-value at ref.temp"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_Mafart_delta"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_Mafart_delta"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("mafart_z_fix")),
                  tags$h4("z-value"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_mafart_z"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_mafart_z"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("mafart_p_fix")),
                  tags$h4("p"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_mafart_p"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_mafart_p"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("mafart_N0_fix")),
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_mafart_N0"), "Mean",
                                        value = 6, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_mafart_N0"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                )
                
              )
            ),  # End Mafart
            conditionalPanel(  # Peleg model
              condition = sprintf("input['%s'] == 'Peleg'", ns("inactv_model")),
              wellPanel( 
                
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("peleg_k_fix")),
                  tags$h4("k"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_peleg_k"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_peleg_k"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("peleg_n_fix")),
                  tags$h4("n"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_peleg_n"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_peleg_z"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("peleg_Tcrit_fix")),
                  tags$h4("Critical temperature"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_peleg_Tcrit"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_peleg_Tcrit"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("peleg_N0_fix")),
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_peleg_N0"), "Mean",
                                        value = 6, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_peleg_N0"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                )
                
              )
            ),  # End Peleg
            conditionalPanel(  # Geerared model
              condition = sprintf("input['%s'] == 'Geeraerd'", ns("inactv_model")),
              wellPanel( 
                
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("geeraerd_D_fix")),
                  tags$h4("D-value at ref. temp."),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_geeraerd_D"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_geeraerd_D"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("geeraerd_z_fix")),
                  tags$h4("z-value"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_geeraerd_z"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_geeraerd_z"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("geeraerd_Nmin_fix")),
                  tags$h4("Tail height"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_geeraerd_Nmin"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_geeraerd_Nmin"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("geeraerd_Cc0_fix")),
                  tags$h4("Initial value of Cc"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_geeraerd_Cc0"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_geeraerd_Cc0"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("geeraerd_N0_fix")),
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_geeraerd_N0"), "Mean",
                                        value = 6, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_geeraerd_N0"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                )
                
              )
            ),  # End Geeraerd
            conditionalPanel(  # Arrhenius model
              condition = sprintf("input['%s'] == 'Arrhenius'", ns("inactv_model")),
              wellPanel( 
                
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("arrhenius_k_fix")),
                  tags$h4("Inactivation rate at ref.temp"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_arrhenius_k"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_arrhenius_k"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("arrhenius_Ea_fix")),
                  tags$h4("Activation energy"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_arrhenius_Ea"), "Mean",
                                        value = 10, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_arrhenius_Ea"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                ),
                conditionalPanel(
                  condition = sprintf("!input['%s']", ns("arrhenius_N0_fix")),
                  tags$h4("Decimal logarithm of N0"),
                  fluidRow(
                    column(3,
                           numericInput(ns("mean_prior_arrhenius_N0"), "Mean",
                                        value = 6, min = 0)
                    ),
                    column(3,
                           numericInput(ns("sd_prior_arrhenius_N0"), "sigma",
                                        value = 1, min = 0)
                    )
                  )
                )
                
              )
            )  # End Arrhenius
            ),
          tags$hr(),
          actionButton(ns("dyna_fit_button"), "Fit the model")
      ),
      box(title = "Initial guess", status = "primary",
          collapsible = TRUE, solidHeader = TRUE,
          plotOutput(ns("dyna_plot_guess"))
      )
    )
  )
}





























