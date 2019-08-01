
body <- dashboardBody(
    tabItems(
        
        ## Prediction tab
        
        tabItem(tabName = "prediction",
                
                ## Popup message
                
                # bsModal(id = 'startupModal', title = 'Migration', trigger = '',
                #         size = 'large', 
                #         p("The tools developed by the group are being migrated to a new domain."),
                #         p("This link will soon stop working."),
                #         a("New site", href = "https://foodlab-upct.shinyapps.io/bioinactivationFE/")
                #         # p("A new version: https://foodlab-upct.shinyapps.io/bioinactivationFE/")
                #         ),
                
                ##
                
                fluidRow(
                    tableFileUI("pred_temp_input", label_2 = "temperature",
                                inputBoxTitle = "Input Temperature",
                                outputBoxTitle = "Temperature Data",
                                default_frame = data.frame(c(0, 10), c(70, 80)))
                    ),
                fluidRow(
                    tableFileUI("pred_micro_data",
                                inputBoxTitle = "Input Microbial counts",
                                outputBoxTitle = "Microbial data",
                                default_frame = data.frame(c(0, 5, 7.5, 2.5, 6, 8),
                                                           c(1e6, 1e5, 15000, 800000, 30000, 1e3)
                                                           )
                                )
                ),
                fluidRow(
                    box(title = "Model parameters", solidHeader = TRUE, collapsible = TRUE,
                        status = "primary",
                        predParsInput("pred_pars")
                    )),
                fluidRow(
                    box(title = "Prediction", status = "success", solidHeader = TRUE,
                        collapsible = TRUE,
                        actionButton("pred_calculate", "Make prediction"),
                        plotOutput("pred_simul_plot")
                    ),
                    box(title = "Edit plot", status = "success", solidHeader = TRUE,
                        collapsible = TRUE,
                        textInput("pred_xlabel", "x-label", value = "Time"),
                        textInput("pred_ylabel", "y-label", value = "logN"),
                        textInput("pred_ylabel2", "Secondary y-label", value = "Temperature"),
                        numericInput("pred_ymin", "Lower limit y-axis", value = 0),
                        numericInput("pred_ymax", "Upper limit y-axis", value = 6),
                        checkboxInput("pred_addTemp", "Temperature profile", TRUE),
                        tags$hr(),
                        checkboxInput("pred_add_time_to_X", "Include time to reach X log-reductions", FALSE),
                        numericInput("pred_tgr_logreductions", "Number of log-reductions", value = 3, min = 0)
                        )
                    
                ),
                fluidRow(
                    box(title = "Residuals", status = "warning", solidHeader = TRUE,
                        collapsible = TRUE,
                        tableOutput("pred_residuals")
                        ),
                    box(title = "Residual analysis", status = "warning", solidHeader = TRUE,
                        collapsible = TRUE,
                        tableOutput("pred_residual_statistics"),
                        tags$footer("* AIC, AICc and BIC are calculated considering that every model parameter was fitted"),
                        tags$hr(),
                        tags$h3("Shapiro-Wilk normality test of the residuals"),
                        verbatimTextOutput("pred_residuals_normality")
                        )
                ),
                fluidRow(
                    box(title = "Export results", status = "danger", solidHeader = TRUE,
                        collapsible = TRUE,
                        downloadButton("pred_down_curve", "Survivor curve"),
                        downloadButton("pred_down_residuals", "Residual table")
                        )# ,
                    # box(title = "Export as FSK", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    #     tabsetPanel(
                    #         tabPanel("General Information",
                    #                  textInput("fsk_pred_name", "Name"),
                    #                  textInput("fsk_pred_source", "Source"),
                    #                  textInput("fsk_pred_identifier", "Identifier"),
                    #                  dateInput("fsk_pred_date", "Creation Date"),
                    #                  textInput("fsk_pred_rights", "Rights", value = "Creative Commons Attribution-NonCommercial 4.0"),
                    #                  textInput("fsk_pred_language", "Language", value = "English"),
                    #                  tags$h4("Creators"),
                    #                  rHandsontableOutput("fsk_pred_creators")
                    #                  # matrixInput("fsk_pred_creators", "Email - Family name - Given name",
                    #                  #             data = data.frame(email = "someone@mail.com", f_name = "Doe", g_name = "Jon")
                    #                  #             )
                    #                  
                    #         ),
                    #         tabPanel("Scope",
                    #                  textInput("fsk_pred_productName", "Product unit"),
                    #                  textInput("fsk_pred_hazardName", "Hazard name"),
                    #                  textInput("fsk_pred_hazardDescript", "Hazard description"),
                    #                  textInput("fsk_pred_hazardType", "Hazard type")
                    #         ),
                    #         tabPanel("Data background",
                    #                  textInput("fsk_pred_title", "Study title"),
                    #                  textInput("fsk_pred_descrtiption", "Study Description")
                    #         ),
                    #         tabPanel("Model math",
                    #                  rHandsontableOutput("fsk_pred_model")
                    #                  ),
                    #         tabPanel("Export",
                    #                  textInput("fsk_pred_filename", "File name", value = "pred_from_bioinactivation.fskx"),
                    #                  downloadButton("fsk_pred_download", "Download")
                    #         )
                    #     )
                    # )
                    
                ),
                fluidRow(
                    # box(title = "Export as FSK", status = "danger", solidHeader = TRUE,
                    #     tabsetPanel(
                    #         tabPanel("General Information",
                    #                  textInput("fsk_pred_name", "Name"),
                    #                  textInput("fsk_pred_source", "Source"),
                    #                  textInput("fsk_pred_identifier", "Identifier"),
                    #                  dateInput("fsk_pred_date", "Creation Date"),
                    #                  textInput("fsk_pred_rights", "Rights", value = "Creative Commons Attribution-NonCommercial 4.0"),
                    #                  textInput("fsk_pred_language", "Language", value = "English"),
                    #                  tags$h4("Creators")
                    #         ),
                    #         tabPanel("Scope",
                    #                  textInput("fsk_pred_productName", "Product unit"),
                    #                  textInput("fsk_pred_hazardName", "Hazard name"),
                    #                  textInput("fsk_pred_hazardDescript", "Hazard description"),
                    #                  textInput("fsk_pred_hazardType", "Hazard type")
                    #         ),
                    #         tabPanel("Data background",
                    #                  textInput("fsk_pred_title", "Study title"),
                    #                  textInput("fsk_pred_descrtiption", "Study Description")
                    #         ),
                    #         tabPanel("Model math"),
                    #         tabPanel("Export",
                    #                  textInput("fsk_pred_filename", "File name", value = "pred_from_bioinactivation.fskx"),
                    #                  downloadButton("fsk_pred_download", "Download")
                    #         )
                    #     )
                    # )
                )
        ),
        
        ## Isothermal tab
        
        tabItem(tabName = "isothermal",
                
                fluidRow(
                    tableFile3colUI("iso_data_input",
                                inputBoxTitle = "Input Temperature",
                                outputBoxTitle = "Data uploaded",
                                default_frame = data.frame(time = c(0, 6, 8, 0, 6, 10, 0, 4, 8),
                                                           logS = c(0, -1.7, -2, 0, -0.3, -1, 0, -0.7, -1.5),
                                                           temperature = c(100, 100, 100, 95, 95, 95, 97, 97, 97)
                                                           )
                                )
                ),
                fluidRow(
                    box(title = "Model parameters", status = "primary",
                        collapsible = TRUE, solidHeader = TRUE,
                        isofitParsInput("iso_model_pars")
                    )
                    
                ),
                fluidRow(
                    box(title = "Fitted curve", status = "success", width = 12,
                        solidHeader = TRUE, collapsible = TRUE,
                        actionButton("iso_fit_button", "Fit model"),
                        plotOutput("iso_fitted_curve")
                        )
                ),
                fluidRow(
                    box(title = "Fitted parameters", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tableOutput("iso_pars_table"),
                        tableOutput("iso_residual_statistics")
                    ),
                    box(title = "Residual analysis", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tags$h3("Residual plot"),
                        plotOutput("iso_residual_plot"),
                        tags$hr(),
                        tags$h3("Shapiro-Wilk normality test of the residuals"),
                        verbatimTextOutput("iso_residuals_normality")
                        )
                ),
                fluidRow(
                    # box(title = "Export as FSK", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    #     tabsetPanel(
                    #         tabPanel("General Information",
                    #                  textInput("fsk_iso_name", "Name"),
                    #                  textInput("fsk_iso_source", "Source"),
                    #                  textInput("fsk_iso_identifier", "Identifier"),
                    #                  dateInput("fsk_iso_date", "Creation Date"),
                    #                  textInput("fsk_iso_rights", "Rights", value = "Creative Commons Attribution-NonCommercial 4.0"),
                    #                  textInput("fsk_iso_language", "Language", value = "English"),
                    #                  tags$h4("Creators")
                    #         ),
                    #         tabPanel("Scope",
                    #                  textInput("fsk_iso_productName", "Product unit"),
                    #                  textInput("fsk_iso_hazardName", "Hazard name"),
                    #                  textInput("fsk_iso_hazardDescript", "Hazard description"),
                    #                  textInput("fsk_iso_hazardType", "Hazard type")
                    #         ),
                    #         tabPanel("Data background",
                    #                  textInput("fsk_iso_title", "Study title"),
                    #                  textInput("fsk_iso_descrtiption", "Study Description")
                    #         ),
                    #         tabPanel("Model math",
                    #                  rHandsontableOutput("fsk_iso_model")
                    #                  ),
                    #         tabPanel("Export",
                    #                  textInput("fsk_iso_filename", "File name", value = "fit_from_bioinactivation.fskx"),
                    #                  checkboxInput("fsk_iso_data", "Include data?"),
                    #                  downloadButton("fsk_iso_download", "Download")
                    #         )
                    #     )
                    # )
                )
                ),
        
        ## Dynamic tab
        
        tabItem(tabName = "dynamic",
                
                fluidRow(
                    tableFileUI("dyna_temp_input", label_2 = "temperature",
                                inputBoxTitle = "Input Temperature",
                                outputBoxTitle = "Temperature Data",
                                default_frame = data.frame(c(0, 10), c(70, 80)))
                ),
                fluidRow(
                    tableFileUI("dyna_micro_data",
                                inputBoxTitle = "Input Microbial counts",
                                outputBoxTitle = "Microbial data",
                                default_frame = data.frame(c(0, 5, 7.5, 2.5, 6, 8),
                                                           c(1e6, 1e5, 15000, 800000, 30000, 1e3)
                                )
                    )
                ),
                fluidRow(
                    dynamicModelUI("dyna_model_pars")
                    
                ),
                # dynamicModel("dyna_model_pars"),
                # fluidRow(
                #     box(title = "Model parameters", width = 12, status = "primary",
                #         collapsible = TRUE, solidHeader = TRUE,
                #         fitParsInput("dyna_model_pars")
                #         )
                # 
                # ),
                # fluidRow(
                #     box(title = "Settings of the fitting algorithm", status = "primary",
                #         solidHeader = TRUE, collapsible = TRUE,
                #         selectInput("dyna_algorithm", "Fitting algorithm",
                #                     list(`Non-linear regression` = "nlr",
                #                          `Adaptive Monte Carlo` = "MCMC",
                #                          `Bayesian Monte Carlo` = "bayesian")
                #                     ),
                #         conditionalPanel(
                #             condition = "input.dyna_algorithm != 'nlr'",
                #             numericInput("dyna_niters", "Number of iterations",
                #                          1000, min = 0),
                #             actionButton("btn_dyna_seed", "Reset Seed")
                #         ),
                #         # conditionalPanel(
                #         #     condition = "input.dyna_algorithm == 'bayesian'",
                #         #     numericInput("dyna_niters", "Number of iterations",
                #         #                  1000, min = 0),
                #         #     actionButton("btn_dyna_seed", "Reset Seed")
                #         # ),
                #         tags$hr(),
                #         actionButton("dyna_fit_button", "Fit the model")
                #         ),
                #     box(title = "Initial guess", status = "primary",
                #         collapsible = TRUE, solidHeader = TRUE,
                #         plotOutput("dyna_plot_guess")
                #         )
                # ),
                fluidRow(
                    box(title = "Fitted curve", status = "success",
                        solidHeader = TRUE, collapsible = TRUE,
                        plotOutput("dyna_plot_fit")
                        ),
                    box(title = "Edit plot", status = "success",
                        solidHeader = TRUE, collapsible = TRUE,
                        textInput("dyna_xlabel", "x-label", value = "Time"),
                        textInput("dyna_ylabel", "y-label", value = "logN"),
                        textInput("dyna_ylabel2", "Secondary y-label", value = "Temperature"),
                        numericInput("dyna_ymin", "Lower limit y-axis", value = 0),
                        numericInput("dyna_ymax", "Upper limit y-axis", value = 6),
                        checkboxInput("dyna_addTemp", "Temperature profile", TRUE)
                        )

                ),
                fluidRow(
                    box(title = "Fitted parameters", status = "warning",
                        solidHeader = TRUE, collapsible = TRUE,
                        tableOutput("dyna_fitted_pars"),
                        tags$hr(),
                        tableOutput("dyna_residuals_statistics")
                    ),
                    box(title = "Diagnostics", status = "warning", collapsible = TRUE,
                        solidHeader = TRUE,
                        tags$h3("Residual plot"),
                        plotOutput("dyna_residuals_plot"),
                        tags$h3("Shapiro-Wilk normality test of the residuals"),
                        verbatimTextOutput("dyna_residuals_normality"),
                        plotOutput("dyna_fit_diagnostic")
                        # conditionalPanel(
                        #     condition = "input.dyna_algorithm == 'nlr'",
                        #     tags$h3("Parameter correlation"),
                        #     tableOutput("dyna_par_cor")
                        # ),
                        # conditionalPanel(
                        #     condition = "input.dyna_algorithm == 'MCMC'",
                        #     tags$h3("Pairs plot"),
                        #     plotOutput("dyna_MCMC_pairs"),
                        #     tags$h3("Convergence of the Markov chain"),
                        #     plotOutput("dyna_MCMC_conv_plot")
                        # )

                        )
                ),
                fluidRow(
                    box(title = "Export results", status = "danger",
                        solidHeader = TRUE, collapsible = TRUE,
                        downloadButton("dyna_down_curve", "Survivor curve"),
                        downloadButton("dyna_down_residuals", "Residual table")
                    )# ,
                    # box(title = "Export as FSK", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    #     tabsetPanel(
                    #         tabPanel("General Information",
                    #                  textInput("fsk_dyna_name", "Name"),
                    #                  textInput("fsk_dyna_source", "Source"),
                    #                  textInput("fsk_dyna_identifier", "Identifier"),
                    #                  dateInput("fsk_dyna_date", "Creation Date"),
                    #                  textInput("fsk_dyna_rights", "Rights", value = "Creative Commons Attribution-NonCommercial 4.0"),
                    #                  textInput("fsk_dyna_language", "Language", value = "English"),
                    #                  tags$h4("Creators")
                    #         ),
                    #         tabPanel("Scope",
                    #                  textInput("fsk_dyna_productName", "Product unit"),
                    #                  textInput("fsk_dyna_hazardName", "Hazard name"),
                    #                  textInput("fsk_dyna_hazardDescript", "Hazard description"),
                    #                  textInput("fsk_dyna_hazardType", "Hazard type")
                    #         ),
                    #         tabPanel("Data background",
                    #                  textInput("fsk_dyna_title", "Study title"),
                    #                  textInput("fsk_dyna_descrtiption", "Study Description")
                    #         ),
                    #         tabPanel("Model math"),
                    #         tabPanel("Export",
                    #                  textInput("fsk_dyna_filename", "File name", value = "pred_from_bioinactivation.fskx"),
                    #                  downloadButton("fsk_dyna_download", "Download")
                    #         )
                    #     )
                    # )
                ),
                fluidRow(
                    tableFileUI("interv_temp_input", label_2 = "temperature",
                                inputBoxTitle = "Temperature Prediction Interval",
                                outputBoxTitle = "Temperature Data",
                                default_frame = data.frame(c(0, 10), c(70, 80)))
                ),
                fluidRow(
                    box(title = "Parameters prediction interval",
                        status = "primary", solidHeader = TRUE, collapsible = TRUE,
                        sliderInput("interv_quantile", "Quantile of the response",
                                    min = 0, max = 100, value = c(5, 95)),
                        numericInput("interv_niter", "Number of MCMC iterations",
                                     1000, min = 10),
                        actionButton("btn_interv_seed", "Reset Seed"),
                        tags$hr(),
                        actionButton("interv_calculate", "Calculate interval")
                        )
                ),
                fluidRow(
                    
                    box(title = "Prediction interval", collapsible = TRUE,
                        status = "success", solidHeader = TRUE,
                        plotOutput("interv_out_plot")
                        ),
                    
                    box(title = "Edit plot", status = "success",
                        solidHeader = TRUE, collapsible = TRUE,
                        textInput("interv_xlabel", "x-label", value = "Time"),
                        textInput("interv_ylabel", "y-label", value = "logN"),
                        numericInput("interv_ymin", "Lower limit y-axis", value = 0),
                        numericInput("interv_ymax", "Upper limit y-axis", value = 6)
                        )
                )
        ),
        
        ## About tab
        
        tabItem(tabName = "about",
                tags$h3("Bioinactivation FE. Version 0.1.0"),
                tags$hr(),
                tags$p("Bioinactivation FE (full environment) has been developed as a colaboration between the departments
of Applied Mathematics and Food Microbiology of the Technical University of Cartagena and the Swedish National Food Safety
                       Agency."),
                tags$p("This application provides a user interface to the functions for modelling of microbial inactivation
                                   implemented in the bionactivation package of R (a.k.a. bioinactivation core)."),
                tags$p("A link to the latest version of this application can be found in the following
                                   webpage:"),
                tags$p("https://foodlab-upct.shinyapps.io/bioinactivationFE/"),
                tags$hr(),
                tags$p("For bug reports and support, please use one of the following e-mail accounts:"),
                tags$p("garre.alberto@gmail.com"),
                tags$p("pablo.fernandez@upct.es"),
                tags$hr(),
                tags$p("When using this application, please cite it as:"),
                tags$p("Alberto Garre, Pablo S. Fernandez, Roland Lindqvist,Jose A. Egea,
                                    Bioinactivation: Software for modelling dynamic microbial inactivation,
                                    Food Research International, Volume 93, March 2017, Pages 66-74, ISSN 0963-9969,
                                    http://dx.doi.org/10.1016/j.foodres.2017.01.012."),
                tags$p("A BibTeX entry for LaTeX users is"),
                tags$p("@Article{,
                                   author = {Alberto Garre and Pablo S. Fernandez and Roland Lindqvist and Jose A. Egea},
                                   title = {Bioinactivation: Software for modelling dynamic microbial inactivation },
                                   journal = {Food Research International },
                                   volume = {93},
                                   pages = {66 - 74},
                                   year = {2017},
                                   issn = {0963-9969},
                                   doi = {10.1016/j.foodres.2017.01.012},
                                   url = {//www.sciencedirect.com/science/article/pii/S0963996917300200},
                                   }"
                )
                ),
        
        ## Articles tab
        
        tabItem(tabName = "articles",
                tags$h3("Scientific article describing the user interface:"),
                tags$h4("Bioinactivation FE: A free web application for modelling isothermal and dynamic microbial inactivation"),
                tags$h4("Garre, A., Clemente-Carazo, M., Fernandez, P.S., Lindqvist, R., Egea, J.A."),
                tags$h4("(2018) Food Research International, 112, 353-360"),
                tags$h4("10.1016/j.foodres.2018.06.057"),
                tags$hr(),
                tags$h3("Scientific article describing the core functions:"),
                tags$h4("Bioinactivation: Software for modelling dynamic microbial inactivation"),
                tags$h4("Garre, A., Fernandez, P.S., Lindqvist, R., Egea, J.A."),
                tags$h4("(2017) Food Research International, 93, 66-74"),
                tags$h4("10.1016/j.foodres.2017.01.012") #,
                # tags$hr(),
                # tags$h3("Some of the articles that use bioinactivation:"),
                # tags$h4()
                )
    )
)
