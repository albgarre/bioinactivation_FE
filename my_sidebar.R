
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Predictions", tabName = "prediction", icon = icon("calculator")),
        menuItem("Isothermal fitting", tabName = "isothermal", icon = icon("thermometer-1")),
        menuItem("Non-isothermal fitting", tabName = "dynamic", icon = icon("line-chart")),
        menuItem("About", tabName = "about", icon = icon("envelope")),
        menuItem("Scientific articles", icon = icon("trophy"),tabName = "articles"),
        menuItem("Github page", icon = icon("github"),
                 href = "https://github.com/albgarre/bioinactivation_FE")
    )
)
