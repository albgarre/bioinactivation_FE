
sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Predictions", tabName = "prediction", icon = icon("calculator")),
        menuItem("Isothermal fitting", tabName = "isothermal", icon = icon("thermometer-1")),
        menuItem("Non-isothermal fitting", tabName = "dynamic", icon = icon("line-chart")),
        menuItem("About", tabName = "about", icon = icon("envelope")),
        menuItem("Original article", icon = icon("trophy"),
                 href = "http://www.sciencedirect.com/science/article/pii/S0963996917300200")
    )
)
