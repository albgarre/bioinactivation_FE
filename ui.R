
## Load libraries

options(shiny.reactlog=TRUE) 

library(shiny)
library(shinydashboard)
library("shinyIncubator")
library(shinyBS)
library(rhandsontable)

library(DT)

source("tableFileUI.R")
source("predParsInput.R")
source("fitParsInput.R")
source("tableFile3colUI.R")
source("isofitParsInput.R")

## Header

source("my_header.R")

## Sidebar

source("my_sidebar.R")

## Body

source("my_body.R")

## Lauch the application

dashboardPage(header, sidebar, body)









