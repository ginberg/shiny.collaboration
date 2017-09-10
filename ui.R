library(shiny)
library(shiny.collections)
library(rhandsontable)
library(purrr)
library(dplyr)

ui = shinyUI(fluidPage(
  includeMarkdown('README.Rmd'),
  hr(),
  rHandsontableOutput("datatable")
))

