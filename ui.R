library(shiny)
library(shiny.collections)
library(rhandsontable)
library(purrr)

ui = shinyUI(fluidPage(
  includeMarkdown('README.Rmd'),
  hr(),
  rHandsontableOutput("datatable")
))

