library(shiny)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(
      title = "Compare air.ly",
      dropdownMenuOutput("APIstatus")
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(
        width = 4,
        box(
          title = "Map", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = NULL,
          leafletOutput("map", width = "100%", height = "400")
        )
      ),
      column(
        width = 8,
        box(
          title = "Plots", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = NULL,
          plotlyOutput("distplot")
        )
      )
    )
  )
)
