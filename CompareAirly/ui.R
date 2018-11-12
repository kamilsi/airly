library(shiny)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(plotly)
library(shinyalert)
library(shinyWidgets)

dashboardPage(
  dashboardHeader(
      title = "Compare air.ly",
      dropdownMenuOutput("APIstatus")
  ),
  dashboardSidebar(
      collapsed = TRUE,
      switchInput(inputId = "longrange", value = FALSE,
                  onLabel = "long", offLabel = "short")
  ),
  dashboardBody(
    fluidRow(
      useShinyalert(),
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
