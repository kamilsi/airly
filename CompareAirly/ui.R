library(shiny)
library(shinythemes)
library(leaflet)
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    titlePanel("Porównaj jakość powietrza!"),

    fluidRow(
      column(
        width = 4,
        box(
          title = "Map", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, width = NULL,
          leafletOutput("map", width = "100%", height = "400")
        ),
        box(
            title = "Locations", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = NULL,
            DT::dataTableOutput("nearest")
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
