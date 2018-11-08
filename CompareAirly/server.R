library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(DT)
library(tidyverse)
library(plotly)

my_secrets <- function() {
  path <- here::here("secret.json")
  if (!file.exists(path)) {
    stop("Can't find secret file: '", path, "'")
  }

  jsonlite::read_json(path)
}

key <- my_secrets() %>% unlist()

shinyServer(function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 19, lat = 52, zoom = 6)
  })

  observeEvent(input$map_click, {
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng
    # query airly about nearest stations
    nearest <- GET("https://airapi.airly.eu/v2/installations/nearest",
      accept_json(),
      add_headers(apikey = key),
      query = list(
        lat = clat,
        lng = clng,
        maxResults = 5
      )
    ) %>%
      content("text") %>%
      fromJSON()

    # on click zoom in to area and add information about air stations
    if (!is.null(dim(nearest))) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(
          lat = ~latitude,
          lng = ~longitude,
          data = nearest$location
        ) %>%
        fitBounds(
          min(nearest$location$longitude),
          min(nearest$location$latitude),
          max(nearest$location$longitude),
          max(nearest$location$latitude)
        )

      output$nearest <- renderDataTable(
        bind_cols(
          id = nearest$id,
          nearest$address %>%
            select(-c(country, displayAddress1, displayAddress2)),
          elevation = nearest$elevation
        ),
        options = list(pageLength = 5, dom = "t")
      )

      output$distplot <- renderPlotly({
        measurment <-
          lapply(nearest$id, function(x) {
            GET("https://airapi.airly.eu/v2/measurements/installation",
              accept_json(),
              add_headers(apikey = key),
              query = list(installationId = x)
            ) %>%
              content("text") %>%
              fromJSON()
          })

        names(measurment) <- nearest$id

        values <-
          measurment %>%
          lapply(function(x) {
            if(any(sapply(x$history$values, length)>0)){
                x$history %>%
                    select(fromDateTime, values) %>%
                    unnest() %>%
                    bind_rows() %>%
                    spread(name, value)
            } else {
                NULL
            }
          }) %>%
          bind_rows(.id = "id")
        
        indexes <-
            measurment %>%
            lapply(function(x) {
                if(any(sapply(x$history$indexes, length)>0)){
                    x$history %>%
                        select(fromDateTime, indexes) %>%
                        unnest() %>%
                        bind_rows() %>%
                        spread(name, value)
                } else {
                    NULL
                }
            }) %>%
            bind_rows(.id = "id")
        
        data <-
            values %>% 
            left_join(
                indexes %>% 
                    select(id, fromDateTime, AIRLY_CAQI)
            )

        plot <-
          data %>%
          gather("measure", "value", -fromDateTime, -id) %>%
          ggplot(aes(x = value, fill = id)) +
          geom_density(aes(alpha = 0.2)) +
          facet_wrap(~measure, scales = "free")
        
        ggplotly(plot)
      })
    }
  })
})
