library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(DT)
library(tidyverse)
library(plotly)

pallette <- c("red", "green", "blue", "orange", "purple")

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
    response <- GET("https://airapi.airly.eu/v2/installations/nearest",
      accept_json(),
      add_headers(apikey = key),
      query = list(
        lat = clat,
        lng = clng,
        maxResults = 5
      )
    )
    headers <-
      response %>%
      headers()
    limits <- list(
      dmax = as.integer(headers$`x-ratelimit-limit-day`),
      dremain = as.integer(headers$`x-ratelimit-remaining-day`),
      mmax = as.integer(headers$`x-ratelimit-limit-minute`),
      mremain = as.integer(headers$`x-ratelimit-remaining-minute`)
    )
    limits$dpcent <- limits$dremain / limits$dmax * 100
    limits$mpcent <- limits$mremain / limits$mmax * 100
    output$APIstatus <- renderMenu(
      dropdownMenu(
        type = "tasks",
        badgeStatus = case_when(
          limits$dpcent > 50 & limits$mpcent > 75 ~ "success",
          limits$dpcent > 25 & limits$mpcent > 25 ~ "warning",
          limits$dpcent <= 25 | limits$mpcent <= 25 ~ "danger"
        ),
        taskItem(
          value = limits$dpcent,
          color = case_when(
            limits$dpcent > 75 ~ "green",
            limits$dpcent > 25 ~ "yellow",
            limits$dpcent <= 25 ~ "red"
          ),
          "Daily air.ly API limit"
        ),
        taskItem(
          value = limits$mpcent,
          color = case_when(
            limits$mpcent > 75 ~ "green",
            limits$mpcent > 25 ~ "yellow",
            limits$mpcent <= 25 ~ "red"
          ),
          "Minute air.ly API limit"
        )
      )
    )
    nearest <-
      response %>%
      content("text") %>%
      fromJSON()
    nearest$location$color <- pallette[1:nrow(nearest$location)]
    nearest$location$label <- paste("ID", nearest$id,
      nearest$address$street,
      nearest$address$number,
      sep = " "
    )

    # on click zoom in to area and add information about air stations
    if (!is.null(dim(nearest))) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addAwesomeMarkers(
          lat = ~latitude,
          lng = ~longitude,
          label = ~label,
          data = nearest$location,
          icon = awesomeIcons(
            icon = "glyphicon-chevron-down",
            markerColor = nearest$location$color
          )
        ) %>%
        fitBounds(
          min(nearest$location$longitude),
          min(nearest$location$latitude),
          max(nearest$location$longitude),
          max(nearest$location$latitude)
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
            if (any(sapply(x$history$values, length) > 0)) {
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
            if (any(sapply(x$history$indexes, length) > 0)) {
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
              select(id, fromDateTime, AIRLY_CAQI),
            by = c("id", "fromDateTime")
          ) %>%
          left_join(tibble(
            id = as.character(nearest$id),
            color = nearest$location$color
          ),
          by = "id"
          )

        colormap <-
            data %>% 
            count(id, color)

        plot <-
          data %>%
          gather("measure", "value", -fromDateTime, -color, -id) %>%
          ggplot(aes(x = value, fill = id)) +
          geom_density(alpha = 0.2) +
          facet_wrap(~measure, scales = "free") +
          scale_fill_manual(
            name = "ID",
            values = setNames(colormap$color, colormap$id),
            labels = as.character(colormap$id)
          )

        ggplotly(plot)
      })
    }
  })
})
