box::use(
  shiny.blueprint[JS, Card, HTMLSelect, Button, HTMLSelect.shinyInput, Text, Divider],
  shiny[req, NS, uiOutput, moduleServer, renderUI, reactive, reactiveVal, observe, div, bindCache],
  RColorBrewer,
  htmltools[tags, HTML, h3, h2],
  leaflet[colorBin, leaflet, leafletOptions, addTiles, setView, addProviderTiles, addPolygons, labelOptions, highlightOptions, addLegend],
  rnaturalearth[ne_countries],
  reshape[cast],
  imola[gridPanel]
)

box::use(app/logic/datasets[dashboard_intro, dataset_info])

choices = c("Asia",
            "Africa",
            "Europe",
            "Oceania",
            "North America",
            "South America",
            "Antarctica")

setInput <- function(inputId, accessor = NULL) {
  JS(paste0("x => Shiny.setInputValue('", inputId, "', x", accessor, ")"))
}

#' Initialize the Shiny module UI for the World Health Dashboard Intro
#'
#' @param id The Shiny module ID
#'
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "homepage",
    div(
      class = "sidebar-control bp4-card bp4-elevation-3",
      div(
        class = "sidebar-content",
        h2("About"),
        h3(dashboard_intro),  # <-- Added Roxygen2 tag for the dataset intro
        Divider(),
        div(
          h2("Dataset Info"),
          h3(dataset_info),  # <-- Added Roxygen2 tag for the dataset info
          tags$a(
            class = "world-bank-link",
            style = "text-decoration:none",
            Button(class = "link", "World Bank",
                   icon = "link"),
            href = "https://databank.worldbank.org/source/world-development-indicators",
            target = "_blank"
          )),
        Divider(),
        div(
          class = "year-continent",
          div(
            class = "year-control",
            h3("Select year"),
            HTMLSelect.shinyInput(
              class = "bp4-large",
              inputId = ns("year"),
              options = 1998:2022,
              value = 2021
            )),
          div(
            class = "continent-control",
            h3("Select continent"),
            HTMLSelect(
              class = "bp4-large",
              onChange = setInput(ns("continent"), ".target.value"),
              options = choices
            ))),
        
        h3("Select parameter"),
        uiOutput(ns("input_parameter"))
      )),
    div(class = "main-content", Card(class = "bp4-elevation-3", uiOutput(ns(
      "map"
    ))))
  )
}

#' Initialize the Shiny module server for the World Health Dashboard Intro
#'
#' @param id The Shiny module ID
#' @param data The input data for the module
#' @param continent_location The location data for continents
#'
#' @export
server <- function(id, data, continent_location) {
  moduleServer(id, function(input, output, session) {
    parameter <- unique(data$Series.Name)
    
    output$input_parameter <- renderUI({
      ns <- session$ns
      HTMLSelect.shinyInput(
        class = "bp4-large",
        inputId = ns("parameter"),
        options = parameter[1:4],
        value = parameter[2]
      )
    })
    
    continent_filtered <- reactive({
      continent_name <-
        ifelse(is.null(input$continent), "Asia", input$continent)
      continent_location <-
        continent_location[continent_location$REGION == continent_name, ]
      continent_sf <-
        ne_countries(scale = "medium",
                     continent = continent_name,
                     returnclass = "sf")
      
      req(input$parameter)
      field <- input$parameter
      
      year_data = paste0("X", input$year)
      
      continent_data <- subset(data, continent == continent_name)
      
      continent_data <-
        continent_data[continent_data[["Series.Name"]] %in% c(field, parameter[5:8]), c(year_data, "Series.Name", "name_long")]
      
      continent_data <-
        data.frame(
          name_long = continent_data$name_long,
          variable = continent_data$Series.Name,
          value = continent_data[[year_data]]
        )
      
      continent_data <-
        cast(continent_data, name_long ~ variable, mean)
      
      continent_data <-
        merge(continent_sf, continent_data, by = "name_long")
      
      continent_data <-
        continent_data[!is.na(continent_data[[field]]), ]
      
      continent_data[[field]] <-
        continent_data[[field]] |> as.numeric()
      
      list(continent_data = continent_data,
           continent_location = continent_location)
    })
    
    output$map <- renderUI({
      continent_data <- continent_filtered()$continent_data
      field <-
        ifelse(is.null(input$parameter), parameter[2], input$parameter)
      
      if (nrow(continent_data) == 0) {
        div(class = "no-data-container", Text(class = "no-data", "No data available"))
      }
      else{
        my_bins <-
          seq(floor(min(continent_data[[field]])), ceiling(max(continent_data[[field]])), length.out = 9) |> round(2)
        
        palette_set <-
          ifelse(field == "Life expectancy at birth, total (years)",
                 "YlGn",
                 "YlOrRd")
        
        mypalette <-
          colorBin(
            palette = palette_set,
            domain = continent_data[[field]],
            na.color = "transparent",
            bins = my_bins
          )
        
        
        # Prepare the text for tooltips:
        mytext <- paste(
          "Country: ",
          continent_data[["name_long"]],
          "<br/>",
          paste(field, ": "),
          round(continent_data[[field]], 2),
          ifelse(
            field == "Life expectancy at birth, total (years)",
            " years",
            " %"
          ),
          "<br/>",
          "Percentage of population",
          "<ul>",
          "<li>",
          "using at least basic drinking water services: ",
          ifelse(
            is.na(continent_data[["People using at least basic drinking water services (% of population)"]]),
            "No data",
            paste(continent_data[["People using at least basic drinking water services (% of population)"]] |> round(2), "%")
          ) ,
          "</li>",
          "<li>",
          "using at least basic sanitation services: ",
          ifelse(
            is.na(continent_data[["People using at least basic sanitation services (% of population)"]]),
            "No data",
            paste(continent_data[["People using at least basic sanitation services (% of population)"]] |> round(2), "%")
          ),
          "</li>",
          "<li>",
          "with basic handwashing facilities including soap and water: ",
          ifelse(
            is.na(continent_data[["People with basic handwashing facilities including soap and water (% of population)"]]),
            "No data",
            paste(continent_data[["People with basic handwashing facilities including soap and water (% of population)"]] |> round(2), "%")
          ),
          "</li>",
          "<li>",
          "practicing open defecation: ",
          ifelse(
            is.na(continent_data[["People practicing open defecation (% of population)"]]),
            "No data",
            paste(continent_data[["People practicing open defecation (% of population)"]] |> round(2), "%")
          ),
          "</li>",
          "</ul>",
          sep = ""
        ) |>
          lapply(HTML)
        
        # Create the leaflet map and add country boundaries
        
        map <- leaflet(
          height = "800px",
          continent_data,
          options = leafletOptions(
            attributionControl = F,
            zoomControl = FALSE,
            minZoom = 3
          )
        ) |>
          addTiles()  |>
          setView(
            lat = continent_filtered()$continent_location[["LAT"]],
            lng = continent_filtered()$continent_location[["LON"]],
            zoom = 3
          ) |>
          addProviderTiles("Stadia.AlidadeSmoothDark") |>
          addPolygons(
            fillColor =  ~ mypalette(continent_data[[field]]),
            stroke = TRUE,
            fillOpacity = 0.6,
            color = "white",
            weight = 1,
            label = mytext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            ),
            highlightOptions = highlightOptions(fillOpacity = 0.8)
          )
        
        map |> addLegend(
          title = paste(
            "Continent:",
            ifelse(is.null(input$continent), "Asia", input$continent),
            "<br/>",
            "Year:",
            input$year,
            "<br/>",
            field
          ),
          pal = mypalette,
          values =  ~ continent_data[[field]],
          opacity = 0.9,
          position = "topright"
        )
      }
    }) |> bindCache(input$parameter, input$continent, input$year)
  })
}
