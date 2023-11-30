  box::use(shiny.blueprint[JS, Switch, Card, Text],
           shiny[NS, moduleServer, uiOutput, renderUI, div],
           plotly[plot_ly, group_by, layout, config],
           reshape2[melt])
  
  #' Initialize the Shiny module UI
  #'
  #' @param id The Shiny module ID
  #'
  #' @export
  ui <- function(id) {
    ns <- NS(id)
    Card(
      class = "bp4-card bp4-elevation-3",
      uiOutput(ns("plot")),
      Switch(
        onChange = setInput(ns("legend"), ".target.checked"),
        defaultChecked = TRUE,
        label = "Show Legend"
      )
    )
  }
  
  #' Initialize the Shiny module server
  #'
  #' @param id The Shiny module ID
  #' @param country_data The input data for the country
  #' @param parameter The selected parameter for plotting
  #' @param title The title of the plot
  #' @param dtick The delta tick for the x-axis
  #'
  #' @export
  server <- function(id, country_data, parameter, title, dtick) {
    moduleServer(id, function(input, output, session) {
      output$plot <- renderUI({
        dw_data <- country_data[country_data$Series.Name %in% parameter, ]
        dw_data <- dw_data[, -c(1, 2)]
        
        long_dw_data <- melt(dw_data, id = c("Series.Name"))
        long_dw_data[["value"]] <- long_dw_data[["value"]] |> round(2)
        
        long_dw_data$variable <- sub(pattern = "X", replacement = "", x = long_dw_data$variable)
        
        if (nrow(long_dw_data) == 0 | all(is.na(long_dw_data[["value"]])) == TRUE)
          div(class = "no-data-container", Text(class = "no-data", "No data available"))
        else {
          xaxis <- list(
            title = "",
            showgrid = FALSE,
            showline = TRUE,
            dtick = dtick,
            zeroline = FALSE,
            showticklabels = TRUE,
            fixedrange = T,
            tickfont = list(
              family = 'Arial',
              size = 12,
              color = 'rgb(82, 82, 82)'
            )
          )
          
          yaxis <- list(
            title = "",
            showline = FALSE,
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = TRUE,
            fixedrange = T,
            tickfont = list(
              family = 'Arial',
              size = 12,
              color = 'rgb(82, 82, 82)'
            )
          )
          
          mycolors <- c("#54B435", "#B0578D", "#337CCF", "#FF6969")
          n <- long_dw_data[["Series.Name"]] |> unique() |> length()
          mycolors <- mycolors[1:n]
          
          long_dw_data |> group_by(Series.Name) |>
            plot_ly(
              colors = mycolors,
              x =  ~ variable,
              y =  ~ value,
              split =  ~ Series.Name,
              type = "scatter",
              color =  ~ Series.Name,
              line = list(width = 3),
              mode = "markers+lines",
              hovertemplate = paste(
                "Year: %{x}",
                "<br>",
                "Value: %{y} ",
                ifelse(
                  long_dw_data[["Series.Name"]] == "Life expectancy at birth, total (years)",
                  " years",
                  "%"
                ),
                sep = ""
              )
            ) |> layout(
              title = list(text = title),
              xaxis = xaxis,
              yaxis = yaxis,
              legend = list(orientation = "h"),
              showlegend = ifelse(is.null(input$legend), TRUE, input$legend)
            ) |> config(displayModeBar = F)
        }
      }) 
    })
  }
  
  #' Set input value for Shiny module
  #'
  #' @param inputId The input ID
  #' @param accessor The accessor for the input value
  #'
  #' @return JavaScript code for setting Shiny input value
  #'
  #' @export
  setInput <- function(inputId, accessor = NULL) {
    JS(paste0("x => Shiny.setInputValue('", inputId, "', x", accessor, ")"))
  }
  