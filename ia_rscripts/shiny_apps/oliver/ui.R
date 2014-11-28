# ui.R
shinyUI(fluidPage(theme = "bootstrap.css",
  fluidRow(
    column(4, 
      wellPanel(
      selectInput("var",
          label = "Choose a tracer to display",
          choices = var_names
          ),
        sliderInput("layer", 
          label = "Choose a layer to display",
          min = 1, 
          max = max_layers, 
          value = 1,
          format = "#"  # prevents decimal
          ),
        sliderInput("time",
          label = "Choose a time to display",
          min = 1, 
          max = max_time, 
          value = 1,
          format = "#"  # prevents decimal
          )
        )
      ),
      column(8,
        plotOutput("map")
        )
      ),
  fluidRow(
    column(4,
      wellPanel(
        selectInput("rel_var",
          label = "Choose a relative biomass to display",
          choices = rel_names
          )
        )
      ),
    column(4,
      wellPanel(
        selectInput("ssb_var",
          label = "Choose a SSB biomass to display",
          choices = ssb_names
          )
        )
      ),
    column(4,
      wellPanel(
        selectInput("yoy_var",
          label = "Chose a YOY biomass to display",
          choices = yoy_names
          )
        )
      ),
  fluidRow(
    column(4,
      plotOutput("rel_map", height = "300px")),
    column(4,
      plotOutput("ssb_map", height = "300px")),
    column(4,
      plotOutput("yoy_map", height = "300px"))
    )
  )
)
)
