# Load packages ----
library(shiny)
library(shinyWidgets)
library(plotly)
library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(icon)
# Source the simulation code ----
source("SIR_sim.R")
# Source the mapper function ----
source("corona_mapper.R")
# Source the daily data function ----
source("computedailydata.R")
# Compute the current date
data = read.csv("time_series_covid19_confirmed_global.csv")
date_string <- colnames(data)[dim(data)[2]]
date_split <- unlist(strsplit(date_string, "\\."))
month <- as.numeric(gsub("X", "", date_split[1]))
day <- as.numeric(date_split[2])
year <- 2000 + as.numeric(date_split[3])
max_css_date <- paste0(year, '-', month, '-', day)

# Design the header ----
header <- dashboardHeader(title = "COVID-19 spread", titleWidth = 300)

# Design the sidebar ----
sidebar <- dashboardSidebar(
  disable = TRUE
)

### Customize theme_grey_dark ---------------------------------------------------------
### Danger! DO NOT TOUCH ONLY WHEN YOU KNOW IT!!!
### I should implement it in styles.css. Leave it to future.  ----Liang 24/04/2020
theme_grey_dark <- shinyDashboardThemeDIY(
### general
  appFontFamily = "Arial"
  , appFontColor = "rgb(205,205,205)"
  , primaryFontColor = "rgb(255,255,255)"
  , infoFontColor = "rgb(255,255,255)"
  , successFontColor = "rgb(255,255,255)"
  , warningFontColor = "rgb(255,255,255)"
  , dangerFontColor = "rgb(255,255,255)"
  , bodyBackColor = "rgb(32,32,32)"

### header
  , logoBackColor = "rgb(45,45,45)"

  , headerButtonBackColor = "rgb(45,45,45)"
  , headerButtonIconColor = "rgb(255,255,255)"
  , headerButtonBackColorHover = "rgb(45,45,45)"
  , headerButtonIconColorHover = "rgb(255,0,0)"

  , headerBackColor = "rgb(45,45,45)"
  , headerBoxShadowColor = ""
  , headerBoxShadowSize = "0px 0px 0px"

### sidebar
  , sidebarBackColor = "rgb(45,45,45)"
  , sidebarPadding = 0

  , sidebarMenuBackColor = "transparent"
  , sidebarMenuPadding = 0
  , sidebarMenuBorderRadius = 0

  , sidebarShadowRadius = ""
  , sidebarShadowColor = "0px 0px 0px"

  , sidebarUserTextColor = "rgb(205,205,205)"

  , sidebarSearchBackColor = "rgb(45,55,65)"
  , sidebarSearchIconColor = "rgb(45,55,65)"
  , sidebarSearchBorderColor = "rgb(45,55,65)"

  , sidebarTabTextColor = "rgb(205,205,205)"
  , sidebarTabTextSize = 18
  , sidebarTabBorderStyle = "none"
  , sidebarTabBorderColor = "none"
  , sidebarTabBorderWidth = 0

  , sidebarTabBackColorSelected = "rgb(45,45,45)"
  , sidebarTabTextColorSelected = "rgb(205,205,205)"
  , sidebarTabRadiusSelected = "5px"

  , sidebarTabBackColorHover = "rgb(45,45,45)"
  , sidebarTabTextColorHover = "rgb(255,0,0)"
  , sidebarTabBorderStyleHover = "none"
  , sidebarTabBorderColorHover = "none"
  , sidebarTabBorderWidthHover = 0
  , sidebarTabRadiusHover = "10px"

### boxes
  , boxBackColor = "rgb(45,45,45)"
  , boxBorderRadius = 5
  , boxShadowSize = "0px 0px 0px"
  , boxShadowColor = ""
  , boxTitleSize = 16
  , boxDefaultColor = "rgb(45,45,45)"
  , boxPrimaryColor = "rgb(200,200,200)"
  , boxInfoColor = "rgb(76,0,153)"
  , boxSuccessColor = "rgb(155,240,80)"
  , boxWarningColor = "rgb(240,80,210)"
  , boxDangerColor = "rgb(240,80,80)"

  , tabBoxTabColor = "rgb(45,45,45)"
  , tabBoxTabTextSize = 18
  , tabBoxTabTextColor = "rgb(190,190,190)"
  , tabBoxTabTextColorSelected = "rgb(255,255,255)"
  , tabBoxBackColor = "rgb(45,45,45)"
  , tabBoxHighlightColor = "rgb(64,64,64)"
  , tabBoxBorderRadius = 5

### inputs
  , buttonBackColor = "rgb(230,230,230)"
  , buttonTextColor = "rgb(0,0,0)"
  , buttonBorderColor = "rgb(50,50,50)"
  , buttonBorderRadius = 5

  , buttonBackColorHover = "rgb(180,180,180)"
  , buttonTextColorHover = "rgb(50,50,50)"
  , buttonBorderColorHover = "rgb(50,50,50)"

## textbox input
  , textboxBackColor = "rgb(45,45,45)"
  , textboxBorderColor = "rgb(64,64,64)"
  , textboxBorderRadius = 5
  , textboxBackColorSelect = "rgb(64,64,64)"
  , textboxBorderColorSelect = "rgb(128,128,128)"

### tables
  , tableBackColor = "rgb(45,45,45)"
  , tableBorderColor = "rgb(70,80,90)"
  , tableBorderTopSize = 1
  , tableBorderRowSize = 1

)

# Design the body ----
body <- dashboardBody(
### changing theme
    theme_grey_dark,

    fluidRow(
      tabBox(
          title = NULL, width = 12,
# The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", height = "810px",
          tabPanel("MAP",
# Customize the control panel
            tags$head(
              includeCSS("styles.css"),
            ),

# Call the mapper function
            div(plotlyOutput("plot_map", height = 780, width = "82%"), align = "right"),

# Customize the absolute panel
            absolutePanel(id = "controls", class = "paneltab1",
                    top = 290, left = 20, width = "17%", fixed = FALSE,
                    draggable = FALSE, height = 400,
                    tags$strong(h4("Updated once daily. The data refers to: ",
                    tags$a(href = "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),

                    selectInput("scale", "Scale of the data:",
                        c("Country/Region", "Province/State")
                    ),
                    selectInput("category", "Category of the data:",
                        c("Confirmed cases" = "confirmed",
                        "Newly increase" = "new",
                        "Death" = "death")
                    ),

                    sliderInput("date",
                                label = h5("Select mapping date"),
                                min = as.Date('2020-01-23', "%Y-%m-%d"),
                                max = as.Date(max_css_date, "%Y-%m-%d"),
                                value = as.Date(max_css_date, "%Y-%m-%d"),
                                timeFormat = "%d %b",
                                width = 300)
            ),

            absolutePanel(id = "infectiondata", class = "paneltab1", top = 60, left = 20, width = "17%", fixed = FALSE, draggable = FALSE, height = 220,
                    h1(textOutput("Total_cases", inline = TRUE), align = "right", style = "color:rgb(250,0,0)"),
                    h1(textOutput("New_cases"), align = "right", style = "color:#FBE251"),
                    h1(textOutput("Death_cases"), align = "right", style = "color:#FEDFE1"),
                    h5(textOutput("Date_data"), align = "right", style = "color:rgb(205,205,205)"),

                    ),
          ),
          tabPanel("MODEL",
# Boxes need to be put in a row (or column)
            fluidRow(
              box(title = "Graph",
                status = "info",
                collapsible = TRUE,
                width = 8,
                plotlyOutput("plot", height = 400)),
              box(
                title = "Controls",
                status = "danger",
# solidHeader = TRUE,
                collapsible = TRUE,
                width = 4,
                h3("This is a toy model. A SIR model is under construction.", align = "left", style = "color:rgb(250,0,0)"),
                withMathJax(),
                helpText("Select parameters."),
                numericInput("beta", "\\(\\beta\\): the contact rate", 0.2),
                numericInput("gamma", "\\(\\gamma\\): the rate of the recovery and death", 0.2),
                numericInput("tend", "Time length", 100),
                actionButton("go", "Go"),
              ),
            )
          ),
          tabPanel("ABOUT",
            absolutePanel(id = "aboutpanel", class = "paneltab1",
                    top = 60, left = 20, width = "98%", fixed = FALSE,
                    draggable = FALSE, height = 400,
                    h3("This Shinyapp aims to develop an epidemic model to study the spread of COVID-19 across different categories, such as different ages, different works,
                    etc. The project is now under construction by ",
                    tags$a(href = "https://www.rug.nl/staff/r.s.etienne/", "Etienne group"),
                    " of ",
                    tags$a(href = "https://www.rug.nl/research/gelifes/", "Groningen Institute for Evolutionary Life Sciences.")),
                    h3(" This UI is developed and maintained by Liang Xu. More details about this UI can be found at",
                    tags$a(href = "https://xl0418.github.io", "my Github.")),
            ),
          )
      )
    ),
)


ui <- dashboardPage(header, sidebar, body)


# Server logic
server <- function(input, output) {

  # Simulating results and plots on Tab 2
  parasInput <- reactiveValues(argu = NULL)
  observeEvent(input$go, {
    parasInput$argu <- c(input$beta, input$gamma)
  })


  output$plot <- renderPlotly({
    if (is.null(parasInput$argu)) SIR_sim(tend = 100, SIR.ini = c(100, 1, 0))
    SIR_sim(tend = input$tend, SIR.ini = c(100, 1, 0), paras = parasInput$argu)
  })

  # The map plot on Tab 1
  output$plot_map <- renderPlotly({
    corona_mapper(date = as.character(input$date), mode = input$scale, cate = input$category)
  })

  # Computing the data according the date user sets
  dailyresult <- reactive({ computedailydata(date = as.character(input$date)) })
  output$Date_data <- renderText({
    paste0("Updated ", as.character(input$date))
  })
  output$Total_cases <- renderText({
    paste0(prettyNum(dailyresult()[[1]], big.mark = ","), " cases")
  })
  output$New_cases <- renderText({
    paste0(prettyNum(dailyresult()[[2]], big.mark = ","), " new")
  })
  output$Death_cases <- renderText({
    paste0(prettyNum(dailyresult()[[3]], big.mark = ","), " death")
  })
}

# Run the app
shinyApp(ui, server)
